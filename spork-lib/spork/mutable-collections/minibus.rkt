#lang racket

(provide
 (struct-out route)
 (struct-out message)
 (contract-out
  [minibus<%> interface?]
  [receiver<%> interface?]
  [minibus? predicate/c]
  [receiver? predicate/c]
  [make-minibus (-> minibus?)]
  [make-unordered-minibus (-> minibus?)]
  [minibus-add-route! (-> minibus? route? void?)]
  [minibus-remove-route! (-> minibus? route? void?)]
  [minibus-handle-message! (-> minibus? any/c message? void?)]
  [minibus-run! (-> minibus? void?)]
  [minibus-stop! (-> minibus? void?)]
  [minibus-running? (-> minibus? boolean?)]
  [minibus-stopping? (-> minibus? boolean?)]
  [minibus-queueing? (-> minibus? boolean?)]))

(require
 spork/optional spork/delegation spork/mutable-collections/runner
 spork/mutable-collections/queue)

(module phase-invariants racket
  (provide
   (all-defined-out)
   (all-from-out racket))
  (require spork/mutable-collections/runner)
  (define receiver<%>
    (interface ()
      [on-message (->m any/c any/c void?)]))

  (struct route
    (emitter receiver)
    #:transparent)

  (struct message
    (tag data)
    #:transparent)

  (define minibus<%>
    (interface (runnable<%>)
      [add-route (->m route? void?)]
      [remove-route (->m route? void?)]
      [handle-message (->m any/c message? void?)]
      [queueing? (->m boolean?)])))

(require
 (submod "." phase-invariants)
 (for-syntax (submod "." phase-invariants)))

(define (receiver? v)
  (is-a? v receiver<%>))

(define (minibus? v)
  (is-a? v minibus<%>))

(struct status
  (running stopping))

;; The receiver-queue% class is implemented to maintain the order
;; of messages sent to a receiver while simultaneously guarding
;; against slow or even deadlocked receivers.
;;
;; Rather than directly delivering messages to a receiver, the
;; bus delivers messages to a queue that is specific the the
;; receiver.  The bus then runs the receiver queue, which will
;; execute in a separate thread until the queue is empty.
;;
;; Note that this mechanism does not guarantee the order of
;; dilivery of messages to distinct  receivers, just to an
;; individual receiver.
(define receiver-queue%
  (class abstract-runnable%
    (super-new)
    (inherit stop)
    (init-field receiver)
    (define queue (make-queue))

    (define/public (on-message tag data)
      (queue-push-back! queue (message tag data)))

    (define/override (get-thunk)
      (thunk
       (match (queue-pop-front! queue)
        [(some (message tag data)) (send receiver on-message tag data)]
        [(none) (stop)])))))

(struct routing-data
  (emitter-receivers
   receiver-emitters
   receiver-queues))

(define (make-routing-data)
  (routing-data
   (make-immutable-hash '())
   (make-immutable-hash '())
   (make-immutable-hash '())))


;; The routing-data% class is a non-user-facing
;; class that manages routing data for a bus.
;; All operations are thread and future safe.
(define routing-data%
  (class object%
    (super-new)
    (define data (box (make-routing-data)))

    (define/public (add-route new-route)
      (match-let ([(route emitter receiver) new-route])
        (let loop ([current-data (unbox data)])
          (unless
              (box-cas! data current-data
                        (routing-data
                         (add-to-table (routing-data-emitter-receivers current-data) emitter receiver)
                         (add-to-table (routing-data-receiver-emitters current-data) receiver emitter)
                         (add-receiver-queue (routing-data-receiver-queues current-data) receiver)))
            (loop)))))

    (define (add-to-table table key val)
      (let ([vals (hash-ref table key '())])
        (if (member val vals) table
          (hash-set table key (append vals (list val))))))

    (define (add-receiver-queue receiver-queues receiver)
      (if (hash-has-key? receiver-queues receiver) receiver-queues
        (hash-set receiver-queues receiver (new receiver-queue% [receiver receiver]))))

    (define/public (remove-route old-route)
      (match-let ([(route emitter receiver) old-route])
        (let loop ([current-data (unbox data)])
          (let* ([new-emitter-receivers (remove-from-table (routing-data-emitter-receivers current-data) emitter receiver)]
                 [new-receiver-emitters (remove-from-table (routing-data-receiver-emitters current-data) receiver emitter)]
                 [new-receiver-queues (if (hash-has-key? new-receiver-emitters receiver) (routing-data-receiver-queues current-data)
                                        (remove-receiver-queue (routing-data-receiver-queues current-data) receiver))])
            (unless
                (box-cas! data current-data
                          (routing-data
                           new-emitter-receivers
                           new-receiver-emitters
                           new-receiver-queues))
              (loop))))))

    (define (remove-from-table table key val)
      (let ([vals (remove val (hash-ref table key '()))])
        (if (null? vals) (hash-remove table key)
          (hash-set table key vals))))

    (define (remove-receiver-queue receiver-queues receiver)
      (hash-remove receiver-queues receiver))

    (define/public (get-receivers emitter)
      (hash-ref (routing-data-emitter-receivers (unbox data)) emitter '()))

    (define/public (get-receiver-queue receiver)
      (hash-ref (routing-data-receiver-queues (unbox data)) receiver #f))))

;; The minibus% class a minibus that is robust
;; against failures caused by slow or deadlocked receivers
(define minibus%
  (class* abstract-runnable%
      (minibus<%>)
    (super-new)

    (define message-queue (make-queue))
    (define routing-data (new routing-data%))

    (define/public (add-route new-route)
      (send routing-data add-route new-route))

    (define/public (remove-route old-route)
      (send routing-data remove-route old-route))

    (define/override (get-thunk)
      (thunk
       (match (queue-pop-front! message-queue)
         [(some (cons emitter (message tag data))) (deliver-message emitter tag data)]
         [(none) (void)])))

    (define (deliver-message emitter tag data)
      (let ([receivers (send routing-data get-receivers emitter
                             )])
        (for ([receiver receivers])
          (let ([receiver-queue (send routing-data get-receiver-queue receiver)])
            (send receiver-queue on-message tag data)
            (send receiver-queue run)))))

    (define/public (handle-message emitter message)
      (queue-push-back! message-queue (cons emitter message)))

    (define/public (queueing?)
      (not (queue-empty? message-queue)))))

(define (make-minibus)
  (define minibus (new minibus%))
  ;; The minibus is wrapped in a delegator to protect
  ;; the methods that expose the internal state
  ;; of the object
  (delegator ([minibus (minibus<%>)])))

(define (make-unordered-minibus)
  (define minibus (new minibus%))
  ;; The minibus is wrapped in a delegator to protect
  ;; methods related to the objet's internal state
  ;; that would otherwise be exposed
  (delegator ([minibus (minibus<%>)])))

(define (minibus-add-route! minibus route)
  (send minibus add-route route))

(define (minibus-remove-route! minibus route)
  (send minibus remove-route route))

(define (minibus-handle-message! minibus emitter message)
  (send minibus handle-message emitter message))

(define (minibus-run! minibus)
  (send minibus run))

(define (minibus-stop! minibus)
  (send minibus stop))

(define (minibus-running? minibus)
  (send minibus running?))

(define (minibus-stopping? minibus)
  (send minibus stopping?))

(define (minibus-queueing? minibus)
  (send minibus queueing?))
