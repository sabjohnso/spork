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

(require spork/optional spork/delegation spork/mutable-collections/queue)

(module phase-invariants racket
  (provide (all-defined-out)
           (all-from-out racket))

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
    (interface ()
      [add-route (->m route? void?)]
      [remove-route (->m route? void?)]
      [handle-message (->m any/c message? void?)]
      [run (->m void?)]
      [stop (->m void?)]
      [running? (->m boolean?)]
      [stopping? (->m boolean?)]
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

(define minibus%
  (class* object%
      (minibus<%>)
    (super-new)

    (define routes (box (make-immutable-hash '())))
    (define message-queue (make-queue))
    (define bus-status (box (status #f #f)))

    ;; This method is hidden by delegation when the the
    ;; minibus is constructed with `make-minibus`
    (define/public (get-routes) routes)

    ;; This method is hidden by delegation when the the
    ;; minibus is constructed with `make-minibus`
    (define/public (get-message-queue) message-queue)

    ;; This method is hidden by delegation when the the
    ;; minibus is constructed with `make-minibus`
    (define/public (get-status) bus-status)



    (define/public (add-route new-route)
      (match-let ([(route emitter receiver) new-route])
        (let loop ([current-routes (unbox routes)])
          (let ([receivers (hash-ref current-routes emitter '())])
            (when (and (not (member receiver receivers))
                       (not (box-cas! routes current-routes
                                      (hash-set current-routes emitter
                                                (append receivers (list receiver))))))
              (loop (unbox routes)))))))

    (define/public (remove-route old-route)
      (match-let ([(route emitter receiver) old-route])
        (let loop ([current-routes (unbox routes)])
          (let ([receivers (hash-ref current-routes emitter '())])
            (when (and (member receiver receivers)
                       (not (box-cas! routes current-routes
                                      (hash-set current-routes emitter (remove receiver receivers)))))
              (loop (unbox routes)))))))

    (define/public (handle-message emitter message)
      (queue-push-back! message-queue (cons emitter message)))

    (define/public (run)
      (let ([current-bus-status (unbox bus-status)])
        (when (and (not (status-running current-bus-status))
                   (box-cas! bus-status current-bus-status
                             (status #t #f)))
          (thread
           (thunk
            (let loop ([item (queue-pop-front! message-queue)])
              (match item
                [(some (cons emitter (message tag data)))
                 (let* ([current-routes (unbox routes)]
                        [receivers (hash-ref current-routes emitter '())])
                   (for ([receiver receivers])
                     (send receiver on-message tag data)))]
                [(none) (void)])
              (when (check-continue)
                (loop (queue-pop-front! message-queue))))))))
      (void))

    ;; This method is hidden by delegation when the the
    ;; minibus is constructed with `make-minibus`
    (define/public (check-continue)
      (let loop ([current-status (unbox bus-status)])
        (if (status-stopping current-status)
            (begin (box-cas! bus-status current-status (status #f #f)) #f)
          #t)))

    (define/public (stop)
      (let loop ([current-status (unbox bus-status)])
        (when (and (status-running current-status)
                   (not (box-cas! bus-status current-status
                                  (struct-copy status current-status
                                    [stopping #t]))))
          (loop (unbox bus-status)))))

    (define/public (running?)
      (status-running (unbox bus-status)))

    (define/public (stopping?)
      (status-stopping (unbox bus-status)))

    (define/public (queueing?)
      (not (queue-empty? message-queue)))))

(define unordered-minibus%
  (class minibus%
    (super-new)
    (inherit get-status get-routes check-continue get-message-queue)
    (define cust (make-custodian))
    (define/override (run)
      (let ([current-bus-status (unbox (get-status))])
        (when (and (not (status-running current-bus-status))
                   (box-cas! (get-status) current-bus-status
                             (status #t #f)))
          (thread
           (thunk
            (let loop ([item (queue-pop-front! (get-message-queue))])
              (match item
                [(some (cons emitter (message tag data)))
                 (let* ([current-routes (unbox (get-routes))]
                        [receivers (hash-ref current-routes emitter '())])
                   (parameterize ([current-custodian cust])
                     (for ([receiver receivers])
                       (thread
                        (thunk
                         (send receiver on-message tag data))))))]
                [(none) (void)])
              (when (check-continue)
                (loop (queue-pop-front! (get-message-queue)))))))))
      (void))))

(define (make-minibus)
  (define minibus (new minibus%))
  ;; The minibus is wrapped in a delegator to protect
  ;; the methods that expose the internal state
  ;; of the object
  (delegator ([minibus (minibus<%>)])))

(define (make-unordered-minibus)
  (define minibus (new unordered-minibus%))
  ;; The minibus is wrapped in a delegator to protect
  ;; the get-status method
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
