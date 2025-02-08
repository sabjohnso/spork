#lang racket

(module core racket
  (provide
   (struct-out route)
   (struct-out message)
   (contract-out
    [minibus<%> interface?]
    [receiver<%> interface?]
    [minibus? predicate/c]
    [receiver? predicate/c]
    [make-minibus (-> minibus?)]
    [minibus-add-route! (-> minibus? route? void?)]
    [minibus-remove-route! (-> minibus? route? void?)]
    [minibus-handle-message! (-> minibus? any/c message? void?)]
    [minibus-run! (-> minibus? void?)]
    [minibus-stop! (-> minibus? void?)]
    [minibus-running? (-> minibus? boolean?)]
    [minibus-stopping? (-> minibus? boolean?)]
    [minibus-queueing? (-> minibus? boolean?)]))

  (require spork/optional spork/mutable-collections/queue)

  (define receiver<%>
    (interface ()
      [on-message (->m any/c any/c void?)]))

  (define (receiver? v)
    (is-a? v receiver<%>))

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
      [queueing? (->m boolean?)]))

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

      (define (check-continue)
        (let loop ([current-status (unbox bus-status)])
          (status-stopping current-status)
          (when (not (box-cas! bus-status current-status (status #f #f)))
            (loop (unbox bus-status)))))

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

  (define (make-minibus)
    (new minibus%))

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
    (send minibus queueing?)))

(require (submod "." core))

(module+ test
  (require spork/optional spork/mutable-collections/queue rackunit rackunit/spec)

  (define mock-receiver%
    (class* object%
        (receiver<%>)
      (super-new)
      (init-field [calls-queue (make-queue)])
      (define/public (get-next-call) (queue-pop-front! calls-queue))
      (define/public (on-message . args)
        (displayln (cons 'on-message args))
        (queue-push-back! calls-queue (cons 'on-message  args))
        (check-false (queue-empty? calls-queue)))))

  (describe "make-minibus"
    (it "makes a minibus"
      (define minibus (make-minibus))
      (check-true (minibus? minibus))
      (check-false (minibus-running? minibus))))

  (describe "minibus?"
    (it "is a predicate recognizing minibusses"
      (check-true (minibus? (make-minibus)))
      (check-false (minibus? 82))))

  (context "with a running minibus and a receiver"
    (define minibus (make-minibus))
    (minibus-run! minibus)
    (check-true (minibus-running? minibus))

    (define receiver (new mock-receiver%))

    (describe "minibus-add-route"
      (it "adds a route to a minibus"
        (define receiver (new mock-receiver%))
        (minibus-add-route! minibus (route 'some-emitter receiver))))

    (describe "minibus-handle-message"
      (it "uses the minibus to handle a message"
        (check-true (minibus-running? minibus))
        (check-false (minibus-queueing? minibus))
        (minibus-handle-message! minibus 'some-emitter (message 'my-tag "Some special data!"))
        (check-true (minibus-queueing? minibus))
        (sleep 1)
        (check-false (minibus-queueing? minibus))
        (check-false (queue-empty? (get-field calls-queue receiver)))
        (check-equal? (send receiver get-next-call) (some '(on-message 'my-tag "Some special data!")))))))
