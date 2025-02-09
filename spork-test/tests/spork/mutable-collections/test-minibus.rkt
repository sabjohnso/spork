#lang racket

(module+ test
  (require
   spork/mutable-collections/minibus spork/optional spork/mutable-collections/queue
   rackunit rackunit/spec)

  (define mock-receiver%
    (class* object%
        (receiver<%>)
      (super-new)
      (init-field [queue (make-queue)])
      (define/public (get-next-call) (queue-pop-front! queue))
      (define/public (on-message . args)
        (queue-push-back! queue (cons 'on-message  args)))

      (define/public (empty?)
        (queue-empty? queue))))

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
        (minibus-add-route! minibus (route 'some-emitter receiver))))

    (describe "minibus-handle-message"
      (it "uses the minibus to handle a message"
        (check-true (minibus-running? minibus))
        (check-false (minibus-queueing? minibus))
        (minibus-handle-message! minibus 'some-emitter (message 'my-tag 'message-data0))
        (minibus-handle-message! minibus 'some-emitter (message 'my-tag 'message-data1))
        (check-true (minibus-queueing? minibus))

        (let loop ()
          (when (minibus-queueing? minibus)
            (loop)))
        (minibus-stop! minibus)

        (let loop ()
          (when (minibus-stopping? minibus)
            (loop)))

        (let ([calls
               (let loop ([accum '()])
                 (match (send receiver get-next-call)
                   [(some call) (loop (cons call accum))]
                   [(none) (reverse accum)]))])
          (check-equal? (list-ref calls 0) '(on-message my-tag message-data0))
          (check-equal? (list-ref calls 1) '(on-message my-tag message-data1))))))

  (describe "unordered minibuses"
    (define minibus (make-unordered-minibus))
    (define receiver (new mock-receiver%))
    (minibus-add-route! minibus (route 'some-emitter receiver))
    (minibus-run! minibus)
    (describe "minibus-handle-message for unordered minbuses"
      (it "uses the minibus to handle a message"
        (check-true (minibus-running? minibus))
        (check-false (minibus-queueing? minibus))
        (minibus-handle-message! minibus 'some-emitter (message 'my-tag 'message-data0))
        (minibus-handle-message! minibus 'some-emitter (message 'my-tag 'message-data1))
        (check-true (minibus-queueing? minibus))

        (let loop ()
          (when (minibus-queueing? minibus)
            (loop)))
        (minibus-stop! minibus)

        (let loop ()
          (when (minibus-stopping? minibus)
            (loop)))

        (let ([calls
               (list->set
                (let loop ([accum '()])
                  (match (send receiver get-next-call)
                    [(some call) (loop (cons call accum))]
                    [(none) (reverse accum)])))])
          (check-true (set-member? calls '(on-message my-tag message-data0)))
          (check-true (set-member? calls '(on-message my-tag message-data1))))))))
