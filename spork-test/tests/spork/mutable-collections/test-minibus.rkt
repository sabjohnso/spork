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
        (check-equal? (send receiver get-next-call)
                      (some '(on-message my-tag message-data0)))
        (check-equal? (send receiver get-next-call)
                      (some '(on-message my-tag message-data1)))
        (check-equal? (send receiver get-next-call)
                      (none))))))
