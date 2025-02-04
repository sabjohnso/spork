#lang racket

(module+ test
  (require spork/mutable-collections/queue spork/optional rackunit rackunit/spec)

  (describe "queues"
    (describe "make-queue"
      (it "makes an empty queue"
        (check-true (queue? (make-queue)))
        (check-true (queue-empty? (make-queue)))))

    (describe "queue?"
      (it "is a predicate recognizing queues"
        (check-true (queue? (make-queue))))
      (it "does not recoginize other things"
        (check-false (queue? 'some-thing-completely-different))))

    (describe "queue-push-back!"
      (it "pushes values onto a queue"
        (define queue (make-queue))
        (check-true (queue-empty? queue))
        (queue-push-back! queue 'x)
        (check-false (queue-empty? queue))))

    (describe "queue-pop-front!"
      (it "pops values from a queue in FIFO order"
        (define queue (make-queue))
        (queue-push-back! queue 'x)
        (queue-push-back! queue 'y)
        (check-equal? (some 'x) (queue-pop-front! queue))
        (check-equal? (some 'y) (queue-pop-front! queue))
        (check-equal? (none) (queue-pop-front! queue))))

    (context "with a new queue"
      (define queue (make-queue))
      (it "should be safe for futures"
        (let ([n 100])
          (for/async ([i (in-range n)])
            (queue-push-back! queue i))
          (for/async ([i (in-range n)])
            (check-false (none? (queue-pop-front! queue))))
          (check-true (queue-empty? queue)))))))
