#lang racket

(module+ test
  (require spork/mutable-collections/lock-free-queue spork/optional racket/future rackunit rackunit/spec)
  (describe "lock free queues"

    (describe "make-lock-free-queue"
      (it "makes a queue"
        (check-true (queue? (make-lock-free-queue)))))

    (describe "queue?"
      (it "is a predicate recognizing queues"
        (check-true (queue? (make-lock-free-queue))))
      (it "does not recoginize other things"
        (check-false (queue? 'some-thing-completely-different))))

    (describe "queue-push-back!"
      (it "pushes values onto a queue"
        (define queue (make-lock-free-queue))
        (check-true (queue-empty? queue))
        (queue-push-back! queue 'x)
        (check-false (queue-empty? queue))))

    (describe "queue-pop-front!"
      (it "pops values from a queue in FIFO order"
        (define queue (make-lock-free-queue))
        (queue-push-back! queue 'x)
        (queue-push-back! queue 'y)
        (check-equal? (some 'x) (queue-pop-front! queue))
        (check-equal? (some 'y) (queue-pop-front! queue))
        (check-equal? (none) (queue-pop-front! queue))))


    (context "with a lock free queue"
      (define queue (make-lock-free-queue))
      (define n 100)
      (for/async ([i (in-range n)])
        (queue-push-back! queue i))
      (for/async ([i (in-range n)])
        (check-true (some? (queue-pop-front! queue))))
      (check-true (queue-empty? queue)))))
