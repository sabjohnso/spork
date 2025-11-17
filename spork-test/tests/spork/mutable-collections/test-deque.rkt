#lang racket

(module+ test
  (require
   spork/mutable-collections/deque
   spork/mutable-collections/stack
   spork/mutable-collections/queue
   spork/optional
   rackunit rackunit/spec)

  (describe "deques"
    (describe "make-deque"
      (it "makes an empty deque, which is also a queue and a stack"
        (define deque (make-deque))
        (check-true (deque? deque))
        (check-true (deque-empty? deque))
        (check-true (queue? deque))
        (check-true (stack? deque))))
    (describe "deque?"
      (it "is a predicate that recognizes deques"
        (check-true (deque? (make-deque)))))

    (describe "deque-push-back!"
      (it "pushes items onto the back of the deque"
        (context "with a defined deque"
          (define deque (make-deque))
          (check-true (deque-empty? deque))
          (deque-push-back! deque 'x)
          (check-false (deque-empty? deque)))))

    (describe "deque-push-front!"
      (it "pushes items onto the front of the deque"
        (context "with a defined deque"
          (define deque (make-deque))
          (check-true (deque-empty? deque))
          (deque-push-front! deque 'x)
          (check-false (deque-empty? deque)))))

    (describe "deque-pop-front!"
      (it "removes and returns the item at the back of the deque"
        (context "with a deque defined"
          (define deque (make-deque))
          (it "returns elements pushed on the front in LIFO order"
            (deque-push-front! deque 'x)
            (deque-push-front! deque 'y)
            (check-equal? (deque-pop-back! deque) (some 'x))
            (check-equal? (deque-pop-back! deque) (some 'y))
            (check-equal? (deque-pop-back! deque) (none)))
          (it "returns elements pushed on the back in FIFO order"
            (deque-push-back! deque 'x)
            (deque-push-back! deque 'y)
            (check-equal? (deque-pop-back! deque) (some 'y))
            (check-equal? (deque-pop-back! deque) (some 'x))
            (check-equal? (deque-pop-back! deque) (none))))))

    (describe "deque-pop-front!"
      (it "removes and returns the item at the front of the deque"
        (context "with a deque defined"
          (define deque (make-deque))
          (it "returns elements pushed on the back in LIFO order"
            (deque-push-back! deque 'x)
            (deque-push-back! deque 'y)
            (check-equal? (deque-pop-front! deque) (some 'x))
            (check-equal? (deque-pop-front! deque) (some 'y))
            (check-equal? (deque-pop-front! deque) (none)))
          (it "returns elements pushed on the front in FIFO order"
            (deque-push-front! deque 'x)
            (deque-push-front! deque 'y)
            (check-equal? (deque-pop-front! deque) (some 'y))
            (check-equal? (deque-pop-front! deque) (some 'x))
            (check-equal? (deque-pop-front! deque) (none))))))

    (describe "deque-clear!"
      (it "removes all data from a deque"
        (define deque (make-deque))
        (deque-push-front! deque 'x)
        (deque-push-front! deque 'y)
        (deque-clear! deque)
        (check-true (deque-empty? deque))))

    (describe "deque-empty?"
      (it "is a predicate on deques that recognizes empty deques"
        (define deque (make-deque))
        (check-true (deque-empty? deque))
        (deque-push-back! deque 'x)
        (check-false (deque-empty? deque))))

    (context "with a define deque"
      (define deque (make-deque))
      (it "is robust against multiple consumers and producers"
        (let ([n 1000]
              [sum (box 0)])
          (for/async ([i (in-range n)])
            (if (even? i) (deque-push-front! deque i)
              (deque-push-back! deque i)))
          (for/async ([i (in-range n)])
            (match-let ([(some value)
                         (if (even? i) (deque-pop-back! deque)
                           (deque-pop-front! deque)) ])
              (let loop ([sum* (unbox sum)])
                (when (not (box-cas! sum sum* (+ sum* value)))
                  (loop (unbox sum))))))
          (check-true (deque-empty? deque))
          (check-equal? (unbox sum) (/ (* n (sub1 n)) 2)))))))
