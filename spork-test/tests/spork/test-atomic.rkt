#lang racket

(module+ test
  (require spork/atomic racket/future rackunit rackunit/spec)

  (describe "atomically"
    (it "accepts a thunk and executes it atomically"
      (let ([x 0]
            [n 1000])
        (for/async ([i (in-range n)])
          (atomically
           (thunk (set! x (add1 x)))))
        (check-equal? x n)))))
