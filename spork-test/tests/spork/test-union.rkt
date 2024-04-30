#lang racket

(module+ test
  (require
   rackunit rackunit/spec
   spork/union spork/functor)

  (describe "union"
    (it "is used to define a discriminated union"
      (context "with either defined as a discriminated union"

        (union either
          (left value)
          (right value)
          #:methods gen:monad
          ((define (return-proc _) left)
           (define (flatmap-proc _) either-flatmap)))

        (define (either-flatmap f mx)
          (match mx
            [(left x) (f x)]
            [mx mx]))

        (it "produces predicate and constructors for each variant"
          (check-true (left? (left 'x)))
          (check-true (right? (right 'y)))
          (check-false (left? (right 'y)))
          (check-false (right? (left 'x))))

        (it "produces a predicate for the union"
          (check-true (either? (left 'x)))
          (check-true (either? (right 'x)))
          (check-false (either? "A duck!")))

        (it "can implement methods for generics"
          (check-equal?
           (let/monad ([x (left 3)]
                       [y (left 4)])
             (return (+ x y)))
           (left 7))

          (check-equal?
           (let/monad ([x (left 3)]
                       [y (right "WARNING: You've been warned!")])
             (return (+ x y)))
           (right "WARNING: You've been warned!")))))))
