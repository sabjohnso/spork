#lang racket

(module+ test
  (require spork/either spork/functor rackunit rackunit/spec)

  (describe "either"
    (it "is a discrminated union with two constructors"
      (describe "left"
        (define mx (left 'x))
        (it "constructs an either?"
          (check-true (either? mx)))

        (it "constructs an either?  that is a left?"
          (check-true (left? mx)))

        (it "does NOT construct a right"
          (check-false (right? mx))))

      (describe "right"
        (define mx (right 'y))
        (it "constructs an either?"
          (check-true (either? mx)))

        (it "constructs an either? that is a right?"
          (check-true (right? mx)))

        (it "does not construct an either? that is a left?"
          (check-false (left? mx)))))

    (context "with left and right values"
      (define mx (left 'x))
      (define my (right 'y))

      (it "is a monad"
        (check-true (monad? mx))
        (check-true (monad? my))

        (check-equal?
         (let/monad ([x (left 3)]
                     [y (left 4)])
           (return (+ x y)))
         (left 7)))

      (it "is an applicative functor"
        (check-true (applicative? mx))
        (check-true (applicative? my))

        (check-equal?
         (let/applicative ([x (left 3)]
                           [y (left 4)])
           (+ x y))
         (left 7)))

      (it "is a functor"
        (check-true (functor? mx))
        (check-true (functor? my))
        (check-equal?
         (let/functor ([x (left 3)]
                       [y (left 4)])
           (+ x y))
         (left (left 7)))))))
