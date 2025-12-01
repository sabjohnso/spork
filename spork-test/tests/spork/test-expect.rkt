#lang racket

(module+ test
  (require
   spork/expect spork/functor spork/infix-notation spork/curried
   rackunit rackunit/spec)

  (define-curried (safe-divide x y)
    (if (zero? y) (unexpected "division by zero")
      (expected (/ x y))))

  (define-curried (add x y)
    (+ x y))

  (describe "expect"
    (it "is a discriminated union with two constructors"
      (describe "unexpected"
        (it "is an unary constructor for unexpected values"
          (check-true (expect? (unexpected "error message")))))

      (describe "expected"
        (it "is a unary constructor for expected values"
          (check-true (expect? (expected 'result)))))

      (describe "unexpected?"
        (it "recognizes unexpected values"
          (check-true (unexpected? (unexpected "error message"))))

        (it "does not recognize expected values"
          (check-false (unexpected? (expected 'result))))

        (it "does not accept other argument types"
          (check-exn exn:fail? (thunk (unexpected? "something completely different")))))

      (describe "expected?"
        (it "recognizes expected values"
          (check-true (expected? (expected 'result))))

        (it "does not recognize unexpected values"
          (check-false (expected? (unexpected "error message"))))

        (it "only accepts expect type arguments"
          (check-exn exn:fail? (thunk (expected? "something completely different"))))))

    (it "is not trivial"
      (check-false (trivial? (expected 'result)))
      (check-false (trivial? (unexpected "error message"))))

    (it "is a monad"
      (check-true (monad? (expected 'result)))
      (check-true (monad? (unexpected "error message")))

      (it "can be operated on by flatmap"
        (check-equal? (flatmap (safe-divide 100) (expected 4)) (expected 25))
        (check-equal? (flatmap (safe-divide 100) (expected 0)) (unexpected "division by zero"))
        (check-equal? (flatmap (safe-divide 100) (unexpected "error message")) (unexpected "error message"))
        (check-equal? (flatmap expected (expected 'result)) (expected 'result))
        (check-equal? (flatmap return (expected 'result)) (expected 'result)))

      (it "has expected as a unit constructor"
        (check-equal? (((safe-divide 100) `>=> expected) 4) (expected 25))
        (check-equal? (((safe-divide 100) `>=> return) 4) (expected 25))
        (check-equal? ((expected `>=> (safe-divide 100)) 4) (expected 25))
        (check-equal? ((return `>=> (safe-divide 100)) 4) (expected 25))
        (check-equal? (((safe-divide 100) `>=> expected) 0) (unexpected "division by zero"))
        (check-equal? (((safe-divide 100) `>=> return) 0) (unexpected "division by zero"))
        (check-equal? ((expected `>=> (safe-divide 100)) 0) (unexpected "division by zero"))
        (check-equal? ((return `>=> (safe-divide 100)) 0) (unexpected "division by zero")))


      (it "can use monadic binding syntax"
        (check-equal?
         (let/monad ([x (expected 3)]
                     [y (expected -4)])
                    (safe-divide 100 (+ x y)))
         (expected -100))

        (check-equal?
         (let/monad ([x (expected 4)]
                     [y (expected -4)])
                    (safe-divide 100 (+ x y)))
         (unexpected "division by zero"))

        (check-equal?
         (let/monad ([x (unexpected "error message")]
                     [y (expected -4)])
                    (safe-divide 100 (+ x y)))
         (unexpected "error message"))

        (check-equal?
         (let/monad ([x (expected 3)]
                     [y (unexpected "error message")])
                    (safe-divide 100 (+ x y)))
         (unexpected "error message"))

        (check-equal?
         (let/monad ([x (unexpected "error message 1")]
                     [y (unexpected "error message 2")])
                    (safe-divide 100 (+ x y)))
         (unexpected "error message 1"))))


    (it "is an applicative functor"
      (check-true (applicative? (expected 'result)))
      (check-true (applicative? (unexpected "error message")))

      (it "can be operated on by fapply"
        (check-equal? (fapply (expected sqr) (expected 3)) (expected 9))
        (check-equal? (fapply (pure sqr) (expected 3)) (expected 9))
        (check-equal? (fapply (expected sqr) (unexpected "error message")) (unexpected "error message"))
        (check-equal? (fapply (unexpected "error message") (expected 3)) (unexpected "error message")))

      (it "can use applicative binding syntax"
        (check-equal?
         (let/applicative ([x (expected 3)]
                           [y (expected 4)])
                          (+ x y))
         (expected 7))

        (check-equal?
         (let/applicative ([x (unexpected "error message")]
                           [y (expected 4)])
                          (+ x y))
         (unexpected "error message"))

        (check-equal?
         (let/applicative ([x (expected 3)]
                           [y (unexpected "error message")])
                          (+ x y))
         (unexpected "error message"))

        (check-equal?
         (let/applicative ([x (unexpected "error message 1")]
                           [y (unexpected "error message 2")])
                          (+ x y))
         (unexpected "error message 1"))))

    (it "is a functor"
      (check-true (functor? (expected 'result)))
      (check-true (functor? (unexpected "error message")))

      (it "can be operated on by fmap"
        (check-equal? (fmap sqr (expected 3)) (expected 9))
        (check-equal? (fmap sqr (unexpected "error message")) (unexpected "error message"))
        (check-equal? (sqr `<$> (unexpected "error message")) (unexpected "error message"))))))
