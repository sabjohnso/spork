#lang racket

(module+ test
  (require spork rackunit rackunit/spec)

  (describe "env"
    (it "is a struct type for computing with an environment"
      (describe "env-return"
        (it "puts a value into the a context with an environment"
          (check-true (env? (env-return 'x)))
          (check-equal? (env-run 'e (env-return 'x)) 'x)))

      (describe "env-ask"
        (it "returns the environment"
          (check-equal? (env-run 'e env-ask) 'e)))

      (describe "env-select"
        (it "returns a selection from the environment"
          (check-equal? (env-run 'e (env-select symbol->string)) "e")))

      (describe "env-local"
        (it "runs an computation in a local environment"
          (check-equal? (env-run 'e (env-local symbol->string env-ask)) "e"))))

    (it "is a monad"
      (check-true (monad? (env-return 'x))))

    (it "is an applicative functor"
      (check-true (applicative? (env-return 'x))))

    (it "is a functor"
      (check-true (functor? (env-return 'x))))))
