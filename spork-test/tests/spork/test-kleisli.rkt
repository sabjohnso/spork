#lang racket

(module+ test
  (require spork rackunit rackunit/spec)

  (describe "kleisli"
    (it "is a struct type holding a monad constructor"
      (define kf (kleisli list))
      (check-true (kleisli? kf))
      (check-equal? (kleisli-run kf 'x) '(x)))

    (it "will fail to run if provided a function that is not a monad constructor"
      (define kf (kleisli symbol->string))
      (check-true (kleisli? kf))
      (check-exn exn:fail? (thunk (kleisli-run kf 3))))))
