#lang racket

(module+ test
  (require
   spork
   rackunit rackunit/spec)

  (describe "stateful"
    (it "accepts an unary procedure returning a pair"
      (define mx (stateful (λ (s) `(x . ,s))))
      (check-equal? (stateful-run 's mx) '(x . s)))

    (it "does not accept non-procedure arguments"
      (check-exn exn:fail? (thunk (stateful 'x))))

    (it "does not accept argument of a different arity"
      (check-exn exn:fail? (thunk (stateful (thunk (void))))))

    (it "will fail to run if provided procedure that does not return a pair"
      (define mx (stateful identity))
      (check-exn exn:fail? (thunk (stateful-run 's mx))))

    (it "is a monad"
      (check-true (monad? (stateful (λ (s) `(x . ,s)))))
      (it "can use flatmap, return and monad binding"
        (check-equal?
         (stateful-run 4
           (flatmap (λ (x)
                      (let/monad ([y stateful-get])
                        (stateful-modify add1)
                        (return (+ x y))))
                    (return 3)))
         `(7 . 5))))

    (it "is an applicative functor"
      (check-true (applicative? (stateful (λ (s) `(x . ,s)))))
      (it "can use pure and applicative binding"
        (check-equal?
         (stateful-run 4
           (let/applicative ([x stateful-get]
                             [y (pure 3)])
             (+ x y)))
         '(7 . 4))))

    (it "is a functor"
      (check-true (functor? (stateful (λ (s) `(x . ,s)))))
      (it "can use fmap"
        (check-equal?
         (stateful-run 's (fmap sqr (stateful-return 3)))
         '(9 . s))))


    (describe "stateful-get"
      (it "is used to get the state"
        (stateful-run 's stateful-get)
        '(s . s)))

    (describe "stateful-select"
      (it "is used to select from the state with a function"
        (let ([s (hash 'x 3 'y 4)])
          (check-equal?
           (stateful-run s (stateful-select (λ (s) (hash-ref s 'x))))
           (cons (hash-ref s 'x) s)))))

    (describe "stateful-put"
      (it "sets the state to the input value"
        (check-equal?
         (stateful-run 's0 (stateful-put 's1))
         (cons undefined-value 's1))))

    (describe "stateful-modify"
      (it "modifies the state with the input function"
        (check-equal?
         (stateful-run 's (stateful-modify symbol->string))
         (cons undefined-value "s")))))

  (describe "struct/stateful"
    (it "it defines struct types with additional definitions for reading and writing state"
      (context "with a type defined using struct/stateful"
        (struct/stateful example
          (field1 field2)
          #:transparent)
        (define a 0)
        (define b 1)
        (define c 2)
        (define d 3)
        (define s (example a b))

        (it "has getters to read from state"
          (check-equal?
           (stateful-eval s example-field1/stateful)
           (example-field1 s))

          (check-equal?
           (stateful-eval s example-field2/stateful)
           (example-field2 s)))

        (it "has setters to modify the state"
          (check-equal?
           (stateful-eval s
             (begin/monad
              (set-example-field1/stateful c)
              (set-example-field2/stateful d)
              stateful-get))
           (example c d)))))))
