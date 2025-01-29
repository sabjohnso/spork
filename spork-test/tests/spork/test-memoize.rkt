#lang racket

(module+ test
  (require rackunit rackunit/spec
           spork/memoize)

  (describe "thunk-memoize"
    (it "is used to define a thunk"
      (let* ([counter 0]
             [proc (thunk-memoize
                    (set! counter (add1 counter))
                    'value)])
        (it "will memoize the value on the first application"
          (check-equal? counter 0)
          (check-equal? (proc) 'value)
          (check-equal? counter 1))
        (it "will return the previously computed value thereafter"
          (check-equal? (proc) 'value)
          (check-equal? counter 1)))))

  (describe "lambda-memoize"
    (it "creates an unamed function"
      (let* ([counter 0]
             [add (lambda-memoize (x y)
                    (set! counter (add1 counter))
                    (+ x y))])

        (it "will comput the result the first time for each unique input"
          (check-equal? counter 0)
          (check-equal? (add 1 2) 3)
          (check-equal? counter 1)
          (check-equal? (add 3 4) 7)
          (check-equal? counter 2))

        (it "will return previously computed results when applied to the same inputs"
          (check-equal? (add 1 2) 3)
          (check-equal? counter 2)))))

  (describe "define-memoize"
    (it "creates an unamed function"
      (let* ([counter 0])
        (define-memoize (add x y)
          (set! counter (add1 counter))
          (+ x y))

        (it "will comput the result the first time for each unique input"
          (check-equal? counter 0)
          (check-equal? (add 1 2) 3)
          (check-equal? counter 1)
          (check-equal? (add 3 4) 7)
          (check-equal? counter 2))

        (it "will return previously computed results when applied to the same inputs"
          (check-equal? (add 1 2) 3)
          (check-equal? counter 2))))))
