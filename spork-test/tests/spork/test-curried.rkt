#lang racket

(module+ test
  (require
   spork/curried
   rackunit rackunit/spec)

  (describe "define-curried/contract"
    (context "with a curried binary function defined with a contract"
      (define-curried/contract (add x y)
        (curried-> number? number? number?)
        (+ x y))

      (it "can be applied to two arguments"
        (check-equal? (add 3 4) 7))

      (it "can be applied to one argument and the result applied to another argument"
        (check-equal? ((add 3) 4) 7))

      (it "cannot be applied to three arguments"
        (check-exn exn:fail? (thunk (add 3 4 5)))))

    (context "with a curried unary function returning a curried function and a contract"
      (define/contract mul
        (curried-> number? (curried-> number? number?))
        (lambda-curried (x)
          (lambda-curried (y)
	    (* x y))))

      (it "can be applied to two arguments"
        (check-equal? (mul 3 4) 12))

      (it "can be applied to one argument and the result applied to another"
        (check-equal? ((mul 3) 4) 12))

      (it "cannot be applied to three arguments"
        (check-exn exn:fail? (thunk (mul 3 4 5)))))

    (context "with a curried unary function"
      (define-curried/contract (symbol-name sym)
        (-> symbol? string?)
        (symbol->string sym))

      (check-equal? (symbol-name 'x) "x"))))
