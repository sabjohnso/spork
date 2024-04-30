#lang racket

(module+ test
  (require
   spork/infix-notation spork/curried
   rackunit rackunit/spec)

  (describe "infix expressions"
    (it "uses a quasiquoted identifier as the function for application"
      (check-equal? (1 `+ 2) 3)
      (check-equal? ('(1 2) `append '(3 4)) '(1 2 3 4))
      (check-equal? ((sqrt `compose sqr) 3) 3)
      (check-true (1 `< 2)))

    (it "will can use more than two terms if the operator supports that many arguments"
      (check-equal? (1 `+ 2 `+ 3 `+ 4) 10)
      (check-true (1 `< 2 `< 3 `< 4)))

    (it "will not combine different operators"
      (check-exn exn:fail? (thunk (1 `+ 2 `* 3))))

    (context "given a binary function"
      (define (add x y)
	(+ x y))

      (it "can be used as an infix operator"
	(check-equal? (3 `add 4) 7))

      (it "cannot be applied to more than two arguments"
	(check-exn exn:fail? (thunk (1 `add 2 `add 3)))))

    (context "given a curried ternary function"
      (define-curried (fma a b c)
	(+ (* a b) c))

      (it "can be used as an infix operator for three arguments"
	(check-equal? (2 `fma 3 `fma 4) 10))

      (it "will return the curried function with only two argument"
	(check-equal? ((2 `fma 3) 4) 10))))

  (describe "partially applied infix expressions"
    (it "will curry an infix operator with the argument on the right"
      (define less-than-three? (`< 3))
      (check-true (less-than-three? 2))
      (check-false (less-than-three? 4)))

    (it "will curry an infix operator with the argument on the left"
      (define three-is-less-than? (3 `<))
      (check-true (three-is-less-than? 4))
      (check-false (three-is-less-than? 2))

      (define subtract-three-from (`- 3))
      (check-equal? (subtract-three-from 5) 2)))

  (describe "things that infix notation will not do because they are a bad ideas"

    (it "won't work with non-symbols as the function"
      (check-exn exn:fail? (thunk (1 `(λ (x y) (+ x y)) 2))))

    (it "won't work if the operator is not uniform doing so would be ambiguous and stupid"
      (check-exn exn:fail? (thunk (1 `+ 2 `* 3))))))
