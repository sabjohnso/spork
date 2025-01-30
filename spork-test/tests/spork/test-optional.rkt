#lang racket

(require
 spork/optional spork/functor spork/infix-notation spork/curried
 rackunit rackunit/spec)

(define (safe-divide x y)
  (if (zero? y) (none)
    (some (/ x y))))

(define-curried (add x y)
  (+ x y))

(describe "optional"
  (it "is a discriminated union with two constructors"
    (describe "none"
      (it "is a nullary constructor for optional"
        (check-true (optional? (none)))))

    (describe "some"
      (it "is an unary constructor for optional"
        (check-true (optional? (some 'x)))))

    (describe "none?"
      (it "recognizes optional values constructed with `none`"
        (check-true (none? (none))))

      (it "does not recognize optional values constructed with `some`"
        (check-false (none? (some 'x)))))

    (describe "some?"
      (it "recognizes optional values constructed with `some`"
        (check-true (some? (some 'x))))

      (it "does not recognize optional values constructed with `none`"
        (check-false (some? (none))))))

  (it "is a monad"
    (check-true (monad? (none)))
    (check-true (monad? (some 'x)))

    (it "can be operated on by flatmap"
      (check-equal?
       (flatmap (λ (x) (if (positive? x) (some (sqr x)) (none))) (some 3))
       (some 9))

      (check-equal?
       (flatmap (λ (x) (if (positive? x) (some (sqr x)) (none))) (some -3))
       (none))

      (check-equal?
       (flatmap (λ (x) (if (positive? x) (some (sqr x)) (none))) (none))
       (none)))

    (it "can be operated on by join"
      (check-equal? (join (some (some 'x))) (some 'x))
      (check-equal? (join (some (none))) (none))
      (check-equal? (join (none)) (none)))

    (it "has `some` as its unit constructor"
      (check-equal? ((some 'x) `>>= some) (some 'x))
      (check-equal? ((some 'x) `>>= return) (some 'x))
      (check-equal? ((none) `>>= some) (none))
      (check-equal? ((none) `>>= return) (none)))

    (it "can use monad binding syntax"
      (define (optional-safe-divide mx my)
        (let/monad ([x mx]
                    [y my])
          (safe-divide x y)))

      (check-equal? (optional-safe-divide (some 6) (some 2)) (some 3))
      (check-equal? (optional-safe-divide (some 6) (some 0)) (none))
      (check-equal? (optional-safe-divide (some 6) (none)) (none))
      (check-equal? (optional-safe-divide (none) (some 2)) (none))))

  (it "is an applicative functor"
    (check-true (applicative? (none)))
    (check-true (applicative? (some 'x)))

    (it "can be operated on by fapply"
      (check-equal? (fapply (some sqr) (some 3)) (some 9))
      (check-equal? (fapply (pure sqr) (some 3)) (some 9))
      (check-equal? (fapply (some sqr) (pure 3)) (some 9))

      (check-equal? (fapply (some sqr) (none)) (none))
      (check-equal? (fapply (none) (some 3)) (none)))

    (it "can use an infix version of fapply"
      (check-equal? ((some add) `<*> (some 3) `<*> (some 4)) (some 7))
      (check-equal? ((pure add) `<*> (some 3) `<*> (some 4)) (some 7))
      (check-equal? ((some add) `<*> (pure 3) `<*> (some 4)) (some 7))
      (check-equal? ((some add) `<*> (some 3) `<*> (pure 4)) (some 7))
      (check-equal? ((none) `<*> (some 3) `<*> (some 4)) (none))
      (check-equal? ((some add) `<*> (none) `<*> (some 4)) (none))
      (check-equal? ((some add) `<*> (some 3) `<*> (none)) (none)))

    (it "can use applicative binding syntax"
      (define (optional-add mx my)
        (let/applicative ([x mx]
                          [y my])
          (+ x y)))

      (check-equal? (optional-add (some 3) (some 4)) (some 7))
      (check-equal? (optional-add (none) (some 4)) (none))
      (check-equal? (optional-add (some 3) (none)) (none))))

  (it "is a functor"
    (check-true (functor? (none)))
    (check-true (functor? (some 'x)))

    (it "can be operated on by `fmap`"
      (check-equal? (fmap sqr (some 3)) (some 9))
      (check-equal? (fmap sqr (none)) (none)))

    (it "can be operated on by the infix version of `fmap`"
      (check-equal? (sqr `<$> (some 3)) (some 9))
      (check-equal? (sqr `<$> (none)) (none)))

    (it "can use functor binding syntax"
      (define (optional-add mx my)
        (let/functor ([x mx]
                      [y my])
          (+ x y)))

      (check-equal? (optional-add (some 3) (some 4)) (some (some 7)))
      (check-equal? (optional-add (none) (some 4)) (none))
      (check-equal? (optional-add (some 3) (none)) (some (none))))))
