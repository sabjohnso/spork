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
      (check-exn exn:fail? (thunk (kleisli-run kf 3))))

    (it "will not accept arguments that are procedures of the wrong arity"
      (check-exn exn:fail? (thunk (kleisli (λ (x y) (+ x y))))))

    (it "will not accept arguments that are not procedures"
      (check-exn exn:fail? (thunk (kleisli "something completely different"))))

    (it "is a category"
      (check-true (category? (kleisli some)))
      (it "supports composition of constructors from right to left"
        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `<< (kleisli (λ (x) (list x 'a)))) 'x)
         '(x x a a)))

      (it "supports composition of constructors from left to right"
        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `>> (kleisli (λ (x) (list x 'a)))) 'x)
         '(x a x a)))

      (it "supports using the category id"
       (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `>> id `>> (kleisli (λ (x) (list x 'a)))) 'x)
         '(x a x a))))

    (it "is an arrow"
      (check-true (arrow? (kleisli list)))

      (it "supports composition of constructors from right to left with arrow operators"
        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `<<< (kleisli (λ (x) (list x 'a)))) 'x)
         '(x x a a))

        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `<<< (arr symbol->string)) 'x)
         '("x" "x"))


        (check-equal?
         (kleisli-run ((arr symbol->string) `<<< (kleisli (λ (x) (list x x)))) 'x)
         '("x" "x"))

        (check-equal?
         (kleisli-run (symbol->string `^<< (kleisli (λ (x) (list x x)))) 'x)
         '("x" "x"))

        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `<<^ symbol->string) 'x)
         '("x" "x")))

      (it "supports composition of constructors from left to right with arrow operators"
        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `>>> (kleisli (λ (x) (list x 'a)))) 'x)
         '(x a x a))

        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `>>> (arr symbol->string)) 'x)
         '("x" "x"))

        (check-equal?
         (kleisli-run ((arr symbol->string) `>>> (kleisli (λ (x) (list x x))) ) 'x)
         '("x" "x"))

        (check-equal?
         (kleisli-run ((kleisli (λ (x) (list x x))) `>>^ symbol->string) 'x)
         '("x" "x"))

        (check-equal?
         (kleisli-run (symbol->string `^>> (kleisli (λ (x) (list x x))) ) 'x)
         '("x" "x")))


      (it "supports the split function"
        (check-equal? (kleisli-run (split (kleisli (λ (x) (list x x))) (kleisli (λ (y) (list y y y)))) '(x . y))
                      '((x . y) (x . y) (x . y) (x . y) (x . y) (x . y)))

        (check-equal?
         (flatmap
          (λ (xy)
            (kleisli-run (split (kleisli (λ (x) (list (symbol->string x))))
                                (kleisli (λ (y) (if (= 2 y) '() (list y))))) xy))
          '((a . 1) (b . 2) (c . 3)))
         '(("a" . 1) ("c" . 3))))

      (it "supports fst function"
        (check-equal? (kleisli-run (fst (kleisli (λ (x) (list x x)))) '(x . y)) '((x . y) (x . y))))

      (it "supports the snd function"
        (check-equal? (kleisli-run (snd (kleisli (λ (y) (list y y)))) '(x . y))
                      '((x . y) (x . y))))

      (it "supports the fanout function"
        (define (func x)
          (kleisli-run
           (fanout (kleisli (λ (x) (if (> x 0) (list x) '())))
                   (kleisli (compose return number->string)))
           x))
        (check-equal? (func -1) '())
        (check-equal? (func 4) '((4 . "4")))))

    (it "is an arrow-choice"
      (check-true (arrow-choice? (kleisli return)))

      (it "supports choose-left"
        (define (func x)
          (flatmap list (kleisli-run (choose-left (kleisli (λ (x) (list x x)))) x)))
        (check-equal? (func (left 'x)) (list (left 'x) (left 'x)))
        (check-equal? (func (right 'y)) (list (right 'y))))

      (it "supports choose-right"
        (define (func x)
          (flatmap list (kleisli-run (choose-left (kleisli (λ (x) (list x x)))) x)))
        (check-equal? (func (left 'x)) (list (left 'x) (left 'x)))
        (check-equal? (func (right 'y)) (list (right 'y))))

      (it "suports choose"
        (check-equal?
         (kleisli-run
          (choose (kleisli list) (kleisli (λ (x) (list x x))))
          (left 'x))
         (list (left 'x))))

      (it "supports the operator (+++) for choose"
        (check-equal?
         (kleisli-run
          ((kleisli list) `+++ (kleisli (λ (x) (list x x))))
          (left 'x))
         (list (left 'x))))

      (it "supports fanin"
        (define (func x)
          (kleisli-run (fanin (kleisli some) (kleisli (const (none)))) x))
        (check-equal? (func (left 'x)) (some 'x))
        (check-equal? (func (right 'y)) (none)))

      (it "supports the operator for fanin"
        (define (func x) (kleisli-run ((kleisli some) `/// (kleisli (const (none)))) x))
        (check-equal? (func (left 'x)) (some 'x))
        (check-equal? (func (right 'y)) (none))))))
