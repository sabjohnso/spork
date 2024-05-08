#lang racket

(module+ test
  (require spork rackunit rackunit/spec)

  (describe "cokleisli"
    (it "accepts a comonad-destructor"
      (define ckf (cokleisli extract))
      (check-equal? (cokleisli-run ckf `(x . e)) 'x))

    (it "does not accept functions of the wrong arity"
      (check-exn exn:fail? (thunk (cokleisli (λ (x y) (+ x y))))))

    (it "does not accept non-procedure arguments"
      (check-exn exn:fail? (thunk (cokleisli "something-completely-different"))))

    (it "can be executed with cokleisli-run"
      (describe "cokleisli-run"
       (it "runs the input cokleisli on the second input"
         (check-equal? (cokleisli-run (cokleisli length) '(1 2 3 4)) 4))

       (it "only accepts cokleisli as the first argument"
         (check-exn exn:fail? (thunk (cokleisli-run (kleisli list) '(1 2 3 4)))))

       (it "only accepts comonads as the second argument"
         (check-exn exn:fail? (thunk (cokleisli-run (cokleisli car) "not a monad"))))))

    (it "is an arrow"
      (check-true (arrow? (cokleisli extract))))

    (it "is an arrow-choice"
      (check-true (arrow-choice? (cokleisli car)))

      (context "with some inputs and functions"
        (define wx (cons (left 'x) 'e))
        (define wy (cons (right 3) 'e))
        (define ckf (cokleisli (compose symbol->string extract)))
        (define ckg (cokleisli (compose number->string extract)))
        (define ckh (cokleisli (compose add1 extract)))

        (it "can choose left for applicateon"
          (check-equal? (cokleisli-run (choose-left ckf) wx) (left "x"))
          (check-equal? (cokleisli-run (choose-left ckf) wy) (right 3)))

        (it "can choose right for application"
          (check-equal? (cokleisli-run (choose-right ckh) wx) (left 'x))
          (check-equal? (cokleisli-run (choose-right ckh) wy) (right 4)))

        (it "can split over left and right"
          (check-equal? (cokleisli-run (choose ckf ckh) wx) (left "x"))
          (check-equal? (cokleisli-run (choose ckf ckh) wy) (right 4)))

        (it "can fanin"
          (check-equal? (cokleisli-run (fanin ckf ckg) wx) "x")
          (check-equal? (cokleisli-run (fanin ckf ckg) wy) "3")

          (check-equal?
           (extend
            (λ (wx) (cokleisli-run
                     (fanin ckf ckg) wx))
            (list (left 'x) (left 'y) (right 3) (right 4)))
           '("x" "y" "3" "4")))))))
