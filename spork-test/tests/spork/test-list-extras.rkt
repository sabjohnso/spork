#lang racket

(module+ test
  (require
   rackunit rackunit/spec
   spork/list-extras)

  (describe "list-return"
    (check-equal? (list-return 'x) '(x)))

  (describe "rappend"
    (it "appends the second input list to the reverse of the first input list"
      (check-equal?
       (rappend '(2 1) '(3 4))
       '(1 2 3 4))))

  (describe "list-flatmap"
    (it "maps a list constructor over a list, returning a list"
      (check-equal?
       (list-flatmap (λ (x) (list x x)) '(1 2 3))
       '(1 1 2 2 3 3)))))
