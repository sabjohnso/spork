#lang racket

(module+ test
  (require
   spork/pair-extras
   rackunit rackunit/spec)

  (describe "pair-extract"
    (it "returns the first item from a pair"
      (check-equal? (pair-extract (cons 'x 'y)) 'x)))

  (describe "pair-duplicate"
    (it "duplicates a pair context"
      (check-equal? (pair-duplicate (cons 'x 'y)) (cons (cons 'x 'y) 'y))))

  (describe "pair-extend"
    (it "extends an observer function over a pair context"
      (check-equal? (pair-extend (compose symbol->string pair-extract) (cons 'x 'y))
                    (cons "x" 'y)))))
