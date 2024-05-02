#lang racket

(module+ test
  (require
   spork/vector-extras
   rackunit rackunit/spec)

  (describe "vector-return"
    (it "puts a value into a vector context"
      (check-equal? (vector-return 'x) #(x))))
  
  (describe "vector-fmap"
    (it "maps a function over a vector"
      (check-equal? (vector-fmap sqr #(1 2)) #(1 4))))

  (describe "vector-fapply"
    (it "performs applicative mapping for vectors"
      (check-equal? (vector-fapply (vector-return sqr) #(3 4)) #(9 16))))

  (describe "vector-flatmap"
    (it "performs monadic mapping for vectors"
      (check-equal? (vector-flatmap (λ (x) (vector x x)) #(a b c)) #(a a b b c c)))))

