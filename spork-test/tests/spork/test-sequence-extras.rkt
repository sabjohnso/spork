#lang racket

(module+ test
  (require spork/sequence-extras
           rackunit rackunit/spec)

  (describe "sequence-return"
    (it "puts a value into a sequence context"
      (define xs (sequence-return 'x))
      (check-true (sequence? xs))
      (check-equal? (sequence->list xs) '(x))))

  (describe "sequence-fmap"
    (it "maps over a sequence"
      (define xs (sequence-fmap sqr (in-list '(1 2))))
      (check-true (sequence? xs))
      (check-equal? (sequence->list xs) '(1 4))))

  (describe "sequence-fapply"
    (it "performs applicative mapping for sequences"
      (define xs (sequence-fapply (list sqr) (list 1 2)))
      (check-true (sequence? xs))
      (check-equal? (sequence->list xs) '(1 4))))

  (describe "sequence-flatmap"
    (it "performs monadic-mapping for sequences"
      (define xs (sequence-flatmap (λ (x) (stream x x)) (stream 'a 'b)))
      (check-true (sequence? xs))
      (check-equal? (sequence->list xs) '(a a b b))))

  (describe "sequence-join"
    (it "flattens a sequence"
      (define xs (sequence-join (list (stream 'a 'b) (stream 'c 'd))))
      (check-true (sequence? xs))
      (check-equal? (sequence->list xs) '(a b c d)))))
