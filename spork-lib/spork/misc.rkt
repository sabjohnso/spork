#lang racket

(provide
 (contract-out
  [call-with (-> any/c (-> any/c any/c) any/c)]
  [twc (-> number? number?)]
  [positive-real? predicate/c]
  [nonnegative-real? predicate/c]
  [nonpositive-real? predicate/c]
  [negative-real? predicate/c]))

(define (positive-real? x)
  (and (real? x) (> x 0)))

(define (nonnegative-real? x)
  (and (real? x) (>= x 0)))

(define (nonpositive-real? x)
  (and (real? x) (<= x 0)))

(define (negative-real? x)
  (and (real? x) (< x 0)))

(define (call-with x f)
  (f x))

(define (twc x)
  (+ x x))
