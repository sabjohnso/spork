#lang racket

(provide
 (contract-out
  [call-with (-> any/c (-> any/c any/c) any/c)]
  [twc (-> number? number?)]))

(define (call-with x f)
  (f x))

(define (twc x)
  (+ x x))
