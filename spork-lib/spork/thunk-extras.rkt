#lang racket

(provide
 (contract-out
  [thunk? predicate/c]
  [thunk-wrap (-> any/c thunk?)]
  [thunk-unwrap (-> thunk? any/c)]))

(define (thunk? x)
  (and (procedure? x) (procedure-arity-includes? x 0)))

(define (thunk-wrap x)
  (thunk x))

(define (thunk-unwrap wx)
  (wx))

