#lang racket

(provide
 (contract-out
  [optional? predicate/c]
  [none? (-> optional? boolean?)]
  [some? (-> optional? boolean?)]
  [none (-> optional?)]
  [some (-> any/c optional?)]))

(require spork/union spork/functor)

(union optional
  (none)
  (some value)
  #:methods gen:monad
  ((define (return-proc this) some)
   (define (flatmap-proc this) optional-flatmap)))

(define (optional-flatmap f mx)
  (match mx
    [(some x) (f x)]
    [mx mx]))

