#lang racket

(provide
 (struct-out some)
 (struct-out none)
 (contract-out
  [optional? predicate/c]))

(require spork/union spork/functor)

(union optional
  (none)
  (some value)
  #:methods gen:monad
  ((define (return-proc this) some)
   (define (flatmap-proc this) optional-flatmap))

  #:methods gen:monad-zero
  ((define (monad-zero-value this) (none))))

(define (optional-flatmap f mx)
  (match mx
    [(some x) (f x)]
    [mx mx]))
