#lang racket

(provide
 (contract-out
  [expect? predicate/c]
  [expected? (-> expect? boolean?)]
  [unexpected? (-> expect? boolean?)]
  [expected (-> any/c expect?)]
  [unexpected (-> any/c expect?)]))

(require spork/union spork/functor)

(union expect
  (expected value)
  (unexpected message)
  #:methods gen:monad
  ((define (return-proc this) expected)
   (define (flatmap-proc this) expect-flatmap)))

(define (expect-flatmap f mx)
  (match mx
    [(expected x) (f x)]
    [mx mx]))
