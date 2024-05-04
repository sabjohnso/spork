#lang racket

(provide
 (contract-out
  [either? predicate/c]
  [left? (-> either? boolean?)]
  [right? (-> either? boolean?)]
  [left (-> any/c either?)]
  [right (-> any/c either?)]))

(require spork/union spork/functor)

(union either
  (left value)
  (right value)
  #:methods gen:monad
  ((define (return-proc _) left)
   (define (flatmap-proc _) either-flatmap)))

(define (either-flatmap f mx)
  (match mx
    [(left x) (f x)]
    [(right y) (right y)]))
