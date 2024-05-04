#lang racket

(provide
 (contract-out
  [either? predicate/c]
  [struct left  ([value any/c])]
  [struct right ([value any/c])]
  [either-flatmap (-> (-> any/c either?) either? either?)]
  [either-swap (-> either? either?)]))

(require spork/union)

(union either
  (left value)
  (right value))

(define (either-flatmap f mx)
  (match mx
    [(left x) (f x)]
    [(right y) (right y)]))

(define (either-swap mx)
  (match mx
    [(left x) (right x)]
    [(right y) (left y)]))
