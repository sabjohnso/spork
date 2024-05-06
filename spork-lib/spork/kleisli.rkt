#lang racket

(provide
 (contract-out
  (struct kleisli ([ctor (-> any/c monad?)]))
  [kleisli-run (-> kleisli? any/c monad?)]))

(require spork/functor)

(struct kleisli
  (ctor))

(define (kleisli-run kf x)
  ((kleisli-ctor kf) x))
