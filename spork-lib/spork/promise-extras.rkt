#lang racket

(provide
 (contract-out
  [promise-wrap (-> any/c promise?)]
  [promise-unwrap (-> promise? any/c)]))


(define (promise-wrap x)
  (delay x))

(define (promise-unwrap mx)
  (force mx))
