#lang racket

(provide
 (contract-out
  [future-wrap (-> any/c future?)]
  [future-unwrap (-> future? any/c)]))

(define (future-wrap x)
  (future (thunk x)))

(define (future-unwrap mx)
  (force mx))
