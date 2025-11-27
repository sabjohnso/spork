#lang racket

(provide
 (contract-out
  [future-wrap (-> any/c future?)]
  [future-unwrap (-> future? any/c)])
 future/thunk)

(require (for-syntax racket racket/syntax syntax/parse))

(define (future-wrap x)
  (future (thunk x)))

(define (future-unwrap mx)
  (touch mx))


(define-syntax (future/thunk stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx
       (future (thunk e)))]))
