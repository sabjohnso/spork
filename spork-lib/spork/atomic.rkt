#lang racket

(provide
 (contract-out
  [atomically (-> (-> any/c) any/c)]))

(require ffi/unsafe/atomic)

(define (atomically thunk)
  (start-atomic)
  (let ([results ((compose list thunk))])
    (end-atomic)
    (apply values results)))
