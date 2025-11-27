#lang racket

(provide
 gen:contravariant
 (contract-out
  [contramap (-> function? contravariant? contravariant?)]))

(require
 racket/generic
 spork/function-extras)

(define-generics contravariant
  (contramap func contravariant)
  #:fast-defaults
  ([function?
    (define (contramap func function)
      (compose function func))]))
