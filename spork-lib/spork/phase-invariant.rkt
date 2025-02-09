#lang racket

(provide
 define/phase-invariant)

(require
 (for-syntax racket racket/syntax syntax/parse))

(define-syntax (define/phase-invariant stx)
  (syntax-parse stx
    [(_ name:id value:expr)
     (syntax/loc stx
       (begin
         (define-for-syntax name value)
         (define name value)))]

    [(_ (name:id args ...) body:expr ...+)
     (syntax/loc stx
       (begin
         (define-for-syntax (name args ...) body ...)
         (define (name args ...) body ...)))]))
