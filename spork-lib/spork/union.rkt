#lang racket

(provide union)
(require
 (for-syntax racket racket/syntax syntax/parse))

(define-syntax (union stx)
  (syntax-parse stx
    [(_ type-name:id
        (constructor:id field-name:id  ...) ...
        union-options ...)
     (syntax/loc stx
       (begin
         (struct type-name
           ()
           #:transparent
           union-options ...)
         (struct constructor
           type-name
           (field-name ...)
           #:transparent)
         ...))]))
