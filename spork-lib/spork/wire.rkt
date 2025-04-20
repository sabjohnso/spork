#lang racket

(provide)

(require
 (for-syntax racket racket/syntax syntax/parse)
 spork/wire-formats)

(begin-for-syntax
 (define-syntax-class record-component
   #:datum-literals (:)
   #:attributes (name type super?)
   (pattern (name:id : type:expr)
     #:with super #f)
   (pattern (name:id : type:expr #:super)
     #:with super #t)))

(define-syntax (struct/wire stx)
  (syntax-parse stx
    [(_ type-name:id
        (component:record-component ...))]))
