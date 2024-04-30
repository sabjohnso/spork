#lang racket
;; Syntax for defining vacuous singletons

(provide (all-from-out (submod "." core)))

(module core racket
  (provide define-tag tag?)

  (require
   (for-syntax racket racket/syntax syntax/parse)
   racket/generic)

  (define-generics tag)

  (define-syntax define-tag
    (syntax-parser
     [(_ name:id)
      (with-syntax ([struct-name (format-id #'name "~a-type" #'name)])
        #'(begin
            (struct name
              ()
              #:constructor-name struct-name
              #:name struct-name
              #:reflection-name 'name
              #:transparent
              #:methods gen:custom-write
              ((define (write-proc this port mode)
                 (display 'name port)))
              #:methods gen:tag ())
            (define name (struct-name))))]
     [(_ name:id super:expr)
      (with-syntax ([struct-name (format-id #'name "~a-type" #'name)])
        #'(begin
            (struct name super
                    ()
                    #:constructor-name struct-name
                    #:name struct-name
                    #:reflection-name 'name
                    #:transparent
                    #:methods gen:custom-write
                    ((define (write-proc this port mode)
                       (display 'name port)))
                    #:methods gen:tag ())
            (define name (struct-name))))])))

(require (submod "." core))

(module+ test
  (require rackunit)
  (define-tag not-present)
  (check-true (not-present? not-present))
  (check-true (tag? not-present))
  (displayln not-present))
