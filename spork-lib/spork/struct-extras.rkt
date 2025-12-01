#lang racket

(provide
 (for-syntax field-decl
             field-options
             struct-options))

(require
 (for-syntax syntax/parse))

(begin-for-syntax
 (define-syntax-class field-decl
   [pattern (field-name:id options:field-options)]
   [pattern field-name:id])

 (define-splicing-syntax-class field-options
   [pattern
    (~seq (~or (~once (~optional #:mutable))
               (~once (~optional #:auto))) ...)])

 (define-splicing-syntax-class struct-options
   [pattern
    (~seq (~or (~once (~optional #:mutable))
               (~once (~optional (~seq #:super super:expr)))
               (~once (~optional (~seq #:auto-value auto:expr)))
               (~once (~optional (~seq #:guard guard:expr)))
               (~optional (~seq #:property property:expr val:expr))
               (~optional (~seq #:properties property-list:expr))
               (~once (~optional #:transparent))
               (~once (~optional #:prefab))
               (~once (~optional #:sealed))
               (~once (~optional #:authentic))
               (~once (~optional (~seq #:name name:id)))
               (~once (~optional (~seq #:extra-name extra-name:id)))
               (~once (~optional (~seq #:constructor-name constructor-name:id)))
               (~once (~optional (~seq #:extra-constructor-name extra-constructor-name:id)))
               (~once (~optional (~seq #:reflection-name reflection-name:expr)))
               (~optional (~seq #:methods generics-name:id method-defs:expr))
               (~once (~optional #:omit-define-syntaxes))
               (~once (~optional #:omit-define-values))) ...)]))
