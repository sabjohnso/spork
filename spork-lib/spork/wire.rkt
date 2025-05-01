#lang racket

(provide
 enum/wire)

(require
 (for-syntax racket racket/syntax syntax/parse)
 spork/wire-formats)

;;
;; ... Enumerated Types
;;

(begin-for-syntax

 (define-syntax-class value-spec
   #:attributes (name value)
   [pattern [name:id value]]
   [pattern [name:id] #:with value #'#f])

 (define (enum-visible-names type-name)
   (with-syntax ([type-name?       (format-id type-name "~a?"            type-name)]
                 [type-name-value? (format-id type-name "~a-value?"      type-name)]
                 [name->value      (format-id type-name "~a-name->value" type-name)]
                 [value->name      (format-id type-name "~a-value->name" type-name)])
     (syntax/loc type-name
       (type-name? type-name-value? name->value value->name))))

 (define (make-name/value-pairs value-specs)
     (let loop ([specs (syntax-e value-specs)]
		[next-value 0]
		[accum '()])
       (if (null? specs) (reverse accum)
	 (syntax-parse (car specs)
	   [(name:id) (loop (cdr specs) (add1 next-value) (cons #`(name . #,next-value) accum))]
	   [(name:id value) (loop (cdr specs) (add1 (syntax->datum #'value)) (cons #'(name . value) accum))]))))

   (define (name/value-pairs->value/name-pairs name/value-pairs)
     (for/list ([name/value-pair (syntax-e name/value-pairs)])
       (syntax-parse name/value-pair [(name:id . value) #'(value . name)])))

   (define (name/value-pairs->enumerated-names name/value-pairs)
     (for/list ([name/value-pair (syntax-e name/value-pairs)])
       (syntax-parse name/value-pair [(name:id . value) #'name])))

   (define (name/value-pairs->enumerated-values name/value-pairs)
     (for/list ([name/value-pair (syntax-e name/value-pairs)])
       (syntax-parse name/value-pair [(name:id . value) #'value]))))

(define-syntax (enum/wire stx)
  (syntax-parse stx
    [(enum/wire type-name:id value-type
       (value-specs:value-spec ...+))
     (with-syntax* ([(type-name? type-name-value? name->value value->name)
                     (enum-visible-names #'type-name)]
                    [(name-set value-set name->value/table value->name/table arg)
                     (generate-temporaries '(name-set value-set name->value/table value->name-table arg))]
                    [(name/value-pairs  ...) (make-name/value-pairs #'(value-specs ...))]
                    [(value/name-pairs  ...) (name/value-pairs->value/name-pairs  #'(name/value-pairs ...))]
                    [(enumerated-names  ...) (name/value-pairs->enumerated-names  #'(name/value-pairs ...))]
                    [(enumerated-values ...) (name/value-pairs->enumerated-values #'(name/value-pairs ...))])

       (syntax/loc stx
        (begin
          (define type-name (enum-type value-type '(name/value-pairs ...)))

          (define name-set  (set 'enumerated-names ...))

         (define value-set (set  enumerated-values ...))

         (define name->value/table (make-immutable-hash '(name/value-pairs ...)))

         (define value->name/table (make-immutable-hash '(value/name-pairs ...)))

         (define (type-name? arg)
           (set-member? name-set arg))

         (define (type-name-value? arg)
           (set-member? value-set arg))

         (define/contract (name->value arg)
           (-> type-name? type-name-value?)
           (hash-ref name->value/table arg))

         (define/contract (value->name arg)
           (-> type-name-value? type-name?)
           (hash-ref value->name/table arg))

         (define enumerated-names 'enumerated-names) ...)))]))
