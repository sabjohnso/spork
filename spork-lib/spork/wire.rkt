#lang racket

(provide
 enum/wire
 integer/wire)

(require
 (for-syntax racket racket/syntax syntax/parse)
 spork/wire-formats)

;;
;; ... enumerated types
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


;;
;;  ... integer types
;;

(begin-for-syntax

 (define-syntax-class endianness
   #:attributes (value)
   #:datum-literals (little big)
   [pattern little
     #:with value 'little]
   [pattern big
     #:with value 'big])

 (define-splicing-syntax-class integer-options
   [pattern
        (~seq (~or (~optional (~seq #:signed? signedness:boolean))
                   (~optional (~seq #:byte-count byte-count:nat))
                   (~optional (~seq #:bits-per-byte bits-per-byte:nat))
                   (~optional (~seq #:byte-order byte-order:endianness))
                   (~optional (~seq #:bit-order bit-order:endianness)))
              ...)]))

(define-syntax (integer/wire stx)
  (syntax-parse stx
    [(_ name:id options:integer-options)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [pred-hidden? (generate-temporary 'pred?)]
                   [signedness (if (attribute options.signedness) #'options.signedness #f)]
                   [byte-count (if (attribute options.byte-count) #'options.byte-count 4)]
                   [bits-per-byte (if (attribute options.bits-per-byte) #'options.bits-per-byte 8)]
                   [byte-order (if (attribute options.byte-order) #'options.byte-order 'little)]
                   [bit-order (if (attribute options.bit-order) #'options.bit-order 'little)])
      (syntax/loc stx
        (begin
          (define name (fixed-integer byte-count bits-per-byte signedness byte-order bit-order))
          (define pred-hidden? (fixed-integer-predicate name))
          (define (name? arg)
            (pred-hidden? arg)))))]))
