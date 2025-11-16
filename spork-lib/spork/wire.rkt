#lang errortrace racket

(provide
 enum/wire
 integer/wire
 array/wire
 struct/wire)

(require
 (for-syntax spork/wire-formats spork/list-extras racket racket/syntax syntax/parse )
 spork/wire-formats )

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
       (~seq (~or (~optional (~seq #:signed? signedness:boolean)
                             #:defaults ([signedness #'#f])
                             #:name "#:signed keyword and value")
                   (~optional (~seq #:byte-count byte-count:nat)
                              #:defaults ([byte-count #'4])
                              #:name "#:byte-count keyword and value")
                   (~optional (~seq #:bits-per-byte bits-per-byte:nat)
                              #:defaults ([bits-per-byte #'8])
                              #:name "#:bits-per-byte keyword and value")
                   (~optional (~seq #:byte-order byte-order:endianness)
                              #:defaults ([byte-order #'little])
                              #:name "#:byte-order keyword and endian value")
                   (~optional (~seq #:bit-order bit-order:endianness)
                              #:defaults ([bit-order #'little])
                              #:name "#:bit-order keyword and endian value"))
              ...)]))

(define-syntax (integer/wire stx)
  (syntax-parse stx
    [(_ name:id options:integer-options)
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [pred-hidden? (generate-temporary 'pred?)])
      (syntax/loc stx
        (begin
                    (define name
            (fixed-integer
                  options.byte-count
                  options.bits-per-byte
                  options.signedness
                  options.byte-order
                  options.bit-order))
          #;
          (define-syntax (name s)
            (syntax-parse s
              [(_)
               (syntax/loc s
                 (fixed-integer
                  options.byte-count
                  options.bits-per-byte
                  options.signedness
                  options.byte-order
                  options.bit-order))]
              [_
               (syntax/loc s
                 (fixed-integer
                  options.byte-count
                  options.bits-per-byte
                  options.signedness
                  options.byte-order
                  options.bit-order))]))

          (define pred-hidden?
            (fixed-integer-predicate name))

          (define (name? arg)
            (pred-hidden? arg)))))]))


(begin-for-syntax
 (define (make-fixed-array-identifiers name)
   (list
    (format-id name "~a?" name)
    (format-id name "make-~a" name)
    (format-id name "~a-ref" name)
    (format-id name "~a-set" name)
    (format-id name "list->~a" name)
    (format-id name "~a->list" name))))




(define-syntax (array/wire stx)
  (syntax-parse stx
    [(_ name:id element-type:id element-count:nat)
     (with-syntax ([(name? make-name name-ref name-set list->name name->list)
                    (make-fixed-array-identifiers #'name)]
                   [(hidden-element-writer  hidden-element-reader  hidden-pred?  hidden-constructor s)
                    (generate-temporaries '(hidden-element-writer  hidden-element-reader  hidden-pred?  hidden-constructor s))])
       (syntax/loc stx
         (begin
           (define name (fixed-array element-type element-count))
           #;
           (define-syntax (name s)
             (syntax-parse s
               [(name) #'name]
               [_ (syntax/loc s
                    (fixed-array element-type element-count))]))

           ;; Hidden functions
           (define hidden-constructor (fixed-array-bits-constructor name))
           (define hidden-pred? (fixed-array-predicate name))
           (define hidden-element-writer (fixed-array-element-writer name))
           (define hidden-element-reader (fixed-array-element-reader name))

           ;; Visible functions
           (define (make-name . args)
             (apply hidden-constructor args))

           (define (name? arg)
             (hidden-pred? arg))

           (define (name-ref array-bits index)
             (hidden-element-reader array-bits index))

           (define (name-set array-bits index input)
             (hidden-element-writer array-bits index input))

           (define (list->name list)
             (apply hidden-constructor list))

           (define (name->list array-bits)
             (for/list ([index (in-range (fixed-array-length name))])
               (name-ref array-bits index))))))]))


;;
;; ... Record Types
;;

(begin-for-syntax
 ;; The record component may be either fields
 ;; or supers, where they are discriminated by
 ;; the presence keyword `#:super` for supers.

 (define-syntax-class  record-component
   #:attributes (name type super?)
   #:datum-literals (:)

   [pattern (name:id : type)
     #:declare type (expr/c #'fixed-size-type?)
     #:with super? #f]

   [pattern (name:id : type #:splice)
     #:declare type (expr/c #'fixed-record?)
     #:with super? #t])

 ;; Transform the input syntax into the fixed-record form.
 ;; (-> syntax? syntax?)
 (define (make-fixed-record-type-syntax stx)
   (syntax-parse stx
     [(_ _ (components:expr ...))
      (with-syntax ([(components ...) (transform-record-components #'(components ...))])
        (syntax/loc stx
          (fixed-record
           (list components ...))))]))


 ;; Transform each component of the input syntax into
 ;; fixed record component syntax
 ;; (-> (listof syntax?) (listof syntax?))
 (define (transform-record-components components)
   (map transform-record-component (syntax-e components)))


 (define (transform-record-component component)
   (syntax-parse component
     #:datum-literals (:)
     [(field-name : field-type)
      (syntax/loc component
        (fixed-record-field 'field-name field-type))]

     [(super-name : super-type #:splice)
      (syntax/loc component
        (fixed-record-super 'super-name super-type))]))

 (define (fixed-record-syntax? arg)
   (and (syntax? arg)
        (syntax-parse arg
          #:literals (fixed-record list)
          [(fixed-recrod (list components:expr ...)) #t]
          [_ #f])))

 (define (extract-field-names components)
   (let loop ([components (syntax-e components)]
              [accum '()])
     (if (null? components) (reverse accum)
       (let ([component (car components)])
         (syntax-parse component
           #:datum-literals (:)
           #:literals (quote)
           [(introducer:id (quote name:id) type:id)
            (match (syntax->datum #'introducer)
              ['fixed-record-field (loop (cdr components) (cons #'name accum))]
              ['fixed-record-super
               (define super-type (local-expand #'type 'top-level '()))
               (syntax-parse super-type
                 [(fixed-record (list super-components ...))
                  (loop (cdr components)
                         (rappend
                          (extract-field-names #'(super-components ...))
                          accum))]
                 [super-name:id
                  (raise-syntax-error
                   'no-binding
                   (format
                       "could not find a binding for super type: ~a"
                     (syntax->datum #'super-name)))])])]))))))


(define-syntax (struct/wire stx)
  (syntax-parse stx
    [(_ name:id
        (component:record-component ...))
     (with-syntax* ([(record-component ...) (transform-record-components #'(component ...))]
                    [constructor (format-id #'name "make-~a" #'name)]
                    [name? (format-id #'name "~a?" #'name)]
                    [(field-name ...) (extract-field-names #'(record-component ...))]
                    [(field-reader ...)
                     (map (λ (field-name) (format-id field-name "~a-~a" #'name field-name))
                          (syntax-e #'(field-name ...)))]
                    [(field-writer ...)
                     (map (λ (field-name) (format-id field-name "set-~a-~a" #'name field-name))
                          (syntax-e #'(field-name ...)))]
                    [(hidden-field-reader ...)
                     (generate-temporaries #'(field-name ...))]
                    [(hidden-field-writer ...)
                     (generate-temporaries #'(field-name ...))]
                    [(hidden-constructor hidden-predicate arg)
                     (generate-temporaries
                      '(hidden-constructor hidden-predicate arg))])
       (displayln "-----------------------------")
       (displayln #'(record-component ...))
       (displayln #'(field-name ...))
       (syntax/loc stx
         (begin

           (define name
             (fixed-record (list record-component ...)))
           #;
           (define-syntax (name s)
             (syntax-parse s
               [_ (fixed-record
                   (list record-component ...))]))

           (define hidden-predicate (fixed-record-predicate name))

           (define (name? arg)
             (hidden-predicate arg))

           (define hidden-constructor (fixed-record-bits-assoc-constructor name))
           (define (constructor field-name ...)
             (hidden-constructor
              (list (cons 'field-name field-name) ...)))


           (define hidden-field-reader (fixed-record-bits-field-reader name 'field-name)) ...

           (define (field-reader name) (hidden-field-reader name)) ...

           (define hidden-field-writer (fixed-record-bits-field-writer name 'field-name)) ...

           (define (field-writer name) (hidden-field-writer name)) ...)))]))
