#lang racket

(provide (all-from-out (submod "." enum-details)))

(module enum-details racket
  (provide enum)
  (require (for-syntax racket racket/syntax syntax/parse))

  (begin-for-syntax

   (define-syntax-class value-spec
     #:attributes (name value)
     [pattern [name:id value]]
     [pattern [name:id] #:with value #'#f])


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
       (syntax-parse name/value-pair [(name:id . value) #'value])))

   #| end of begin-for-syntax |#)

  (define-syntax (enum stx)
    (syntax-parse stx
      [(_ type-name (value-specs:value-spec ...+))
       (with-syntax* ([type-name? (format-id #'type-name "~a?" #'type-name)]
		      [type-name-value? (format-id #'type-name "~a-value?" #'type-name)]
		      [(name/value-pairs ...) (make-name/value-pairs #'(value-specs ...))]
		      [(value/name-pairs ...) (name/value-pairs->value/name-pairs #'(name/value-pairs ...))]
		      [(enumerated-names ...) (name/value-pairs->enumerated-names #'(name/value-pairs ...))]
		      [(enumerated-values ...) (name/value-pairs->enumerated-values #'(name/value-pairs ...))]
		      [(name-set value-set name->value/table value->name-table)
		       (generate-temporaries '(name-set value-set name->value/table value->name-table))]
		      [name->value (format-id #'type-name "~a-name->value" #'type-name)]
		      [value->name (format-id #'type-name "~a-value->name" #'type-name)])
	 (syntax/loc stx
	   (begin
	     (define name-set (set 'enumerated-names ...))
	     (define value-set (set enumerated-values ...))
	     (define name->value/table (make-immutable-hash '(name/value-pairs ...)))
	     (define value->name/table (make-immutable-hash '(value/name-pairs ...)))
	     (define (type-name? x) (set-member? name-set x))
	     (define (type-name-value? x) (set-member? value-set x))
	     (define/contract (name->value x)
	       (-> type-name? type-name-value?)
	       (hash-ref name->value/table x))
	     (define/contract (value->name x)
	       (-> type-name-value? type-name?)
	       (hash-ref value->name/table x))
	     (define enumerated-names 'enumerated-names) ...)))]))

  #| end  of module enum-details |#)


(require (submod "." enum-details))
#;
(module+ test
  (require rackunit rackunit/spec)
  (describe "enum, a macro for defining enumerated value types"
    (context "with a type for enumerated values a b and c"
      (enum abc
	([a]
	 [b]
	 [c]))
      (it "binds the names to the corresponding symbols"
	(check-equal? a 'a)
	(check-equal? b 'b)
	(check-equal? c 'c))
      (describe "abc?"
	(it "is a predicate that recognizes 'a 'b and 'c"
	  (check-true (abc? 'a))
	  (check-true (abc? 'b))
	  (check-true (abc? 'c)))
	(it "does not recognize other values"
	  (check-false (abc? 'x))
	  (check-false (abc? 7))
	  (check-false (abc? "a purple cow"))))
      (describe "abc-value?"
	(it "is a predicate that recognizes the values associated with 'a 'b and 'c"
	  (check-true (abc-value? 0))
	  (check-true (abc-value? 1))
	  (check-true (abc-value? 2)))
	(it "does NOT recognize the symbols or other values"
	  (check-false (abc-value? 'a))
	  (check-false (abc-value? 'b))
	  (check-false (abc-value? 'c))
	  (check-false (abc-value? 3))))
      (describe "abc-name->value"
	(it "converts an abc name to an abc value"
	  (check-equal? (abc-name->value 'a) 0)
	  (check-equal? (abc-name->value 'b) 1)
	  (check-equal? (abc-name->value 'c) 2))
	(it "fails on other inputs"
	  (check-exn exn:fail? (thunk (abc-name->value 'x)))))
      (describe "abc-value->name"
	(it "converts an abc value to an abc name"
	  (check-equal? (abc-value->name 0) 'a)
	  (check-equal? (abc-value->name 1) 'b)
	  (check-equal? (abc-value->name 2) 'c))
	(it "fails on other inputs"
	  (check-exn exn:fail? (thunk (abc-value->name 3))))))
    (context "with a type for enumerated values one and two"
      (enum thing
	([one 1]
	 [two]))
      (describe "thunk-name->value"
	(it "was expicitly defined for one"
	  (check-equal? (thing-name->value 'one) 1))
	(it "was implicitly define for two"
	  (check-equal? (thing-name->value 'two) 2))))))
