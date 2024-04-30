#lang racket

(provide
 lambda-curried define-curried curried-> define-curried/contract
 (rename-out [lambda-curried λ-curried]))

(require
 (for-syntax racket racket/syntax syntax/parse))

(define-syntax (lambda-curried stx)
  (syntax-parse stx

    ;; This rule matches formals that are a list of one or
    ;; more identifiers. These are the only functions that
    ;; with be curried.
    [(_ (x:id xs:id ...) e es ...)
     (with-syntax ([f  (generate-temporary 'f)]
		   [ys (generate-temporary 'ys)])
       (syntax/loc stx
	 ((λ (f) (f f))
	  (λ (f)
	    (case-lambda
	      [(x xs ...) e es ...]
	      [(x xs ... . ys) (apply ((f f) x xs ...) ys)]
	      [(x) (lambda-curried (xs ...) e es ...)]
	      [(x . ys) (apply ((f f) x) ys)]
	      [() (f f)])))))]

    ;; This rule doesn't constrain the formals to be identifiers
    ;; and will match formals that have optional parameters,
    ;; keyword parameters or rest parameters. The function returned
    ;; will not be curried.
    ;;
    ;; This rule is included to allow the syntax to be used as a
    ;; drop-in replacement for `lambda`
    [(_ formals e es ...)
     (syntax/loc stx
       (λ formals e es ...))]))

(define-syntax (define-curried stx)
  (syntax-parse stx
    ;; This rule matches a generator style of definition for
    ;; functions where the formals are all identifiers. These
    ;; functions will be curried.
    [(_ (f x:id xs:id ...) e es ...)
     (syntax/loc stx
       (define f (lambda-curried (x xs ...) e es ...)))]

    ;; This rule matches generator style definitions that are
    ;; not matched by the first rule.  These functions will
    ;; not be curried.  The rule is included to allow define-curried
    ;; to work as a drop-in replacement for define.
    [(_ (f xs ...) e es ...)
     (syntax/loc stx
       (define (f xs ...) e es ...))]

    ;; This rule bind an identifier to an expression, where
    ;; the expression may or may not yield a function.  This
    ;; rule is included to allow `define-curried` to be used
    ;; as a drop-in replacement for `define`
    [(_ x e)
     (syntax/loc stx
       (define x e))]))

(begin-for-syntax
 (define (make-curried-cases doms rng)
   (reverse
    (let* ([doms (syntax-e doms)])
      (if (= 1 (length doms))
	  (with-syntax ([dom (car doms)]
			[rng rng])
	    (list (syntax (-> dom rng))))
	(for/list ([i (in-range 0 (length doms))])
	  (with-syntax ([(left-doms ...) (take doms i)]
			[(right-doms ...) (drop doms i)]
			[rng rng])
	    (if (zero? i)
		(syntax (-> left-doms ... right-doms ... rng))
	      (syntax (-> left-doms ... (curried-> right-doms ... rng)))))))))))

(define-syntax (curried-> stx)
  (syntax-parse stx
    #:literals (curried->)
    ;; This rule is for a thunk and will not be a curried contract.
    ;; It is included to make `curried->` a drop-in replacement for
    ;; `->`
    [(_ rng)  (syntax/loc stx (-> rng))]


    ;; This rule is for contracts where the range is also a
    ;; curried function. The range is expanded into the
    ;; the curried function contract.
    [(_ left-doms ...+ (curried-> right-doms ...+ rng))
     (syntax/loc stx (curried-> left-doms ... right-doms ... rng))]

    [(_ dom rng) (syntax/loc stx (-> dom rng))]

    ;; This rule is for curried function contracts. The list of application
    ;; cases is spliced into a `case->` contract
    [(_ doms ...+ rng)
     (with-syntax ([(curried-cases ...) (make-curried-cases #'(doms ...) #'rng)])
       (syntax/loc stx
	 (case-> curried-cases ...)))]))

(define-syntax (define-curried/contract stx)
  (syntax-parse stx
    [(_ (f:id x:id xs:id ...) ctc e es ...)
     (syntax/loc stx
       (define/contract f ctc
	 (lambda-curried (x xs ...) e es ...)))]

    [(_ (f:id xs ...) ctc e es ...)
     (syntax/loc stx
       (define/contract f ctc
	 (lambda-curried (xs ...) e es ...)))]

    [(_ x ctc e)
     (syntax/loc stx
       (define/contract x ctc e))]))
