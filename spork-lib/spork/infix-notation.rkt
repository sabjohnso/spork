#lang racket

(provide (rename-out [app #%app]))
(require
 (for-syntax racket racket/syntax syntax/parse)
 spork/curried)

(begin-for-syntax
 (define-syntax-class infix-operator
   #:literals (quasiquote)
   [pattern (quasiquote name:id)])

 (define (uniform-operator? head-operator tail-operator)
   (or (not (syntax->datum tail-operator))
       (eq? (syntax->datum head-operator)
	    (syntax->datum tail-operator))))

 (define-splicing-syntax-class infix-head
   #:attributes (term operator)
   [pattern (~seq term:expr op:infix-operator)
            #:with operator #'op.name])

 (define-splicing-syntax-class infix-tail
   #:attributes (operator [terms 1])
   [pattern (~seq term:expr op:infix-operator tail:infix-tail)
            #:when (uniform-operator? #'op.name #'tail.operator)
            #:with operator #'op.name
            #:with (terms ...) #'(term tail.terms ...)]
   [pattern (~seq term:expr)
            #:with operator #f
            #:with (terms ...) #'(term)])

 (define-splicing-syntax-class broken-infix-tail
   #:attributes (operator [terms 1])
   [pattern (~seq op:infix-operator term:expr  tail:broken-infix-tail)
            #:with operator #'op.name
            #:with (terms ...) #'(term tail.terms ...)
            #:when (uniform-operator? #'op.name #'tail.operator)]
   [pattern (~seq op:infix-operator)
            #:with operator #'op.name
            #:with (terms ...) #'()]))

(define-syntax (app stx)
  (syntax-parse stx
    #:literals (quasiquote)
    [(_ head:infix-head tail:infix-tail)
     #:when (uniform-operator? #'head.operator #'tail.operator)
     (syntax/loc stx
       (#%app head.operator head.term tail.terms ...))]

    [(_ op:infix-operator tail:infix-tail)
     #:when (uniform-operator? #'op.name #'tail.operator)
     (with-syntax ([arg (generate-temporary 'arg)])
       (syntax/loc stx
	 (λ (arg) (op.name arg tail.terms ...))))]

    [(_ term:expr tail:broken-infix-tail)
     (with-syntax ([arg (generate-temporary 'arg)])
       (syntax/loc stx
	 (λ (arg) (tail.operator term tail.terms ... arg))))]

    [(_ head:infix-head tail:expr ...+)
     (bad-infix-expression stx #'head #'(tail ...))]

    [(_ fun:expr args ...)
     (syntax/loc stx
       (#%app fun args ...))]))


(begin-for-syntax
 (define (bad-infix-expression stx head tail)
   (raise-syntax-error
    'inhomogeneous-infix-operators
    (format bad-infix-fmt (operators head tail))
    stx
    #f
    (list stx)))

 (define (operators head tail)
   (cons (syntax->datum (head-operator head))
         (map syntax->datum (tail-operators tail))))

 (define (head-operator head)
   (syntax-parse head
     [(term:expr operator)
      (strip-quasiquote #'operator)]))

 (define (tail-operators tail)
   (for/list ([i (in-naturals)]
              [e (syntax-e tail)]
              #:when (odd? i))
     (strip-quasiquote e)))

 (define (strip-quasiquote stx)
   (syntax-parse stx
     #:datum-literals (quasiquote)
     [(quasiquote operator:expr) #'operator]
     [stx #'stx]))

 (define bad-infix-fmt
   "
To prevent the ambiguity that frequently
accompanies infix notation, the infix operator
must be homogeneous in an infix expression.
However, the following inhomogeneous set of
operators was encountered: ~a"))
