#lang racket

(provide
 gen:monoid
 (contract-out
    [monoid? predicate/c]
    [mappend (-> monoid? monoid? monoid?)]
    [++ (->* () () #:rest (listof monoid?) monoid?)]
    [id/monoid monoid?]))

(require spork/group spork/curried racket/generic)

(define-generics monoid
  (monoid-proc monoid)
  (monoid-identity monoid)
  #:fast-defaults
  ([list?
    (define (monoid-proc list) append)
    (define (monoid-identity list) '())]
   [string?
    (define (monoid-proc string) string-append)
    (define (monoid-identity string) "")]
   [vector?
    (define (monoid-proc vector) vector-append)
    (define (monoid-identity vector) #())]
   [group?
    (define closed-associataive-proc-with-identity group-operator)
    (define monoid-identity group-identity)]))

(define (mappend arg1 arg2)
  (match* (arg1 arg2)
    [((unresolved-identity) (unresolved-identity)) (unresolved-identity)]
    [((unresolved-identity) y) y]
    [(x (unresolved-identity)) x]
    [(x y) ((monoid-proc x) x y)]))

(define (++ . args)
  (cond [(= 2 (length args)) (mappend (car args) (cadr args))]
        [(= 1 (length args)) (car args)]
        [(= 0 (length args)) (unresolved-identity)]
        [else (apply ++ (++ (car args) (cadr args)) (cddr args))]))

(define id/monoid id/group)
