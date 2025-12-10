#lang racket

(provide
 gen:group
 (contract-out
  [group? predicate/c]
  [group-operator (-> group? group? group?)]
  [<> (->* () () #:rest (listof group?) group?)]
  [invert (-> group? group?)]
  [group-identity (-> group? group?)]
  [id/group group?]
  (struct unresolved-identity ())))

(require spork/curried racket/generic)

(struct unresolved-identity ())

(define (unresolved-identity-operator x y)
  (unresolved-identity))

(define (unresolved-identity-invert x)
  (unresolved-identity))

(define-generics group
  (group-proc group)
  (group-inverse-proc group)
  (group-identity group)

  #:fast-defaults
  ([unresolved-identity?
    (define (group-proc _) unresolved-identity-operator)
    (define (group-inverse-proc _) unresolved-identity-invert)
    (define (group-identity _) (unresolved-identity))]))

(define (invert x)
  ((group-inverse-proc x) x))

(define-curried (group-operator x y)
  (match* (x y)
    [((unresolved-identity) (unresolved-identity)) (unresolved-identity)]
    [((unresolved-identity) y) y]
    [(x (unresolved-identity)) x]
    [(x y) ((group-proc x) x y)]))

(define (<> . args)
  (cond [(= 2 (length args)) (group-operator (car args) (cadr args))]
        [(= 1 (length args)) (car args)]
        [(= 0 (length args)) (unresolved-identity)]
        [else (apply <> (<> (car args) (cadr args)) (cddr args))]))

(define id/group (unresolved-identity))
