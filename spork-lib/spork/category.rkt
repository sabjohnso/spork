#lang racket

(provide
 gen:category
 (contract-out
  [id? predicate/c]
  [id id?]
  [category? predicate/c]
  [category-compose (->* () () #:rest (listof category?) category?)]
  [category-id (-> category? category?)]
  [>> (->* () () #:rest (listof category?) category?)]
  [<< (->* () () #:rest (listof category?) category?)]))

(require
 racket/generic spork/tag spork/function-extras)

(define-tag id)

(define (id-compose f g) id)

(define-generics category
  (compose-proc category)
  (id-value category)
  #:fast-defaults
  ([function?
    (define (compose-proc function) compose)
    (define (id-value function) identity)]
   [id?
    (define (compose-proc _) id-compose)
    (define (id-value _) id)]))

(define (category-compose-2 f g)
  (match* (f g)
    [((? id?) (? id?)) id]
    [(f (? id?)) f]
    [((? id?) g) g]
    [(f g) ((compose-proc f) f g)]))

(define (category-id f)
  (id-value f))

(define (category-compose . fs)
  (cond [(= (length fs) 2) (category-compose-2 (car fs) (cadr fs))]
        [(> (length fs) 2) (apply category-compose
                                  (category-compose-2 (car fs) (cadr fs))
                                  (cddr fs))]
        [(= (length fs) 1) (car fs)]
        [(null? fs) (id)]))

(define (<< . fs)
  (apply category-compose fs))

(define (>> . xs)
  (apply category-compose (reverse xs)))
