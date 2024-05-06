#lang racket

(provide
 (contract-out
  [list-return (-> any/c list?)]
  [rappend (-> list? list? list?)]
  [list-fmap (-> (-> any/c any/c) list? list?)]
  [list-fapply (-> (listof (-> any/c any/c)) list? list?)]
  [list-flatmap (-> (-> any/c list?) list? list?)]
  [list-join (-> (listof list?) list?)]
  [nonempty-list? predicate/c]
  [nonempty-list-duplicate (-> nonempty-list? (listof nonempty-list?))]
  [nonempty-list-extend (-> (-> nonempty-list? any/c) nonempty-list? nonempty-list?)]))

(define (rappend xs ys)
  (if (null? xs) ys
    (rappend (cdr xs) (cons (car xs) ys))))

(define (list-fmap f xs)
  (map f xs))

(define (list-fapply fs xs)
  (for*/list ([f fs]
              [x xs])
    (f x)))

(define (list-flatmap f xs)
  (define (recur xs accum)
    (if (null? xs) (reverse accum)
      (recur (cdr xs) (rappend (f (car xs)) accum))))
  (recur xs '()))

(define (list-return x) (list x))

(define (list-join xss)
  (define (recur xss accum)
    (if (null? xss) (reverse accum)
      (recur (cdr xss) (rappend (car xss) accum))))
  (recur xss '()))

(define (nonempty-list? x)
  (and (list? x) (not (null? x))))

(define (nonempty-list-duplicate xs)
  (define (recur xs accum)
    (if (null? xs) (reverse accum)
      (recur (cdr xs) (cons xs accum))))
  (recur xs '()))

(define (nonempty-list-extend f xs)
  (define (recur xs accum)
    (if (null? xs) (reverse accum)
      (recur (cdr xs) (cons (f xs) accum))))
  (recur xs '()))
