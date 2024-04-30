#lang racket

(provide
 (contract-out
  [list-return (-> any/c list?)]
  [rappend (-> list? list? list?)]
  [list-flatmap (-> (-> any/c list?) list? list?)]))

(define (rappend xs ys)
  (if (null? xs) ys
    (rappend (cdr xs) (cons (car xs) ys))))

(define (list-flatmap f xs)
  (define (recur xs accum)
    (if (null? xs) (reverse accum)
      (recur (cdr xs) (rappend (f (car xs)) accum))))
  (recur xs '()))

(define (list-return x) (list x))
