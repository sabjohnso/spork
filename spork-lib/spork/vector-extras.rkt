#lang racket

(provide
 (contract-out
  [vector-return (-> any/c vector?)]
  [vector-fmap (-> (-> any/c any/c) vector? vector?)]
  [vector-fapply (-> (vectorof (-> any/c any/c)) vector? vector?)]
  [vector-flatmap (-> (-> any/c vector?) vector? vector?)]
  [vector-join (-> (vectorof vector?) vector?)]))

(define (vector-return x)
  (vector x))

(define (vector-fmap f xs)
  (vector-map f xs))

(define (vector-fapply fs xs)
  (for*/vector ([f fs]
                [x xs])
    (f x)))

(define (vector-flatmap f xs)
  (apply vector-append
         (for/list ([x xs])
           (f x))))

(define (vector-join xss)
  (apply vector-append (vector->list xss)))


