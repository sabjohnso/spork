#lang racket

(provide
 (contract-out
  [sequence-return (-> any/c sequence?)]
  [sequence-fmap (-> (-> any/c any/c) sequence? sequence?)]
  [sequence-fapply (-> (sequence/c (-> any/c any/c)) sequence? sequence?)]
  [sequence-flatmap (-> (-> any/c sequence?) sequence? sequence?)]
  [sequence-join (-> (sequence/c sequence?) sequence?)]))

(define (sequence-return x)
  (list x))

(define (sequence-fmap f xs)
  (sequence-map f xs))

(define (sequence-fapply fs xs)
  (in-stream
   (for*/stream ([f fs]
                 [x xs])
     (f x))))

(define (sequence-join xss)
  (define (recur xss)
    (if (stream-empty? xss) empty-stream
      (stream-append (for/stream ([x (stream-first xss)]) x)
                     (recur (stream-rest xss)))))
  (in-stream (recur xss)))

(define (sequence-flatmap f xs)
  (sequence-join
   (for/stream ([x xs])
     (f x))))

