#lang racket

(provide
 (contract-out
  [stream-return (-> any/c stream?)]
  [stream-fmap (-> (-> any/c any/c) stream? stream?)]
  [stream-fapply (-> (stream/c (-> any/c any/c)) stream? stream?)]
  [stream-flatmap (-> (-> any/c stream?) stream? stream?)]
  [stream-join (-> (stream/c stream?) stream?)]
  [nonempty-stream? predicate/c]
  [nonempty-stream-duplicate (-> nonempty-stream? (and/c nonempty-stream? (stream/c nonempty-stream?)))]
  [nonempty-stream-extend (-> (-> nonempty-stream? any/c) nonempty-stream? nonempty-stream?)]))

(define (stream-return x)
  (stream x))

(define (stream-fmap f xs)
  (stream-map f xs))

(define (stream-fapply fs xs)
  (define (recur fs)
    (if (stream-empty? fs) empty-stream
      (stream-append (stream-map (stream-first fs) xs)
                     (recur (stream-rest fs)))))
  (recur fs))

(define (stream-flatmap f xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-append (f (stream-first xs)) (recur (stream-rest xs)))))
  (recur xs))

(define (stream-join xss)
  (if (stream-empty? xss) empty-stream
    (stream-append (stream-first xss) (stream-join (stream-rest xss)))))

(define (nonempty-stream? x)
  (and (stream? x) (not (stream-empty? x))))

(define (nonempty-stream-duplicate xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-cons xs (recur (stream-rest xs)))))
  (recur xs))

(define (nonempty-stream-extend f xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-cons (f xs) (recur (stream-rest xs)))))
  (recur xs))
