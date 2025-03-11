#lang racket

(provide
 stream-iterate
 stream-window
 stream-return
 stream-fmap
 stream-fapply
 stream-fapply-transpose
 stream-flatmap
 stream-join
 nonempty-stream-duplicate
 nonempty-stream-extend
 (contract-out
  [stream-iterate-function (-> (-> any/c any/c)  any/c stream?)]
  [stream-window-function (-> stream? natural-number/c stream?)]
  [stream-return-function (-> any/c stream?)]
  [stream-fmap-function (-> (-> any/c any/c) stream? stream?)]
  [stream-fapply-function (-> (stream/c (-> any/c any/c)) stream? stream?)]
  [stream-fapply-transpose-function (-> (stream/c (-> any/c any/c)) stream? stream?)]
  [stream-flatmap-function (-> (-> any/c stream?) stream? stream?)]
  [stream-join-function (-> (stream/c stream?) stream?)]
  [nonempty-stream? predicate/c]
  [nonempty-stream-duplicate-function (-> nonempty-stream? (and/c nonempty-stream? (stream/c nonempty-stream?)))]
  [nonempty-stream-extend-function (-> (-> nonempty-stream? any/c) nonempty-stream? nonempty-stream?)]))

(require (for-syntax racket racket/syntax syntax/parse))

(define (stream-return-function x)
  (stream x))

(define-syntax (stream-return stx)
  (syntax-parse stx
    [_:id #'stream-return-function]
    [(_ x:expr)
     (syntax/loc stx
       (stream x))]))

(define (stream-fmap-function f xs)
  (stream-map f xs))

(define-syntax (stream-fmap stx)
  (syntax-parse stx
    [_:id #'stream-fmap-function]
    [(_ f:expr xs:expr)
     (syntax/loc stx
       (stream-lazy (stream-map f xs)))]))

(define (stream-fapply-function fs xs)
  (define (recur fs)
    (if (stream-empty? fs) empty-stream
      (stream-append (stream-map (stream-first fs) xs)
                     (recur (stream-rest fs)))))
  (recur fs))

(define-syntax (stream-fapply stx)
  (syntax-parse stx
    [_:id #'stream-fapply-function]
    [(_ fs:expr xs:expr)
     (syntax/loc stx
       (stream-lazy
        (stream-fapply-function fs xs)))]))


(define (stream-fapply-transpose-function fs xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-lazy
       (let ([x (stream-first xs)])
         (stream-append (stream-map (λ (f) (f x)) fs)
                        (recur (stream-rest xs)))))))
  (recur xs))

(define-syntax (stream-fapply-transpose stx)
  (syntax-parse stx
    [_:id #'stream-fapply-transpose-function]
    [(_ fs:expr xs:expr)
     (syntax/loc stx
       (stream-lazy
        (stream-fapply-transpose-function fs xs)))]))

(define (stream-flatmap-function f xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-lazy
       (stream-append (f (stream-first xs)) (recur (stream-rest xs))))))
  (recur xs))

(define-syntax (stream-flatmap stx)
  (syntax-parse stx
    [_:id #'stream-flatmap-function]
    [(_ f:expr xs:expr)
     (syntax/loc stx
       (stream-lazy
        (stream-flatmap-function f xs)))]))

(define (stream-join-function xss)
  (if (stream-empty? xss) empty-stream
    (stream-append (stream-first xss) (stream-join (stream-rest xss)))))

(define-syntax (stream-join stx)
  (syntax-parse stx
    [_:id #'stream-join-function]
    [(_ xss)
     (syntax/loc stx
       (stream-lazy
        (stream-join-function xss)))]))

(define (nonempty-stream? x)
  (and (stream? x) (not (stream-empty? x))))

(define (nonempty-stream-duplicate-function xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-cons xs (recur (stream-rest xs)))))
  (recur xs))

(define-syntax (nonempty-stream-duplicate stx)
  (syntax-parse stx
    [_:id #'nonempty-stream-duplicate-function]
    [(_ xs)
     (syntax/loc stx
       (stream-lazy
        (nonempty-stream-duplicate-function xs)))]))

(define (nonempty-stream-extend-function f xs)
  (define (recur xs)
    (if (stream-empty? xs) empty-stream
      (stream-cons (f xs) (recur (stream-rest xs)))))
  (recur xs))

(define-syntax (nonempty-stream-extend stx)
  (syntax-parse stx
    [_:id #'nonempty-stream-extend-function]
    [(_ f xs)
     (syntax/loc stx
       (stream-lazy
        (nonempty-stream-extend-function f xs)))]))

(define (stream-iterate-function f x)
  (stream-cons x (stream-iterate f (f x))))

(define-syntax (stream-iterate stx)
  (syntax-parse stx
    [_:id #'stream-iterate-function]
    [(_ f x)
     (syntax/loc stx
       (stream-lazy
        (stream-iterate-function f x)))]))

(define (stream-window-function xs n)
  (stream-cons
   (stream-take xs n)
   (stream-window-function (stream-tail xs n) n)))

(define-syntax (stream-window stx)
  (syntax-parse stx
    [_:id #'stream-window-function]
    [(_ xs:expr n:expr)
     (syntax/loc stx
       (stream-lazy
        (stream-window-function xs n)))]))
