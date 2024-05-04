#lang racket

(provide
 (contract-out
  [function?        predicate/c]
  [function-fmap    (-> (-> any/c any/c) function? function?)]
  [function-fapply  (-> function? function? function?)]
  [function-flatmap (-> (-> any/c function?) function? function?)] 
  [function-return  (-> any/c function?)]
  [function-join    (-> function? function?)]
  [function-arr     (-> function? function?)]
  [function-fst     (-> (-> any/c any/c) (-> pair? pair?))]
  [function-snd     (-> (-> any/c any/c) (-> pair? pair?))]
  [function-split   (-> (-> any/c any/c) (-> any/c any/c) (-> pair? pair?))]
  [function-fanout  (-> (-> any/c any/c) (-> any/c any/c) (-> any/c pair?))]))

(define (function? x)
  (and (procedure? x) (procedure-arity-includes? x 1)))

(define (function-return x)
  (const x))

(define (function-flatmap f mx)
  (λ (e) ((f (mx e)) e)))

(define (function-fmap f mx)
  (compose f mx))

(define (function-fapply mf mx)
  (λ (e) ((mf e) (mx e))))

(define function-ask identity)

(define (function-local f mx)
  (compose mx f))

(define (function-join mmx)
  (λ (e) ((mmx e) e)))

(define (function-arr f) f)

(define (function-fst f)
  (λ (xs) (cons (f (car xs)) (cdr xs))))

(define (function-snd f)
  (λ (xs) (cons (car xs) (f (cdr xs)))))

(define (function-split f g)
  (λ (xs) (cons (f (car xs)) (g (cdr xs)))))

(define (function-fanout f g)
  (λ (x) (cons (f x) (g x))))


