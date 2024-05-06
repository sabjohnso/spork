#lang racket

(provide
 (contract-out
  (struct stateful ([proc (-> any/c pair?)]))
  [stateful-run (-> any/c stateful? pair?)]
  [stateful-exec (-> any/c stateful? any/c)]
  [stateful-eval (-> any/c stateful? pair?)]
  [stateful-return (-> any/c stateful?)]
  [stateful-get stateful?]
  [stateful-select (-> (-> any/c any/c) stateful?)]
  [stateful-put (-> any/c stateful?)]
  [stateful-modify (-> (-> any/c any/c) stateful?)]
  [undefined-value? predicate/c]
  [undefined-value undefined-value?]))

(require
 spork/tag spork/functor)

(struct stateful
  (proc)
  #:methods gen:monad
  ((define (return-proc _) stateful-return)
   (define (flatmap-proc _) stateful-flatmap)))

(define (stateful-run s mx)
  ((stateful-proc mx) s))

(define (stateful-eval s mx)
  (car (stateful-run s mx)))

(define (stateful-exec s mx)
  (cdr (stateful-run s mx)))

(define (stateful-return x)
  (stateful (λ (s) (cons x s))))

(define (stateful-flatmap f mx)
  (stateful
   (λ (s)
     (match-let ([(cons x s) (stateful-run s mx)])
       (stateful-run s (f x))))))

(define-tag undefined-value)

(define stateful-get
  (stateful (λ (s) (cons s s))))

(define (stateful-select f)
  (stateful (λ (s) (cons (f s) s))))

(define (stateful-put s)
  (stateful (λ (_) (cons undefined-value s))))

(define (stateful-modify f)
  (stateful (λ (s) (cons undefined-value (f s)))))
