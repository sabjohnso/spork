#lang racket

(provide
 (contract-out
  (struct env ([proc (-> any/c any/c)]))
  [env-run (-> any/c env? any/c)]
  [env-return (-> any/c env?)]
  [env-ask env?]
  [env-select (-> (-> any/c any/c) env?)]
  [env-local (-> (-> any/c any/c) env? env?)]))

(require spork/functor)

(struct env
  (proc)
  #:methods gen:monad
  ((define (return-proc _) env-return)
   (define (flatmap-proc _) env-flatmap)))

(define (env-run e mx)
  ((env-proc mx) e))

(define (env-return x)
  (env (const x)))

(define (env-flatmap f mx)
  (env (λ (e) (env-run e (f (env-run e mx))))))

(define env-ask
  (env identity))

(define (env-select f)
  (env f))

(define (env-local f mx)
  (env (λ (e) (env-run (f e) mx))))
