#lang racket

(provide
 struct/env
 (contract-out
  (struct env ([proc (-> any/c any/c)]))
  [env-run (-> any/c env? any/c)]
  [env-return (-> any/c env?)]
  [env-ask env?]
  [env-select (-> (-> any/c any/c) env?)]
  [env-local (-> (-> any/c any/c) env? env?)]))

(require
 spork/functor
 spork/struct-extras
 (for-syntax racket/syntax syntax/parse))

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


(begin-for-syntax
 (define (make-field-env-accessors struct-name/stx field-names/stx)
   (for/list ([field-name/stx (syntax-e field-names/stx)])
     (make-field-env-accessor struct-name/stx field-name/stx)))

 (define (make-field-env-accessor struct-name/stx field-name/stx)
   (with-syntax ([accessor (format-id field-name/stx "~a-~a" struct-name/stx field-name/stx)]
                 [accessor/env (format-id field-name/stx "~a-~a/env" struct-name/stx field-name/stx)])
     (syntax/loc #'accessor
       (define accessor/env (env-select accessor))))))

(define-syntax (struct/env stx)
  (syntax-parse stx
    [(_ struct-name:id (fields:field-decl ...)
        options:struct-options)
     (with-syntax ([(env-accessor-defs ...) (make-field-env-accessors #'struct-name #'(fields.field-name ...))]
                   [(options ...) #'options])
       (syntax/loc stx
         (begin
           (struct struct-name
             (fields ...)
             options ...)
           env-accessor-defs ...)) )]))
