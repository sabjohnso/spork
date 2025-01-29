#lang racket

(provide
 thunk-memoize
 lambda-memoize
 define-memoize
 (rename-out [lambda-memoize λ-memoize]))

(require
 (for-syntax racket racket/syntax syntax/parse)
 spork/mutex spork/tag)

(define-tag undefined)

(struct memoized-thunk
  (proc)
  #:property prop:procedure (struct-field-index proc))

(define-syntax (thunk-memoize stx)
  (syntax-parse stx
    [(_ es:expr ...+)
     (with-syntax ([(value proc mex) (generate-temporaries '(value proc mex))])
       (syntax/loc stx
         (let ([value undefined]
               [proc (thunk es ...)]
               [mex (make-mutex)])
           (memoized-thunk
            (thunk
             (when (undefined? value)
               (with-mutex mex
                 (thunk
                  (when (undefined? value)
                    (set! value (proc))
                    (set! proc undefined)
                    (set! mex undefined)))))
             value)))))]))

(struct memoized-lambda
  (proc)
  #:property prop:procedure (struct-field-index proc))

(define-syntax (lambda-memoize stx)
  (syntax-parse stx
    [(_ (xs:id ...+) body:expr ...+)
     (with-syntax ([(table proc mex args) (generate-temporaries '(table proc mex args))])
       (syntax/loc stx
         (let ([table (make-hash '())]
               [proc (λ (xs ...) body ...)]
               [mex (make-mutex)])
           (memoized-lambda
            (λ (xs ...)
              (let ([args (list xs ...)])
                (when (not (hash-has-key? table args))
                  (with-mutex mex
                    (thunk
                     (when (not (hash-has-key? table args))
                       (hash-set! table args (apply proc args))))))
                (hash-ref table args)))))))]))

(define-syntax (define-memoize stx)
  (syntax-parse stx
    [(_ (f:id) body ...+)
     (syntax/loc stx
       (define f (thunk-memoize body ...)))]
    [(_ (f:id xs:id ...+) body:expr ...+)
     (syntax/loc stx
       (define f (lambda-memoize (xs ...) body ...)))]))
