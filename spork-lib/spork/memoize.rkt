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
         (let ([value (box undefined)]
               [proc (thunk es ...)])
           (memoized-thunk
            (thunk
             (let ([current-value (unbox value)])
               (if (undefined? current-value)
                   (let ([result ((compose list proc))])
                     (box-cas! value current-value result)
                     (apply values result))
                 (apply values current-value))))))))]))

(struct memoized-lambda
  (proc)
  #:property prop:procedure (struct-field-index proc))

(define-syntax (lambda-memoize stx)
  (syntax-parse stx
    [(_ (xs:id ...+) body:expr ...+)
     (with-syntax ([(table proc mex args) (generate-temporaries '(table proc mex args))])
       (syntax/loc stx
         (let ([table (box (make-immutable-hash '()))]
               [proc (λ (xs ...) body ...)])
           (memoized-lambda
            (λ (xs ...)
              (let ([args (list xs ...)])
                (let ([current-table (unbox table)])
                  (if (not (hash-has-key? current-table args))
                      (let ([result ((compose list proc) xs ...)])
                        (box-cas! table current-table (hash-set current-table args result))
                        (apply values result))
                    (apply values (hash-ref (unbox table) args))))))))))]))

(define-syntax (define-memoize stx)
  (syntax-parse stx
    [(_ (f:id) body ...+)
     (syntax/loc stx
       (define f (thunk-memoize body ...)))]
    [(_ (f:id xs:id ...+) body:expr ...+)
     (syntax/loc stx
       (define f (lambda-memoize (xs ...) body ...)))]))
