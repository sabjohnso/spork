#lang racket

(provide
 (contract-out
  [mutex? predicate/c]
  [make-mutex (-> mutex?)]
  [with-mutex (-> mutex? (-> any/c) any/c)]))

(require racket/future)

(define mutex%
  (class object%
    (super-new)

    (define mex (make-semaphore 1))

    (define/public (run thunk)
      (semaphore-wait mex)
      (let ([results ((compose list thunk))])
        (semaphore-post mex)
        (apply values results)))))

(define (mutex? x)
  (is-a? x mutex%))

(define (make-mutex)
  (new mutex%))

(define (with-mutex mex thunk)
  (send mex run thunk))

(module+ test
  (require rackunit)

  (let ([mex (make-mutex)])
    (check-true (mutex? mex))
    (check-equal?
     (with-mutex mex
       (thunk 'x))
     'x)))
