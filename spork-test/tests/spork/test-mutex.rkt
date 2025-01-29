#lang racket

(module+ test
  (require
   racket/future rackunit rackunit/spec
   spork/mutex)

  (describe "make-mutex"
    (it "makes a mutex"
      (define mutex (make-mutex))
      (check-true (mutex? mutex))))

  (describe "with-mutex"
    (context "with a mutex, and a counter"
      (define mutex (make-mutex))
      (define counter 0)
      (define n 1000)
      (it "will prevent simultaneous execution of the thunk passed to it"
        (let* ([futures
                (for/list ([i (in-range n)])
                  (future
                   (thunk
                    (with-mutex mutex
                      (thunk
                       (set! counter (add1 counter)))))))]
               [xs (for/list ([fut futures])
                     (touch fut))])
          (for ([x xs])
            (check-true (void? x)))
          (check-equal? (length xs) n))
        (check-equal? counter n)))))
