#lang racket

(module+ test
  (require rackunit rackunit/spec
           spork/contravariant)

  (struct predicate
    (pred)
    #:methods gen:contravariant
    ((define (contramap func this)
       (match-let ([(predicate pred) this])
         (predicate (compose pred func))))))

  (define (predicate-run pred arg)
    (match-let ([(predicate proc) pred])
      (proc arg)))

  (define is-even (predicate even?))
  (check-true (predicate-run is-even 2))
  (check-false (predicate-run is-even 3))

  (define is-even-length (contramap length is-even))
  (check-true (predicate-run is-even-length '(1 2)))
  (check-false (predicate-run is-even-length '(1 2 3)))


  (define even-length? (contramap length even?))
  (check-true (even-length? '(1 2)))
  (check-false (even-length? '(1 2 3))))
