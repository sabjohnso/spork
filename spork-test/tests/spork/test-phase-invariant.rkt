#lang racket

(module+ test

  (require
   spork/phase-invariant
   (for-syntax racket racket/syntax syntax/parse)
   rackunit rackunit/spec)

  (define/phase-invariant y 3)
  (define-syntax y-times
    (syntax-parser
     [(_ e:expr)
      (with-syntax ([(es ...) (for/list ([i y]) #'e)])
        #'(list es ...))]))

  (check-equal? (length (y-times 'x)) y)
  (define/phase-invariant (twc x) (+ x x))

  (define-syntax run-twc
    (syntax-parser
     [(_ n:nat)
      (with-syntax ([m (twc (syntax->datum #'n))])
        #'m)]))

  (check-equal?  (run-twc 4) 8))
