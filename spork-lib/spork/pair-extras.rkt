#lang racket

(provide
 (contract-out
  [pair-extract (-> pair? any/c)]
  [pair-duplicate (-> pair? pair?)]
  [pair-extend (-> (-> pair? any/c) pair? pair?)]))

(define (pair-extract xs) (car xs))

(define (pair-duplicate xs)
  (cons xs (cdr xs)))

(define (pair-extend f xs)
  (cons (f xs) (cdr xs)))
