#lang racket

(provide
 (contract-out
  (struct wire-type ())))

(struct wire-type () #:transparent)
