#lang racket

(module+ test
  (require
   spork/wire
   spork/wire-formats
   rackunit rackunit/spec)


  (enum/wire thing uint8-big
    ([one 1]
     [two 2]))

  (check-true (thing? one))
  (check-true (thing? two))

  (check-true (thing-value? 1))
  (check-true (thing-value? 2))

  (check-equal? (thing-name->value one) 1)
  (check-equal? (thing-name->value two) 2)
  (check-equal? (thing-value->name 1) one)
  (check-equal? (thing-value->name 2) two)

  (check-true (enum-type? thing))

  (check-equal? (enum-type-value-type thing) uint8-big))
