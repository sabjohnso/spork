#lang racket

(module+ test
  (require spork/misc rackunit rackunit/spec)

  (describe "twc"
    (it "accepts a number and returns a number that is twice the input"
      (check-equal? (twc 3) (* 2 3))))

  (describe "call-with"
    (it "accepts an argument and a function, the returns the result of applying the function to the argument"
      (check-equal? (call-with 4 twc) 8))))
