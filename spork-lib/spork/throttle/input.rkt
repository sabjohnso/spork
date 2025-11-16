#lang racket

(provide
 (contract-out
  (struct throttle-input
    ([clock (-> real?)]
     [interval exact-positive-integer?]
     [max-count exact-positive-integer?]))))

(require (only-in spork/misc positive-real?))

(struct throttle-input (clock interval max-count) #:transparent)
