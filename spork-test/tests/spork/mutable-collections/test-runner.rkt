#lang racket

(module+ test
  (require
   spork/mutable-collections/runner
   rackunit rackunit/spec)

  (describe "runner%"
    (it "is a class that runs a thunk repeatedly in a separate thread"
      (define counter 0)
      (define thk (thunk (set! counter (add1 counter))))
      (define runner (new runner%))
      (check-false (send runner running?))
      (check-false (send runner stopping?))

      (send runner run-thunk thk)

      (check-true (send runner running?))
      (check-false (send runner stopping?))

      (send runner stop)
      (let loop ()
        (check-true (or (send runner stopping?) (not (send runner running?))))
        (when (send runner running?)
          (loop)))

      (check-false (send runner running?))
      (check-false (send runner stopping?))))

  (describe "abstract-runnable%"
    (it "is an abstract class to help implementing runnable<%>"
      (define runnable%
        (class abstract-runnable%
          (super-new)
          (define/override (get-thunk) void)))

      (define runnable (new runnable%))
      (check-false (send runnable running?))
      (check-false (send runnable stopping?))

      (send runnable run)

      (check-true (send runnable running?))
      (check-false (send runnable stopping?))

      (send runnable stop)
      (let loop ()
        (check-true (or (send runnable stopping?) (not (send runnable running?))))
        (when (send runnable running?)
          (loop)))
      (check-false (send runnable running?))
      (check-false (send runnable stopping?)))))
