#lang racket

(module leaky-queue racket
  (provide)
  (require spork/throttle/input)
  (struct leaky-queue
    (clock
     interval
     max-count
     prev-time
     count
     queue)
    #:transparent))

(module+ test
  (require rackunit rackunit/spec
           spork/throttle/fixed-window-counter
           spork/throttle/token-bucket)
  (struct manual-clock
    ([time #:mutable])
    #:transparent
    #:property prop:procedure
    (λ (this) (manual-clock-time this)))

  (describe "fixed window rate limiters"
    (context "with some well defined inputs"
      (define clock (manual-clock 0))
      (define interval 10)
      (define max-count 4)

      (describe "fixed-window-input"
        (it "is a struct type with three fields used to construct a fixed-window rate limiter"
          (define input (fixed-window-input clock interval max-count))
          (check-true (fixed-window? (make-fixed-window input))))

        (it "requires a thunk producing a real value as the first parameter (clock)"
          (define bad-clock 'bad-clock)
          (check-exn exn:fail? (thunk (fixed-window-input bad-clock interval max-count))))

        (it "requires a positive real value as as the second parameter (interval)"
          (define bad-interval 0)
          (check-exn exn:fail? (thunk (fixed-window-input clock bad-interval max-count))))
        (it "requires a positive integer as the third parameter (max-count)"
          (define bad-max-count 0)
          (check-exn exn:fail? (thunk (fixed-window-input clock interval bad-max-count)))))

      (describe "the fixed-window rate limiter"
        (define throttle (make-fixed-window (fixed-window-input clock interval max-count)))
        (define execution-count 0)
        (define (proc)
          (set! execution-count (add1 execution-count)))
        (check-true (fixed-window? throttle))

        (it "executes its input during an interval when the count is less than the max count"
          (for ([i (in-range max-count)])
            (check-not-false (throttle proc))
            (check-equal? (add1 i) execution-count)))

        (it "will fail with an addional execution"
          (define prev-execution-count execution-count)
          (check-false (throttle proc))
          (check-equal? execution-count prev-execution-count))

        (it "will continue on the next interval"
          (define prev-execution-count execution-count)
          (set-manual-clock-time! clock (+ (manual-clock-time clock) interval))
          (check-not-false (throttle proc))
          (check-equal? (add1 prev-execution-count) execution-count)))))

  (describe "token bucket rate limiters"
    (context "with some well defined inputs"
      (define clock (manual-clock 0))
      (define interval 10)
      (define max-count 4)

      (describe "token-bucket-input"
        (it "is a struct type with three fields used to construct a token bucket rate limiter"
          (define input (token-bucket-input clock interval max-count))
          (check-true (token-bucket? (make-token-bucket input))))

        (it "requires a thunk producing a real value as the first parameter (clock)"
          (define bad-clock 'bad-clock)
          (check-exn exn:fail? (thunk (token-bucket-input bad-clock interval max-count))))

        (it "requires a positive real value as as the second parameter (interval)"
          (define bad-interval 0)
          (check-exn exn:fail? (thunk (token-bucket-input clock bad-interval max-count))))

        (it "requires a positive integer as the third parameter (max-count)"
          (define bad-max-count 0)
          (check-exn exn:fail? (thunk (token-bucket-input clock interval bad-max-count)))))

      (describe "the token-bucket rate limiter"
        (define input (token-bucket-input clock interval max-count))
        (define throttle (make-token-bucket input))
        (define execution-count 0)
        (define (proc)
          (set! execution-count (add1 execution-count)))

        (check-true (token-bucket? throttle))

        (it "is preloaded with max-count tokens"
          (for ([i (in-range max-count)])
            (check-not-false (throttle proc)))
          (check-equal? execution-count max-count)
          (check-false (throttle proc))
          (check-equal? execution-count max-count))

        (it "adds a token after the token period"
          (set-manual-clock-time! clock (/ interval max-count))
          (check-not-false (throttle proc))

          (check-false (throttle proc)))))))
