#lang racket

(provide
 (contract-out
  (struct token-bucket-input
    ([clock (-> real?)]
     [interval positive-real?]
     [max-count exact-positive-integer?]))
  [make-token-bucket (-> token-bucket-input? token-bucket?)]
  [token-bucket? predicate/c]))

(require spork/misc spork/throttle/input)
(struct token-bucket-input throttle-input () #:transparent)

(struct token-bucket
  token-bucket-input
  (token-period
   [token-count #:mutable]
   [prev-time #:mutable])
  #:transparent
  #:property prop:procedure (λ (this proc) (token-bucket-apply this proc)))

(define (token-bucket-apply throttle proc)
  (match-let ([(token-bucket clock interval max-count token-period token-count prev-time) throttle])
    (let* ([curr-time (clock)]
           [elapsed-time (max 0 (- curr-time prev-time))])
      (cond [(or (> token-count 0) (>= elapsed-time token-period))
             (let ([results ((compose list proc))]
                   [full-periods (floor (/ elapsed-time token-period))])
               (set-token-bucket-token-count! throttle (min max-count (sub1 (+ token-count full-periods))))
               (set-token-bucket-prev-time! throttle (+ prev-time (* full-periods token-period)))
               results)]
            [else #f]))))

(define (make-token-bucket input)
  (match-let ([(token-bucket-input clock interval max-count) input])
    (let ([time-delta (/ interval max-count)])
      (token-bucket
       clock interval max-count
       time-delta
       max-count
       (clock)))))
