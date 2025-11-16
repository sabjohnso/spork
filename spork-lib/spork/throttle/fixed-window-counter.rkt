#lang racket

(provide
   (contract-out
    (struct fixed-window-input
      ([clock (-> real?)]
       [interval positive-real?]
       [max-count exact-positive-integer?]))
    [make-fixed-window (-> fixed-window-input? fixed-window?)]
    [fixed-window? predicate/c]))

(require spork/misc spork/throttle/input)

(struct fixed-window-input throttle-input () #:transparent)

(struct fixed-window
  fixed-window-input
  ([next-time #:mutable]
   [count #:mutable])
  #:transparent
  #:property prop:procedure (λ (this proc) ( fixed-window-apply this proc)))


(define  (fixed-window-apply throttle proc)
  (match-let ([(fixed-window clock interval max-count next-time count)  throttle])
    (let ([curr-time (clock)])
      (cond [(>= curr-time  next-time)
             (let ([results ((compose list proc))])
               (set-fixed-window-count! throttle 1)
               (set-fixed-window-next-time! throttle (+ curr-time interval))
               results)]
            [(> max-count count)
             (let ([results ((compose list proc))])
               (set-fixed-window-count! throttle (add1 count))
               results)]
            [else #f]))))

(define (make-fixed-window input)
  (match-let ([(fixed-window-input clock interval max-count) input])
    (fixed-window
     clock
     interval
     max-count
     (clock)
     0)))
