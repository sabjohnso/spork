#lang racket

(provide
 (contract-out
  [stack? predicate/c]
  [lock-free-stack? predicate/c]
  [make-lock-free-stack (-> stack?)]
  [stack-push! (-> stack? any/c void?)]
  [stack-pop! (-> stack? optional?)]
  [stack-clear! (-> stack? void?)]
  [stack-empty? (-> stack? boolean?)]))

(require spork/atomic spork/optional spork/mutable-collections/stack)

(define lock-free-stack%
  (class* object%
      (stack<%>)
    (super-new)

    (define data '())

    (define/public (push! value)
      (atomically
       (thunk
        (set! data (cons value data)))))

    (define/public (pop!)
      (atomically
       (thunk
        (if (null? data) (none)
          (let ([result (car data)])
            (set! data (cdr data))
            (some result))))))

    (define/public (clear!)
      (atomically
       (thunk (set! data '()))))

    (define/public (empty?)
      (null? data))))

(define (make-lock-free-stack)
  (new lock-free-stack%))

(define (lock-free-stack? v)
  (is-a? v lock-free-stack%))
