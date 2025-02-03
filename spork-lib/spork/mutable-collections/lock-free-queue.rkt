#lang racket

(provide
 (contract-out
  [queue? predicate/c]
  [make-lock-free-queue (-> queue?)]
  [queue-push-back! (-> queue? any/c void?)]
  [queue-pop-front! (-> queue? optional?)]
  [queue-clear! (-> queue? void?)]
  [queue-empty? (-> queue? boolean?)]))

(require
 spork/mutable-collections/queue
 spork/optional
 spork/atomic
 ffi/unsafe/atomic)

(define lock-free-queue%
  (class* object%
      (queue<%>)
    (super-new)
    (define input-data '())
    (define output-data '())

    (define/public (push-back! v)
      (atomically
       (thunk
        (set! input-data (cons v input-data)))))

    (define (rearrange-data)
      (when (null? output-data)
        (set! output-data (reverse input-data))
        (set! input-data '())))

    (define/public (pop-front!)
      (atomically
       (thunk
        (rearrange-data)
        (if (null? output-data) (none)
          (let ([result (car output-data)])
            (set! output-data (cdr output-data))
            (some result))))))

    (define/public (clear!)
      (atomically
       (thunk
        (set! input-data '())
        (set! output-data '()))))

    (define/public (empty?)
      (atomically
       (thunk
        (and (null? input-data)
             (null? output-data)))))))

(define (make-lock-free-queue)
  (new lock-free-queue%))
