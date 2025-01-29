#lang racket

(provide
 (contract-out
  [queue? predicate/c]
  [make-queue (-> queue?)]
  [queue-push-back! (-> queue? any/c void?)]
  [queue-pop-front! (-> queue? optional?)]
  [queue-clear! (-> queue? void?)]
  [queue-empty? (-> queue? boolean?)]))

(require spork/optional spork/mutex)

(define queue<%>
  (interface ()
    [push-back! (->m any/c void?)]
    [pop-front! (->m optional?)]
    [clear! (->m void?)]
    [empty? (->m boolean?)]))

(define (queue? v)
  (is-a? v queue<%>))

(define queue%
  (class* object%
      (queue<%>)
    (super-new)

    (define mutex (make-mutex))
    (define input-data '())
    (define output-data '())

    (define/public (push-back! v)
      (with-mutex mutex
        (thunk
         (set! input-data (cons v input-data)))))

    (define (rearrange-data)
      (when (null? output-data)
        (set! output-data (reverse input-data))
        (set! input-data '())))

    (define/public (pop-front!)
      (with-mutex mutex
        (thunk
         (rearrange-data)
         (if (null? output-data) (none)
           (let ([result (car output-data)])
             (set! output-data (cdr output-data))
             (some result))))))

    (define/public (clear!)
      (with-mutex mutex
        (thunk
         (set! input-data '())
         (set! output-data '()))))

    (define/public (empty?)
      (with-mutex mutex
        (thunk
         (and (null? input-data)
              (null? output-data)))))))

(define (make-queue) (new queue%))

(define (queue-push-back! queue v)
  (send queue push-back! v))

(define (queue-pop-front! queue)
  (send queue pop-front!))

(define (queue-clear! queue)
  (send queue clear!))

(define (queue-empty? queue)
  (send queue empty?))
