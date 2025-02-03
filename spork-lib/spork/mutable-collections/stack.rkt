#lang racket

(provide
 (contract-out
  [stack<%> interface?]
  [stack? predicate/c]
  [make-stack (-> stack?)]
  [stack-push! (-> stack? any/c void?)]
  [stack-pop! (-> stack? optional?)]
  [stack-clear! (-> stack? void?)]
  [stack-empty? (-> stack? boolean?)]))
(require spork/mutex spork/optional)

(define stack<%>
  (interface ()
    [push! (->m any/c void?)]
    [pop! (->m optional?)]
    [clear! (->m void?)]
    [empty? (->m boolean?)]))

(define (stack? v)
  (is-a? v stack<%>))

(define stack%
  (class* object%
      (stack<%>)
    (super-new)

    (define mutex (make-mutex))
    (define data '())

    (define/public (push! value)
      (with-mutex mutex
        (thunk
         (set! data (cons value data)))))

    (define/public (pop!)
      (with-mutex mutex
        (thunk
         (if (null? data) (none)
           (let ([result (car data)])
             (set! data (cdr data))
             (some result))))))

    (define/public (clear!)
      (with-mutex mutex
        (thunk
         (set! data '()))))

    (define/public (empty?)
      (null? data))))

(define (make-stack)
  (new stack%))

(define (stack-push! stack value)
  (send stack push! value))

(define (stack-pop! stack)
  (send stack pop!))

(define (stack-clear! stack)
  (send stack clear!))

(define (stack-empty? stack)
  (send stack empty?))
