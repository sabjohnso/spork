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
    [push-front! (->m any/c void?)]
    [pop-front! (->m optional?)]
    [clear! (->m void?)]
    [empty? (->m boolean?)]))

(define (stack? v)
  (is-a? v stack<%>))

(define stack%
  (class* object%
      (stack<%>)
    (super-new)

    (define data (box '()))

    (define/public (push-front! value)
      (let loop ([current-data (unbox data)])
        (when (not (box-cas! data current-data (cons value current-data)))
          (loop (unbox data)))))

    (define/public (pop-front!)
      (let loop ([current-data (unbox data)])
        (if (null? current-data) (none)
            (if (box-cas! data current-data (cdr current-data)) (some (car current-data))
              (loop (unbox data))))))

    (define/public (clear!)
      (let loop ([current-data (unbox data)])
        (when (not (box-cas! data current-data '()))
          (loop (unbox data)))))

    (define/public (empty?)
      (null? (unbox data)))))

(define (make-stack)
  (new stack%))

(define (stack-push! stack value)
  (send stack push-front! value))

(define (stack-pop! stack)
  (send stack pop-front!))

(define (stack-clear! stack)
  (send stack clear!))

(define (stack-empty? stack)
  (send stack empty?))
