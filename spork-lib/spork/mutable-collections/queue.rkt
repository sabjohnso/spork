#lang racket

(provide
 (contract-out
  [queue<%> interface?]
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

(struct queue-data
  (front back))

(define queue%
  (class* object%
      (queue<%>)
    (super-new)

    (define mutex (make-mutex))
    (define data (box (queue-data '() '())))

    (define/public (push-back! v)
      (let loop ([current-data (unbox data)])
        (match-let ([(queue-data front back) current-data])
          (when (not (box-cas! data current-data (queue-data front (cons v back))))
            (loop (unbox data))))))

    (define (maybe-move-data-to-front current-data)
      (match-let ([(queue-data front back) current-data])
        (if (null? front)
            (queue-data (reverse back) '())
          current-data)))

    (define/public (pop-front!)
      (let loop ([current-data (unbox data)])
        (match-let ([(queue-data front back) (maybe-move-data-to-front current-data)])
          (let-values ([(result updated-data)
                        (if (null? front) (values (none) (queue-data front back))
                          (values (some (car front))
                                  (queue-data (cdr front) back)))])
            (if (box-cas! data current-data updated-data) result
              (loop (unbox data)))))))

    (define/public (clear!)
      (let loop ([current-data (unbox data)])
        (when (not (box-cas! data current-data (queue-data '() '())))
          (loop (unbox data)))))

    (define/public (empty?)
      (match-let ([(queue-data front back) (unbox data)])
        (and (null? front)
             (null? back))))))

(define (make-queue) (new queue%))

(define (queue-push-back! queue v)
  (send queue push-back! v))

(define (queue-pop-front! queue)
  (send queue pop-front!))

(define (queue-clear! queue)
  (send queue clear!))

(define (queue-empty? queue)
  (send queue empty?))
