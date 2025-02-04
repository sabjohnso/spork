#lang racket

(provide
 (contract-out
  [deque<%> interface?]
  [deque? predicate/c]
  [make-deque (-> (and/c deque? deque-empty?))]
  [deque-push-back! (-> deque? any/c void?)]
  [deque-push-front! (-> deque? any/c void?)]
  [deque-pop-back! (-> deque? optional?)]
  [deque-pop-front! (-> deque? optional?)]
  [deque-clear! (-> deque? void?)]
  [deque-empty? (-> deque? boolean?)]))

(require
 spork/mutable-collections/queue spork/mutable-collections/stack spork/optional spork/atomic)

(define deque<%>
  (interface (stack<%> queue<%>)
    [pop-back! (->m optional?)]))

(define (deque? v)
  (is-a? v deque<%>))

(struct deque-data
  (front back))

(define deque%
  (class* object%
      (deque<%>)
    (super-new)

    (define data (box (deque-data '() '())))

    (define/public (push-front! v)
      (let loop ([current-data (unbox data)])
        (match-let ([(deque-data front back) current-data])
          (when (not (box-cas! data current-data (deque-data (cons v front) back)))
            (loop (unbox data))))))

    (define/public (push-back! v)
      (let loop ([current-data (unbox data)])
        (match-let ([(deque-data front back) current-data])
          (when (not (box-cas! data current-data (deque-data front (cons v back))))
            (loop (unbox data))))))

    (define/public (pop-front!)
      (let loop ([current-data (unbox data)])
        (match-let ([(deque-data front back) (move-to-front current-data)])
          (let-values ([(result updated-data)
                        (if (null? front) (values (none) (deque-data front back))
                          (values (some (car front)) (deque-data (cdr front) back)))])
            (if (box-cas! data current-data updated-data) result
              (loop (unbox data)))))))

    (define/public (pop-back!)
      (let loop ([current-data (unbox data)])
        (match-let ([(deque-data front back) (move-to-back current-data)])
          (let-values ([(result updated-data)
                        (if (null? back) (values (none) (deque-data front back))
                          (values (some (car back)) (deque-data front (cdr back))))])
            (if (box-cas! data current-data updated-data) result
              (loop (unbox data)))))))

    (define (move-to-front current-data)
      (match-let ([(deque-data front back) current-data])
        (if (null? front)
            (deque-data (reverse back) '())
          current-data)))

    (define (move-to-back current-data)
      (match-let ([(deque-data front back) current-data])
        (if (null? back)
            (deque-data '() (reverse front))
          current-data)))

    (define/public (clear!)
      (let loop ([current-data (unbox data)])
        (when (not (box-cas! data current-data (deque-data '() '())))
          (loop (unbox data)))))

    (define/public (empty?)
      (match-let ([(deque-data front back) (unbox data)])
        (and (null? front) (null? back))))))

(define (make-deque)
  (new deque%))

(define (deque-push-back! deque value)
  (send deque push-back! value))

(define (deque-push-front! deque value)
  (send deque push-front! value))

(define (deque-pop-back! deque)
  (send deque pop-back!))


(define (deque-pop-front! deque)
  (send deque pop-front!))

(define (deque-clear! deque)
  (send deque clear!))

(define (deque-empty? deque)
  (send deque empty?))
