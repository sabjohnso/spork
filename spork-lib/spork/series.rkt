#lang racket

(provide
 (contract-out
  (struct series ([proc (-> natural-number/c number?)]))
  [series-partial-sums (-> series? seq?)]
  [midpoint-transform (-> seq? seq?)]
  [diff-transform (-> seq? seq?)]
  [eulers-transform (-> seq? seq?)]
  [van-wijngaarden-diagonal-transform (-> seq? seq?)]
  [delta-squared-transform (-> seq? seq?)]))
(require racket/generic)

(struct series
  (proc)
  #:property prop:procedure (struct-field-index proc))

(struct seq
  (data)
  #:methods gen:stream
  ((define/generic generic-stream-first stream-first)
   (define/generic generic-stream-rest stream-rest)
   (define (stream-empty? xs) #f)
   (define (stream-first xs) (generic-stream-first (seq-data xs)))
   (define (stream-rest xs) (seq (generic-stream-rest (seq-data xs))))))

(define (series-partial-sums f)
  (define (recur i x0)
    (stream-lazy
     (let ([x1 (+ (f i) x0)])
       (seq (stream-cons x1 (recur (add1 i) x1))))))
  (seq (recur 0 0)))

;; midpoint-transform (-> seq seq)
(define (midpoint-transform xs)
  (define (recur x0 xs)
    (seq
     (stream-lazy
      (let ([x1 (stream-first xs)])
        (stream-cons (/ (+ x0 x1) 2) (recur x1 (stream-rest xs)))))))
  (seq (stream-lazy
        (recur (stream-first xs) (stream-rest xs)))))

;; diff-transform (-> seq seq)
(define (diff-transform xs)
  (define (recur x0 xs)
    (seq (stream-lazy
          (let ([x1 (stream-first xs)])
            (stream-cons (- x1 x0) (recur x1 (stream-rest xs)))))))
  (seq (stream-lazy (recur (stream-first xs) (stream-rest xs)))))

;; diff-transform (-> seq seq)
(define (eulers-transform xs)
  (define (recur xs)
    (seq
     (stream-lazy
      (let ([ys (midpoint-transform xs)])
        (stream-cons (stream-first ys) (recur ys))))))
  (recur xs))

;; van-wijngaarden-diagonal-transform (-> seq? seq?)
(define (van-wijngaarden-diagonal-transform xs)
  (define (recur xs i)
    (seq
     (stream-lazy
      (stream-cons
       (stream-ref xs i)
       (recur (midpoint-transform xs) (add1 i))))))
  (recur xs 0))

;; van-wijngaarden-diagonal-transform (-> seq? seq?)
(define (van-wijngaarden-power-transform xs)
  (define (recur xs i)
    (seq
     (stream-lazy
      (stream-cons
       (stream-ref xs i)
       (recur (van-wijngaarden-diagonal-transform xs) (add1 i))))))
  (recur xs 0))

;; van-wijngaarden-diagonal-transform (-> seq? seq?)
(define (delta-squared-transform xs)
  (define (recur xs dxs ddxs)
    (seq
     (stream-lazy
      (let ([x (stream-first xs)]
            [dx (stream-first dxs)]
            [ddx (stream-first ddxs)])
        (stream-cons (- x (/ (sqr dx) ddx))
                     (recur (stream-rest xs)
                            (stream-rest dxs)
                            (stream-rest ddxs)))))))
  (let* ([dxs (diff-transform xs)]
         [ddxs (diff-transform dxs)])
    (recur (stream-tail xs 2)
           (stream-tail dxs 1)
           ddxs)))

(define (stream-second xs)
  (stream-first (stream-rest xs)))
