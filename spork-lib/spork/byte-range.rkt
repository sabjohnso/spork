#lang racket

(provide
 unsafe-byte-range-intersect?
 unsafe-byte-range-adjacent?
 unsafe-byte-range-contiguous?
 unsafe-byte-range-simplify
 (contract-out
  (struct byte-range ([lower byte?] [upper byte?]))
  [byte-range-contains (-> byte-range? byte? boolean?)]
  [string->byte-range (-> byte-range-string? byte-range?)]
  [byte-range<? binary-relation/c]
  [byte-range<=? binary-relation/c]
  [byte-range>? binary-relation/c]
  [byte-range>=? binary-relation/c]
  [byte-range=? binary-relation/c]
  [byte-range-intersect? binary-relation/c]
  [byte-range-adjacent? binary-relation/c]
  [byte-range-contiguous? binary-relation/c]

  [byte-range-simplify (-> byte-range? byte-range? byte-range-simple-ascending-list?)]
  [byte-range-simplify-list (-> (listof byte-range?) byte-range-simple-ascending-list?)]))

(require)


(define/contract (byte-range-guard lower upper name)
  (->i ([lower byte?] [upper (lower) (and/c byte? (>=/c lower))] [name 'byte-range])
       (values
        [lower-result (lower) (=/c lower)]
        [upper-result (upper) (=/c upper)]))
  (values lower upper))


(struct byte-range
  (lower
   upper)
  #:guard byte-range-guard
  #:transparent)

(define (byte-range-string? x)
  (and (string? x)
       (regexp-match? #px"(.)-(.)" x)
       (match-let ([(regexp #px"(.)-(.)" (list _ lower upper)) x])
         (let ([lower (char->integer (string-ref lower 0))]
               [upper (char->integer (string-ref upper 0))])
           (and (byte? lower)
                (byte? upper)
                (<= lower upper))))))

(define (string->byte-range string)
  (match-let  ([(regexp #px"(.)-(.)" (list _ lower upper)) string])
    (let ([lower (char->integer (string-ref lower 0))]
          [upper (char->integer (string-ref upper 0))])
      (byte-range lower upper))))


(define (byte-range-contains range byte)
  (match-let ([(byte-range lower upper) range])
    (and (>= byte lower)
         (<= byte upper))))

(define (byte-range<? range1 range2)
  (match-let ([(byte-range lower1 upper1) range1]
              [(byte-range lower2 upper2) range2])
    (or (< lower1 lower2)
        (and (= lower1 lower2)
             (< upper1 upper2)))))

(define (byte-range<=? range1 range2)
  (not (byte-range<? range2 range1)))

(define (byte-range>? range1 range2)
  (byte-range<? range2 range1))

(define (byte-range>=? range1 range2)
  (not (byte-range<? range1 range2)))

(define (byte-range=? range1 range2)
  (not (or (byte-range<? range1 range2)
           (byte-range<? range2 range1))))


(define (unsafe-byte-range-intersect? range1 range2)
  ;; This function is unsafe in the sense that is assumes
  ;; that the arguments are in ascending order, without
  ;; any validation.

  (byte-range-contains range1 (byte-range-lower range2)))

(define (byte-range-intersect? range1 range2)
  (if (byte-range<? range1 range2)
      (unsafe-byte-range-intersect? range1 range2)
    (unsafe-byte-range-intersect? range2 range1)))

(define (unsafe-byte-range-adjacent? range1 range2)
  ;; This function is unsafe in the sense that is assumes
  ;; that the arguments are in ascending order, without
  ;; any validation.
  (= 1 (- (byte-range-lower range2) (byte-range-upper range1))))

(define (byte-range-adjacent? range1 range2)
  (if (byte-range<? range1 range2)
      (unsafe-byte-range-adjacent? range1 range2)
    (unsafe-byte-range-adjacent? range2 range1)))

(define (unsafe-byte-range-contiguous? range1 range2)
  ;; This function is unsafe in the sense that is assumes
  ;; that the arguments are in ascending order, without
  ;; any validation.
  (or (unsafe-byte-range-adjacent? range1 range2)
      (unsafe-byte-range-intersect? range1 range2)))

(define (byte-range-contiguous? range1 range2)
  (if (byte-range<? range1 range2)
      (unsafe-byte-range-contiguous? range1 range2)
    (unsafe-byte-range-contiguous? range2 range1)))

(define (unsafe-byte-range-simplify range1 range2)
  (if (unsafe-byte-range-contiguous? range1 range2)
      (list (byte-range (byte-range-lower range1)
                        (max (byte-range-upper range1)
                             (byte-range-upper range2))))
    (list range1 range2)))


(define (byte-range-simplify range1 range2)
  (if (byte-range<? range1 range2)
      (unsafe-byte-range-simplify range1 range1)
    (unsafe-byte-range-simplify range2 range1)))

(define (byte-range-simplify-list ranges)
  (let ([ranges (sort ranges byte-range<?)])
    (define (recur prev remaining accum)
      (if (null? remaining) (reverse (cons prev accum))
        (match (unsafe-byte-range-simplify prev (car remaining))
          [(list prev next) (recur next (cdr remaining) (cons prev accum))]
          [(list next) (recur next (cdr remaining) accum)])))
    (if (null? ranges) '()
      (recur (car ranges) (cdr ranges) '()))))


(define (byte-range-simple-ascending-list? ranges)
  (and (list? ranges)
       (or (null? ranges)
           (and (byte-range? (car ranges))
                (for/fold ([result #t] [prev (car ranges)] #:result result)
                    ([current (cdr ranges)] #:break (not result))
                  (values (and (byte-range? current)
                               (byte-range<? prev current)
                               (not (byte-range-contiguous? prev current)))
                          current))))))

(define binary-relation/c (-> byte-range? byte-range? boolean?))
