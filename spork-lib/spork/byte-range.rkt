#lang racket

(provide
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
  [unsafe-byte-range-intersect? binary-relation/c]
  [unsafe-byte-range-adjacent? binary-relation/c]
  [unsafe-byte-range-contiguous? binary-relation/c]))

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

(define binary-relation/c (-> byte-range? byte-range? boolean?))
