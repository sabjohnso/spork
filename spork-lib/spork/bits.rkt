#lang racket

(provide
 (contract-out
  (struct byte-spec ([size natural-number/c] [position natural-number/c]))
  [bits? predicate/c]
  [make-bits
   (->i ([size natural-number/c]) ([data (size) (and/c natural-number/c (</c (expt 2 size)))])
        [result bits?])]
  [bits-size (-> bits? natural-number/c)]
  [bits-zeros? (-> bits? boolean?)]
  [bits-ones? (-> bits? boolean?)]
  [bits-ref
   (->i ([bits bits?] [index (bits) (and/c natural-number/c (</c (bits-size bits)))])
        [result bit/c])]
  [bits-set
   (->i ([bits bits?]
         [index (bits) (and/c natural-number/c (</c (bits-size bits)))]
         [bit bit/c])
        [result bits?])]
  [bits-load-byte
   (->i ([bits bits?] [spec (bits) (byte-in-range/c (bits-size bits))])
        [result natural-number/c])]
  [bits-store-byte
   (->i ([bits bits?]
         [spec (bits) (byte-in-range/c (bits-size bits))]
         [value (spec) (</c (expt 2 (byte-spec-size spec)))])
        [result bits?])]
  [bits-get-slice (->i ([bits bits?] [spec (bits) (byte-in-range/c (bits-size bits))])
                       [result bits?])]
  [bits-set-slice (->i ([bits bits?] [position natural-number/c]
                        [slice (bits position) (slice-in-range/c (bits-size bits) position)])
                       [result bits?])]))

(require srfi/54)

(define bit/c
  (or/c 0 1))

(define/contract (byte-in-range/c bits-size)
  (-> natural-number/c contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'byte-in-range/c bits-size)
   #:first-order
   (λ (spec)
     (and (byte-spec? spec)
          (match-let ([(byte-spec size position) spec])
            (< (+ size position) bits-size))))))

(define/contract (slice-in-range/c bits-size position)
  (-> natural-number/c natural-number/c contract?)
  (make-flat-contract
   #:name (build-compound-type-name 'byte-in-range/c bits-size)
   #:first-order
   (λ (slice)
     (and (bits? slice)
          (match-let ([(bits size _) slice])
            (< (+ size position) bits-size))))))

(struct bits
  (size data)
  #:methods gen:custom-write
  ((define (write-proc this out mode)
     (display (format-bits this) out)))
  #:transparent)

(define (format-bits bs)
  (format "#<bits ~a ~a>"
    (bits-size bs)
    (cat (bits-data bs) 'binary (bits-size bs) #\0)))

(define (make-bits size [initial-data 0])
  (bits size initial-data))

(define (bits-ref bs index)
  (bitwise-and (arithmetic-shift (bits-data bs) (- index)) 1))

(define (bits-set bs index bit)
  (struct-copy bits bs
    [data (bitwise-and (sub1 (arithmetic-shift 1 (bits-size bs)))
                       (if (zero? bit) (bitwise-and (bitwise-not (arithmetic-shift 1 index)))
                         (bitwise-ior (arithmetic-shift 1 index))))]))

(define (bits-zeros? bs)
  (zero? (bits-data bs)))

(define (bits-ones? bs)
  (= (bits-data bs)
     (sub1 (arithmetic-shift 1 (bits-size bs)))))

(struct byte-spec
  (size position))

(define (bits-load-byte bs spec)
  (match-let ([(byte-spec size position) spec]
              [(bits _ data) bs])
    (bitwise-and (sub1 (arithmetic-shift 1 size))
                 (arithmetic-shift data (- position)))))

(define (bits-store-byte bs spec value)
  (let ([cleared-data (bits-clear-byte bs spec)])
    (struct-copy bits bs
        [data (bitwise-ior
               cleared-data
               (arithmetic-shift value (byte-spec-position spec)))])))

(define (bits-clear-byte bs spec)
  (match-let ([(bits size data) bs]
              [(byte-spec byte-size byte-position) spec])
    (let ([byte-upper-bound (+ byte-size byte-position)])
      (bitwise-and
       (arithmetic-shift (ones (- size byte-upper-bound)) byte-upper-bound)
       (ones byte-position)))))

(define (bits-get-slice bs spec)
  (bits (byte-spec-size spec)
        (bits-load-byte bs spec)))

(define (bits-set-slice bs position slice)
  (bits-store-byte bs (byte-spec (bits-size slice) position) (bits-data slice)))

(define (ones n)
  (sub1 (bit n)))

(define (bit n)
  (arithmetic-shift 1 n))
