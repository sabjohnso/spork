#lang racket

(provide
 (contract-out

  [endian? predicate/c]
  [big endian?]
  [little endian?]

  (struct fixed-integer
    ([size natural-number/c]
     [unit exact-positive-integer?]
     [signed? boolean?]
     [endianness endian?]
     [bit-endianness endian?]))
  [fixed-unsigned
   (->* (natural-number/c)
        (#:unit exact-positive-integer? #:endianness endian? #:bit-endianness endian?)
        fixed-integer?)]
  [fixed-signed
   (->* (natural-number/c)
        (#:unit exact-positive-integer? #:endianness endian? #:bit-endianness endian?)
        fixed-integer?)]
  [uint8-big    fixed-integer?]
  [uint16-big   fixed-integer?]
  [uint32-big   fixed-integer?]
  [uint64-big   fixed-integer?]
  [uint128-big  fixed-integer?]
  [uint256-big  fixed-integer?]
  [uint512-big fixed-integer?]

  [int8-big    fixed-integer?]
  [int16-big   fixed-integer?]
  [int32-big   fixed-integer?]
  [int64-big   fixed-integer?]
  [int128-big  fixed-integer?]
  [int256-big  fixed-integer?]
  [int512-big  fixed-integer?]

  [uint8-little    fixed-integer?]
  [uint16-little   fixed-integer?]
  [uint32-little   fixed-integer?]
  [uint64-little   fixed-integer?]
  [uint128-little  fixed-integer?]
  [uint256-little  fixed-integer?]
  [unint512-little fixed-integer?]

  [int8-little    fixed-integer?]
  [int16-little   fixed-integer?]
  [int32-little   fixed-integer?]
  [int64-little   fixed-integer?]
  [int128-little  fixed-integer?]
  [int256-little  fixed-integer?]
  [nint512-little fixed-integer?]
  [fixed-integer-bits-reader (-> fixed-integer? (-> bits? natural-number/c exact-integer?))]
  [fixed-integer-bits-writer (-> fixed-integer? (-> bits? natural-number/c exact-integer? bits?))]

  (struct fixed-string
    ([length exact-positive-integer?]
     [padding-character byte-char?]
     [padding-side padding-side?]))

  [fixed-string-bits-reader (-> fixed-string? (-> bits? natural-number/c bytes?))]
  [fixed-string-bits-writer (-> fixed-string? (-> bits? natural-number/c bytes? bits?))]

  (struct fixed-array
    ([element-type fixed-size-type?]
     [length natural-number/c]))

  (struct fixed-tuple
    ([components (listof (or/c fixed-size-type? fixed-tuple-super?))]))

  (struct fixed-tuple-super
    ([type fixed-tuple?]))
  [fixed-tuple-element-count (-> fixed-tuple? natural-number/c)]
  [fixed-tuple-super-count (-> fixed-tuple? natural-number/c)]

  (struct fixed-record
    ([components
      (and/c (listof fixed-record-component?)
             unique-field-names?
             unique-super-names?)]))

  (struct fixed-record-field
    ([name symbol?]
     [type fixed-size-type?]))

  (struct fixed-record-super
    ([name symbol?]
     [type fixed-record?]))

  [component-field-names (-> (listof fixed-record-component?) (listof symbol?))]
  [fixed-record-component? predicate/c]
  [fixed-record-supers (-> fixed-record (listof fixed-record-super?))]
  [fixed-record-super-count (-> fixed-record? natural-number/c)]
  [fixed-record-fields (-> fixed-record (listof fixed-record-field?))]
  [fixed-record-field-names (-> fixed-record (listof symbol?))]
  [fixed-record-field-count (-> fixed-record? natural-number/c)]

  (struct enum-type
    [(value-type fixed-size-type?)
     (named-values  unique-name-integer-pairs?)])

  (struct discriminated-union
    ([discriminant enum-type?]
     [variants (listof (cons/c symbol? wire-format?))]))

  [fixed-size-type? predicate/c]
  [fixed-size-in-bits (-> fixed-size-type? natural-number/c)]

  [unique-field-names? predicate/c]
  [unique-super-names? predicate/c]))

(require spork/list-extras spork/bits)

(define bits-per-octet 8)


(define endian
  (set 'little 'big))


(define (endian? arg)
  (set-member? endian arg))

(define little 'little)
(define big 'big)


;;
;; ... Fixed Size Integers
;;

(struct fixed-integer
  (size unit signed? endianness bit-endianness)
  #:transparent)


(define (fixed-integer-min-value type)
  (match type
    [(fixed-integer _ _ #f _ _) 0]
    [(fixed-integer size unit #t _ _)
     (- (expt 2 (sub1 (* size unit))))]))


(define (fixed-integer-max-value type)
  (match-let ([(fixed-integer size unit signed? _ _) type])
    (if signed? (expt 2 (sub1 (* size unit)))
      (expt 2 (* size unit)))))


(define (fixed-integer-predicate type)
  (let ([min (fixed-integer-min-value type)]
        [max (fixed-integer-max-value type)])
    (λ (x)
      (and (exact-integer? x)
           (>= x min)
           (<= x max)))))


(define (fixed-signed
         size
         #:unit [unit 8]
         #:endianness [endianness 'little]
         #:bit-endianness [bit-endianness 'little])
  (fixed-integer size unit #t endianness bit-endianness))


(define (fixed-unsigned
         size
         #:unit [unit 8]
         #:endianness [endianness 'little]
         #:bit-endianness [bit-endianness 'little])
  (fixed-integer size unit #f endianness bit-endianness))


(define uint8-big    (fixed-unsigned  1 #:endianness big))
(define uint16-big   (fixed-unsigned  2 #:endianness big))
(define uint32-big   (fixed-unsigned  4 #:endianness big))
(define uint64-big   (fixed-unsigned  8 #:endianness big))
(define uint128-big  (fixed-unsigned 16 #:endianness big))
(define uint256-big  (fixed-unsigned 32 #:endianness big))
(define uint512-big  (fixed-unsigned 64 #:endianness big))

(define int8-big    (fixed-signed  1 #:endianness big))
(define int16-big   (fixed-signed  2 #:endianness big))
(define int32-big   (fixed-signed  4 #:endianness big))
(define int64-big   (fixed-signed  8 #:endianness big))
(define int128-big  (fixed-signed 16 #:endianness big))
(define int256-big  (fixed-signed 32 #:endianness big))
(define int512-big  (fixed-signed 64 #:endianness big))

(define uint8-little    (fixed-unsigned  1 #:endianness little))
(define uint16-little   (fixed-unsigned  2 #:endianness little))
(define uint32-little   (fixed-unsigned  4 #:endianness little))
(define uint64-little   (fixed-unsigned  8 #:endianness little))
(define uint128-little  (fixed-unsigned 16 #:endianness little))
(define uint256-little  (fixed-unsigned 32 #:endianness little))
(define unint512-little (fixed-unsigned 64 #:endianness little))

(define int8-little    (fixed-signed  1 #:endianness little))
(define int16-little   (fixed-signed  2 #:endianness little))
(define int32-little   (fixed-signed  4 #:endianness little))
(define int64-little   (fixed-signed  8 #:endianness little))
(define int128-little  (fixed-signed 16 #:endianness little))
(define int256-little  (fixed-signed 32 #:endianness little))
(define nint512-little (fixed-signed 64 #:endianness little))

;; readers

(define (fixed-integer-bits-reader type)
  (match type
    [(fixed-integer _ _ #f 'little 'little) (fixed-integer-bits-reader/unsigned-little/little type)]
    [(fixed-integer _ _ #t 'little 'little) (fixed-integer-bits-reader/signed-little/little   type)]
    [(fixed-integer _ _ #f 'big    'little) (fixed-integer-bits-reader/unsigned-big/little    type)]
    [(fixed-integer _ _ #t 'big    'little) (fixed-integer-bits-reader/signed-big/little      type)]
    [(fixed-integer _ _ #f 'little 'big)    (fixed-integer-bits-reader/unsigned-little/big    type)]
    [(fixed-integer _ _ #t 'little 'big)    (fixed-integer-bits-reader/signed-little/big      type)]
    [(fixed-integer _ _ #f 'big    'big)    (fixed-integer-bits-reader/unsigned-big/big       type)]
    [(fixed-integer _ _ #t 'big    'big)    (fixed-integer-bits-reader/signed-big/big         type)]))

(define (fixed-integer-bits-reader/unsigned-little/little type)
  (match-let ([(fixed-integer size unit _ _ _) type])
    (define nbits (* size unit))
    (define (read-bits bits offset)
      (bits-load-byte bits (byte-spec nbits offset)))
    read-bits))

(define (fixed-integer-bits-reader/signed-little/little type)
  (match-let ([(fixed-integer size unit _ _ _) type])
    (define nbits (* size unit))
    (define unsigned->signed (make-unsigned->signed nbits))
    (define (read-bits bits offset)
      (unsigned->signed (bits-load-byte bits (byte-spec nbits offset))))
    read-bits))

(define (fixed-integer-bits-reader/unsigned-big/little type)
  (match-let ([(fixed-integer size unit _ _ _) type])
    (define nbits (* size unit))
    (define reverse-bytes (make-byte-reverser size unit))
    (define (read-bits bits offset)
      (reverse-bytes (bits-load-byte bits (byte-spec nbits offset))))
    read-bits))

(define (fixed-integer-bits-reader/signed-big/little      type)
  (match-let ([(fixed-integer size unit _ _ _) type])
    (define nbits (* size unit))
    (define reverse-bytes (make-byte-reverser size unit))
    (define unsigned->signed (make-unsigned->signed nbits))
    (define (read-bits bits offset)
      (unsigned->signed (reverse-bytes (bits-load-byte bits (byte-spec nbits offset)))))
    read-bits))

(define (fixed-integer-bits-reader/unsigned-little/big    type)
  (error "not implemented"))

(define (fixed-integer-bits-reader/signed-little/big      type)
  (error "not implemented"))

(define (fixed-integer-bits-reader/unsigned-big/big       type)
  (error "not implemented"))

(define (fixed-integer-bits-reader/signed-big/big         type)
  (error "not implemented"))

;; writers

(define (fixed-integer-bits-writer type)
  (match type
    [(fixed-integer _ _ #f 'little 'little) (fixed-integer-bits-writer/unsigned-little/little type)]
    [(fixed-integer _ _ #t 'little 'little) (fixed-integer-bits-writer/signed-little/little   type)]
    [(fixed-integer _ _ #f 'big    'little) (fixed-integer-bits-writer/unsigned-big/little    type)]
    [(fixed-integer _ _ #t 'big    'little) (fixed-integer-bits-writer/signed-big/little      type)]
    [(fixed-integer _ _ #f 'little 'big)    (fixed-integer-bits-writer/unsigned-little/big    type)]
    [(fixed-integer _ _ #t 'little 'big)    (fixed-integer-bits-writer/signed-little/big      type)]
    [(fixed-integer _ _ #f 'big    'big)    (fixed-integer-bits-writer/unsigned-big/big       type)]
    [(fixed-integer _ _ #t 'big    'big)    (fixed-integer-bits-writer/signed-big/big         type)]))

(define (fixed-integer-bits-writer/unsigned-little/little type)
  (match-let ([(fixed-integer size unit  _ _ _) type])
    (define nbits (* size unit))
     (define valid-value? (fixed-integer-predicate type))
     (define/contract (write-bits bits offset exact-integer)
       (-> bits? natural-number/c valid-value? bits?)
       (bits-store-byte bits (byte-spec nbits offset) exact-integer))
     write-bits))

(define (fixed-integer-bits-writer/signed-little/little   type)
  (match-let ([(fixed-integer size unit  _ _ _) type])
    (define nbits (* size unit))
    (define valid-value? (fixed-integer-predicate type))
    (define signed->unsigned (make-signed->unsigned nbits))
    (define/contract (write-bits bits offset exact-integer)
       (-> bits? natural-number/c valid-value? bits?)
       (bits-store-byte bits (byte-spec nbits offset) (signed->unsigned exact-integer)))
     write-bits))

(define (fixed-integer-bits-writer/unsigned-big/little type)
  (match-let ([(fixed-integer size unit  _ _ _) type])
    (define nbits (* size unit))
    (define valid-value? (fixed-integer-predicate type))
    (define reverse-bytes (make-byte-reverser size unit))
    (define/contract (write-bits bits offset exact-integer)
      (-> bits? natural-number/c valid-value? bits?)
      (bits-store-byte bits (byte-spec nbits offset) (reverse-bytes exact-integer)))
    write-bits))

(define (fixed-integer-bits-writer/signed-big/little type)
  (match-let ([(fixed-integer size unit  _ _ _) type])
    (define nbits (* size unit))
    (define valid-value? (fixed-integer-predicate type))
    (define signed->unsigned (make-signed->unsigned nbits))
    (define reverse-bytes (make-byte-reverser size unit))
    (define/contract (write-bits bits offset exact-integer)
      (-> bits? natural-number/c valid-value? bits?)
      (bits-store-byte bits (byte-spec nbits offset) (reverse-bytes (signed->unsigned exact-integer))))
    write-bits))

(define (fixed-integer-bits-writer/unsigned-little/big    type)
  (error "not implemented"))

(define (fixed-integer-bits-writer/signed-little/big      type)
  (error "not implemented"))

(define (fixed-integer-bits-writer/unsigned-big/big       type)
  (error "not implemented"))

(define (fixed-integer-bits-writer/signed-big/big         type)
  (error "not implemented"))


(define (make-byte-reverser size unit)
  (define byte-mask (sub1 (arithmetic-shift 1 unit)))
  (define shift (- unit))
  (define base (expt 2 unit))
  (define (reverse-bytes big-byte)
    (define reversed-bytes
      (for/fold ([big-byte big-byte] [byte-list '()] #:result (reverse byte-list))
          ([byte-index (in-range size)])
            (values (arithmetic-shift big-byte shift)
                    (cons (bitwise-and byte-mask big-byte) byte-list))))
    (for/sum ([byte reversed-bytes]
              [byte-index (in-naturals)])
      (arithmetic-shift byte (* byte-index unit))))
  reverse-bytes)

(define (make-unsigned->signed nbits)
  (define shift (- (sub1 nbits)))
  (define max-plus (expt 2 nbits))
  (define (unsigned->signed unsigned-integer)
    (if (zero? (arithmetic-shift unsigned-integer shift))
        unsigned-integer
      (add1 (-  unsigned-integer max-plus))))
  unsigned->signed)

(define (make-signed->unsigned nbits)
  (define max-plus (expt 2 nbits))
  (define (signed->unsigned signed-integer)
    (if (exact-nonnegative-integer? signed-integer)
        signed-integer
      (sub1 (+ max-plus signed-integer))))
  signed->unsigned)

;;
;; ... Fixed Length Strings 8859-1
;;
(struct fixed-string
  (length padding-character padding-side)
  #:transparent)


(define (padding-side? x)
  (and (symbol? x)
       (or (eq? 'left x)
           (eq? 'right x))))


(define (byte-char? x)
  (and (char? x)
       (byte? (char->integer x))))

(define (fixed-string-bits-reader type)
  (match-let ([(fixed-string length _ _) type])
    (λ (bits offset)
      (let ([bytes (make-bytes length)])
        (for ([i (in-range length)])
          (bytes-set! bytes i
                      (bits-load-byte bits (byte-spec bits-per-octet (+ offset (* i bits-per-octet))))))
        bytes))))

(define (fixed-string-bits-writer type)
  (match-let ([(fixed-string length padding-character padding-side) type])
    (let ([padding-byte (char->integer padding-character)])

      (define padding-byte (char->integer padding-character))

      ;; Store the bytes in the bits
      (define (store-bytes bits offset bytes)
        (for/fold ([bits bits])
            ([byte bytes]
             [i (in-naturals)])
          (bits-store-byte bits (byte-spec bits-per-octet (+ offset (* i bits-per-octet))) byte)))

      ;; Store the padding bytes in the bits
      (define (store-padding bits offset n)
        (for/fold ([bits bits])
            ([i (in-range n)])
          (bits-store-byte bits (byte-spec bits-per-octet (+ offset (* i bits-per-octet)))
                           padding-byte)))

      ;; Format an error message about the input length being to long for the type
      (define (bytes-length-error bytes)
        (format bytes-exceeds-fixed-string-length
          (bytes-length bytes)
          (fixed-string-length type)
          bytes
          type))

      ;; right padding variant writes the input, then the padding
      (match padding-side
        ['right
         (λ (bits offset bytes)
           (let ([n (bytes-length bytes)])
             (cond [(< n length)
                    (let ([bits (store-bytes bits offset bytes)])
                      (store-padding bits (+ offset (* n bits-per-octet)) (- length n)))]
                   [(= n length)
                    (store-bytes bits offset bytes)]
                   [(> n length)
                    (error (bytes-length-error bytes))])))]

        ;; left padding variant writes the padding, then the input
        ['left
         (λ (bits offset bytes)
           (let ([n (bytes-length bytes)])
             (cond [(< n length)
                    (let* ([m (- length n)]
                           [bits (store-padding bits offset m)])
                      (store-bytes bits (+ offset (* m bits-per-octet)) bytes))]
                   [(= n length)
                    (store-bytes bits offset bytes)]
                   [(> n length)
                    (error (bytes-length-error bytes))])))]))))


(define bytes-exceeds-fixed-string-length
  "The length of the input bytes (~a) exceeds the
permissible length of the `fixed-string` type (~a).
\tbytes: ~S
\tfixed string type: ~s")


;;
;; ... Fixed Length Arrays of Fixed Size Types
;;
(struct fixed-array
  (element-type length)
  #:transparent)


;;
;; ... Fixed Tuple of Fixed Size Types
;;
(struct fixed-tuple
  (components)
  #:transparent)


(struct fixed-tuple-super
  (type)
  #:transparent)


(define (fixed-tuple-supers fixed-tuple)
  (for/list ([component (fixed-tuple-components)]
             #:when (fixed-tuple-super? component))
    component))


(define (fixed-tuple-super-count fixed-tuple)
  (for/sum ([component (fixed-tuple-components fixed-tuple)]
            #:when (fixed-tuple-super? component))
    1))


(define (fixed-tuple-element-count fixed-tuple)
  (for/sum ([component (fixed-tuple-components fixed-tuple)])
    (match component
      [(fixed-tuple-super super-type) (fixed-tuple-element-count super-type)]
      [(? fixed-size-type?) 1])))


(define (fixed-tuple-super-ref fixed-tuple index)
  (list-ref (fixed-tuple-supers fixed-tuple) index))


(define (fixed-tuple-elements tuple)
  (define (recur components accum)
    (if (null? components) (reverse accum)
      (match components
        [(list (fixed-tuple-super (fixed-tuple super-components)) more-components ...)
         (recur (append super-components more-components) accum)]
        [(list component more-components ...)
         (recur more-components (cons component accum))])))
  (recur (fixed-tuple-components tuple) '()))


(define (fixed-tuple-element-offsets-in-bits fixed-tuple)
  (for/fold ([offsets '(0)] #:result (reverse offsets))
      ([element (butlast (fixed-tuple-elements fixed-tuple))])
    (cons (+ (car offsets) (fixed-size-in-bits element))
          offsets)))


;;
;; ... Fixed Record of Fixed Size Types
;;
(struct fixed-record
  (components)
  #:transparent)


(struct fixed-record-field
  (name type)
  #:transparent)


(struct fixed-record-super
  (name type)
  #:transparent)


(define (fixed-record-component? arg)
  (or (fixed-record-field? arg)
      (fixed-record-super? arg)))


(define (fixed-record-direct-supers record)
  (for/list ([component (fixed-record-components record)]
             #:when (fixed-record-super? component))
    component))


(define (fixed-record-direct-super-count record)
  (for/sum ([component (fixed-record-components record)]
            #:when (fixed-record-super? component))
    1))


(define (fixed-record-supers record)
  (define (recur components accum)
    (match components
      [(list (and super (fixed-record-super name (fixed-record components))) more-components ...)
       (recur (append components more-components) (cons super accum))]
      [(list _ more-components ...) (recur more-components accum)]
      ['() (reverse accum)]))
  (recur (fixed-record-components record) '()))


(define (fixed-record-super-count record)
  (length (fixed-record-supers record)))


(define (fixed-record-fields record)
  (define (recur components accum)
    (match components
      [(list (and field (? fixed-record-field?)) more-components ...)
       (recur more-components (cons field accum))]
      [(list (fixed-record-super name (fixed-record super-components)) more-components ...)
       (recur (append super-components more-components) accum)]
      ['() (reverse accum)]))
  (recur (fixed-record-components record) '()))


(define (fixed-record-field-count record)
  (for/sum ([component (fixed-record-components record)])
    (match component
      [(? fixed-record-field?) 1]
      [(fixed-record-super _ type)
       (fixed-record-field-count type)])))


(define (fixed-record-elements record)
  (define (recur components accum)
    (match components
      [(list (and field (? fixed-record-field?)) more-components ...)
       (recur more-components (cons field accum))]
      [(list (fixed-record-super _ (fixed-record components)) more-components ...)
       (recur (append components more-components) accum)]
      ['() (reverse accum)]))
  (recur (fixed-record-components record) '()))


(define (unique-field-names? components)
  (unique-names?
   (component-field-names components)))


(define (unique-super-names? components)
  (unique-names? (component-super-names components)))


(define/contract (component-field-names components)
  (-> (listof fixed-record-component?) (listof symbol?))
  (define (recur components accum)
    (match components
      [(list (fixed-record-field name _) more-components ...)
       (recur more-components (cons name accum))]

      [(list (fixed-record-super _ (fixed-record components)) more-components ...)
       (recur (append components more-components) accum)]

      ['() (reverse accum)]))
  (recur components '()))


(define (component-super-names components)
  (define (recur components accum)
    (match components
      [(list (fixed-record-super name (fixed-record components)) more-components ...)
       (recur (append components more-components) (cons name accum))]

      [(list _ more-components ...)
       (recur more-components accum)]

      ['() (reverse accum)]))
  (recur components '()))

(define (fixed-record-field-names record)
  (map fixed-record-field-name (fixed-record-fields record)))




(define (enum-type-guard type named-values name)
  (let ([pred? (fixed-integer-predicate type)])
    (if (for/and ([value (map cdr named-values)])
          (pred? value))
        (values type named-values)
      (error
       (format enum-type-value-error
         name
         type
         (filter (λ (named-value) (pred? (cdr named-value))) named-values)
         (filter (λ (named-value) (not (pred? (cdr named-value)))) named-values))))))


(define enum-type-value-error
  "When constructing the an enum type, not all of the enum values are members of the value type.
\t    value type: ~a
\t  valid values: ~a
\tinvalid values: ~a")


(struct enum-type
  (value-type named-values)
  #:guard enum-type-guard)


(define (enum-type-names type)
  (map car (enum-type-named-values type)))


(define (enum-type-values type)
  (map cdr (enum-type-named-values type)))


(define (enum-type-value-predicate type)
  (let ([values (apply set (enum-type-values type))])
    (λ (x) (set-member? values x))))


(define (enum-type-name-predicate type)
  (let ([names (apply set (enum-type-names type))])
    (λ (x) (set-member? names x))))


(define (fixed-size-type? arg)
  (match arg
    [(or (? fixed-integer?)
         (? fixed-array?)
         (? fixed-tuple?)
         (? fixed-record?)
         (? fixed-string?)
         (? enum-type?))
     #t]
    [_ #f]))


(define (fixed-size-in-bits arg)
  (match arg
    [(fixed-integer size unit _ _ _) (* size unit)]
    [(fixed-array element-type length) (* length (fixed-size-in-bits element-type))]
    [(fixed-tuple (list components ...))
     (for/sum ([component components])
       (match component
         [(fixed-tuple-super super-type) (fixed-size-in-bits super-type)]
         [fixed-element-type (fixed-size-in-bits fixed-element-type)]))]
    [(fixed-record (list components ...))
     (for/sum ([component components])
       (match component
         [(fixed-record-super _ type)
          (fixed-size-in-bits type)]
         [(fixed-record-field _ type)
          (fixed-size-in-bits type)]))]
    [(fixed-string length _ _) (* length 8)]
    [(enum-type value-type _) (fixed-size-in-bits value-type)]))


(define (discriminated-union-guard discriminant variants name)
  (let ([discriminant-names (enum-type-names discriminant)]
        [variant-names (map car variants)])
    (cond [(> (length discriminant-names) (length variant-names))
           (error (format discriminated-union-missing-variants-error
                    discriminant
                    variant-names
                    (set-subtract (list->set discriminant-names) (list->set variant-names))))]
          [(< (length discriminant-names) (length variant-names))
           (error (format discriminated-union-extra-variants-error
                    discriminant
                    (set-subtract (list->set variant-names) (list->set discriminant-names))))]
          [else (values discriminant variants)])))


(define discriminated-union-missing-variants-error
  "Some variants are missing for a discriminated union:
\t     discriminant: ~a
\t    variant names: ~a
\t missing variants: ~a")


(define discriminated-union-extra-variants-error
  "Some variants are missing for a discriminated union:
\t     discriminant: ~a
\t   extra variants: ~a")


(struct discriminated-union
  (discriminant variants))


(struct dynamically-sized-array
  (length-type
   element-type
   minimum-length
   maximum-length))


(define (dynamically-sized-type? x)
  (match x
    [(? discriminated-union?) #t]
    [_ #f]))


(define (minimum-size-in-bits type)
  (if (fixed-size-type? type)
      (fixed-size-in-bits type)
    (minimum-dynamic-size-in-bits type)))


(define (minimum-dynamic-size-in-bits type)
  (match type
    [(discriminated-union discriminant variants)
     (+ (fixed-size-in-bits discriminant)
        (apply min (map (λ (variant) (minimum-size-in-bits (cdr variant))) variants)))]
    [(dynamically-sized-array length-type element-type minimum-length maximum-length)
     (+ (minimum-size-in-bits length-type)
        (* minimum-length (minimum-size-in-bits element-type)))]))


(define (maximum-size-in-bits type)
  (if (fixed-size-type? type)
      (fixed-size-in-bits type)
    (maximum-dynamic-size-in-bits type)))


(define (maximum-dynamic-size-in-bits type)
  (match type
    [(discriminated-union discriminant variants)
     (+ (fixed-size-in-bits discriminant)
        (apply max (map (λ (variant) (maximum-size-in-bits (cdr variant))) variants)))]
    [(dynamically-sized-array length-type element-type minimum-length maximum-length)
     (+ (minimum-dynamic-size-in-bits length-type)
        (* maximum-length (maximum-size-in-bits element-type)))]))


(define (wire-format? x)
  (or (fixed-size-type? x)
      (dynamically-sized-type? x)))


(define (unique-names? names)
  (unsafe-unique? names symbol<?))


(define (unique-numbers? numbers)
  (unsafe-unique? numbers <))


(define (unique-name-integer-pairs? x)
  (and (list? x)
       (for/and ([item x])
         (and (pair? item)
              (symbol? (car item))
              (exact-integer? (cdr item))))
       (unique-names? (map car x))
       (unique-numbers? (map cdr x))))
