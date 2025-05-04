#lang racket

(module+ test
  (require spork/wire-formats spork/bits rackunit rackunit/spec)

  (describe "endian?"
    (it "is a predicate that recognizes endian specifiers"
      (it "recognizes 'little"
        (check-true (endian? 'little)))
      (it "recognizes 'big"
        (check-true (endian? 'big)))
      (it "doesn't recognize anything else"
        (check-false (endian? 'potato)))))

  (describe "fixed integer types"
    (describe "fixed-integer"
      (it "constructs a fixed integer type"
        (define size 32)
        (define unit 1)
        (define signed #f)
        (define byte-endianness 'little)
        (define bit-endianness 'little)
        (define uint32 (fixed-integer size unit signed byte-endianness bit-endianness))
        (check-true (fixed-integer? uint32))
        (it "accepts a natural-number/c as the first argument to specify the size"
          (check-equal? (fixed-integer-size uint32) size))
        (it "does not accept values that are not natural-number/c for the first argument"
          (define bad-size -1)
          (check-exn exn:fail? (thunk (fixed-integer bad-size unit signed byte-endianness bit-endianness))))
        (it "accepts an exact-positive-integer? as the second argument specifying the unit"
          (check-equal? (fixed-integer-unit uint32) unit))
        (it "only accepts an exact-positive-integer? as the second argument specifying the unit"
          (define bad-unit 0)
          (check-exn exn:fail? (thunk (fixed-integer size bad-unit signed byte-endianness bit-endianness))))
        (it "accepts a boolean? as the second argument specifying the signedness"
          (check-equal? (fixed-integer-signed? uint32) signed))
        (it "only accepts a boolean? as the third argument specifying the signedness"
          (define bad-signed 8)
          (check-exn exn:fail? (thunk (fixed-integer size unit bad-signed byte-endianness bit-endianness))))
        (it "accepts an endian? value for fourth argument specifying the endianness"
          (check-equal? (fixed-integer-endianness uint32) byte-endianness))
        (it "only accepts an endian? value for fourth argument specifying the endianness"
          (define bad-byte-endianness 'medium)
          (check-exn exn:fail? (thunk (fixed-integer size unit signed bad-byte-endianness bit-endianness))))
        (it "accepts an endian? value for fifth argument specifying the bit-endianness"
          (check-equal? (fixed-integer-bit-endianness uint32) bit-endianness))
        (it "only accepts an endian? value for fifth argument specifying the bit-endianness"
          (define bad-bit-endianness 'medium)
          (check-exn exn:fail? (thunk (fixed-integer size unit signed byte-endianness bad-bit-endianness))))))

    (describe "fixed-unsigned"
      (it "it constructs fixed unsigned integer types"
        (check-true (fixed-integer? (fixed-unsigned 8 #:endianness big)))
        (check-false (fixed-integer-signed? (fixed-unsigned 8 #:endianness big)))
        (check-equal? (fixed-integer-endianness (fixed-unsigned 8)) little)
        (check-equal? (fixed-integer-endianness (fixed-unsigned 8 #:endianness little)) little)))

    (describe "fixed-signed"
      (it "it constructs fixed signed integer types"
        (check-true (fixed-integer? (fixed-signed 8 #:endianness 'big)))

        (check-true (fixed-integer-signed? (fixed-signed 8 #:endianness 'big))))
      (it "it producess little endian values by default"
        (check-equal? (fixed-integer-endianness (fixed-signed 8 )) 'little))

      (it "can optionally have the endianness specified"
        (check-equal? (fixed-integer-endianness (fixed-signed 8 #:endianness 'little)) 'little)))

    (describe "fixed-integer-bits-writer"
      (it "builds a function to write a fixed integer value to bits"
        (context "with an unsigend little endian fixed integer type"
          (define uint16-little (fixed-unsigned 2 #:endianness little))
          (define read-bits/uint16-little (fixed-integer-bits-reader uint16-little))
          (define write-bits/uint16-little (fixed-integer-bits-writer uint16-little))
          (it "returns a function that accepts an appropriately sized exact unsigned integer"
            (define value (sub1 (expt 2 16)))
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/uint16-little input-bits offset value))
            (check-equal? (read-bits/uint16-little output-bits offset)  value))
          (it "returns a function that won't accept negative values"
            (define value -1)
            (define input-bits (make-bits 512))
            (check-exn exn:fail? (thunk (write-bits/uint16-little input-bits 0 value))))
          (it "returns a function that won't accept values that too big"
            (define value (expt 2 16))
            (define input-bits (make-bits 512))
            (check-exn exn:fail? (thunk (write-bits/uint16-little input-bits 0 value))))
          (it "returns a function that won't accept bits that are too small"
            (define input-bits (make-bits 8))
            (check-exn exn:fail? (thunk (write-bits/uint16-little input-bits 0 0))))
          (it "returns a function that won't accept an offset that is too large"
            (define input-bits (make-bits 512))
            (define offset 500)
            (check-exn exn:fail? (thunk (write-bits/uint16-little input-bits offset 0)))))
        (context "with an unsigned big endian fixed integer type"
          (define uint16-big (fixed-unsigned 2 #:endianness big))
          (define read-bits/uint16-big (fixed-integer-bits-reader uint16-big))
          (define write-bits/uint16-big (fixed-integer-bits-writer uint16-big))
          (it "returns a function that acceps an appropriately sized exact unsigned integer"
            (define value (sub1 (expt 2 16)))
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/uint16-big input-bits offset value))
            (check-equal?  (read-bits/uint16-big output-bits offset) value)))
        (context "with a signed little endian fixed integer type"
          (define int16-little (fixed-signed 2 #:endianness little))
          (define read-bits/int16-little (fixed-integer-bits-reader int16-little))
          (define write-bits/int16-little (fixed-integer-bits-writer int16-little))
          (it "returns a function that accepts appropriately sized exact integers"
            (define value (sub1 (expt 2 (sub1 16))))
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-little input-bits offset value))
            (check-equal? value (read-bits/int16-little output-bits offset)))
          (it "returns a function that accepts appropriately sized negative exact-intgers"
            (define value (- (sub1 (expt 2 (sub1 16)))))
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-little input-bits offset value))
            (check-equal? (read-bits/int16-little output-bits offset) value))
          (it "returns a function that accepts 0"
            (define value 0)
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-little input-bits offset value))
            (check-equal? (read-bits/int16-little output-bits offset) value))

          )
        (context "with a signed big endian fixed integer type"
          (define int16-big (fixed-signed 2 #:endianness big))
          (define read-bits/int16-big (fixed-integer-bits-reader int16-big))
          (define write-bits/int16-big (fixed-integer-bits-writer int16-big))
          (it "returns a function that accepts appropriately sized exact exact integer"
            (define value (sub1 (expt 2 15)))
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-big input-bits offset value))
            (check-equal? (read-bits/int16-big output-bits offset) value))
          (it "returns a function that accepts appropriately sized negative integers"
            (define value (- (sub1 (expt 2 15))))
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-big input-bits offset value))
            (check-equal? (read-bits/int16-big output-bits offset) value))
          (it "returns a function that accepts 0"
            (define value 0)
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-big input-bits offset value))
            (check-equal? (read-bits/int16-big output-bits offset) value))
          (it "returns a function that accepts -1"
            (define value -1)
            (define input-bits (make-bits 512))
            (define offset 32)
            (define output-bits (write-bits/int16-big input-bits offset value))
            (check-equal? (read-bits/int16-big output-bits offset) value))))))

  (describe "fixed string types"
    (context "with a defined parameters for a fixed string type"
      (define len 10)
      (define padding-character #\space)
      (define padding-side 'right)

      (describe "fixed-string"
        (it "constructs a fixed string type"
          (check-true (fixed-string? (fixed-string len padding-character padding-side)))))

      (context "with a defined fixed string type"
        (define string10 (fixed-string len padding-character padding-side))
        (define write-string10 (fixed-string-bits-writer string10))
        (define read-string10 (fixed-string-bits-reader string10))
        (describe "fixed-string-make-writer/bytes"
          (it "builds a function to write bytes to bits"
            (define offset 13)
            (define input-bytes #"abc123")
            (define bits (write-string10 (make-bits 256) offset input-bytes))
            (define output-bytes (read-string10 bits offset))
            (define expected-bytes (bytes-append
                                    input-bytes
                                    (make-bytes (- len (bytes-length input-bytes))
                                                (char->integer #\space))))
            (check-equal? output-bytes expected-bytes))))
      (context "with a defined fixed string type with left padding"
        (define padding-side 'left)
        (define string10 (fixed-string len padding-character padding-side))
        (define write-string10 (fixed-string-bits-writer string10))
        (define read-string10 (fixed-string-bits-reader string10))
        (it "builds a function to write bytes to bits"
          (define input-bytes #"abc123")
          (define offset 32)
          (define bits (write-string10 (make-bits 256) offset input-bytes))
          (define output-bytes (read-string10 bits offset))
          (define expected-bytes (bytes-append
                                  (make-bytes (- len (bytes-length input-bytes))
                                              (char->integer #\space))
                                  input-bytes))
          (check-equal? output-bytes expected-bytes))
        (it "builds a function that only accepts bytes"
          check-exn exn:fail (thunk (write-string10 "abc123")))
        (it "builds a function that only accepts bytes of valid lengths"
          (check-exn exn:fail? (thunk (write-string10 (make-bytes (add1 len)))))))))



  (describe "fixed-array"
    (it "constructs a fixed array type"
      (define uint16 (fixed-integer 16 1 #f 'little 'little))
      (define len 3)
      (define uint16x3 (fixed-array uint16 len))
      (check-true (fixed-array? uint16x3))

      (it "constructs a fixed-size-type?"
        (check-true (fixed-size-type? uint16x3)))

      (it "accepts a fixed-size-type? as the first argument for the element-type"
        (check-true (fixed-size-type? uint16))
        (check-equal? (fixed-array-element-type uint16x3) uint16))

      (it "only accepts a fixed-size-type? as the first argument for the element-type"
        (check-exn exn:fail? (thunk (fixed-array 'bad-element-type len))))

      (it "accepts a natural-number as the second argument, specifying the length of the fixed array"
        (check-equal? (fixed-array-length uint16x3) len))

      (it "only accepts natural numbers for specifying the number of elements"
        (check-exn exn:fail? (thunk (fixed-array uint16 -1))))

      (it "does accept 0 for the number of elements"
        (check-true (fixed-array? (fixed-array uint16 0))))))

  (describe "fixed-tuple"
    (it "constructs a fixed-tuple? from a list of fixed-size-type? and fixed-tuple-super? elements"
      (define uint16 (fixed-integer 16 1 #f 'little 'little))
      (define uint64 (fixed-integer 64 1 #f 'little 'little))
      (define element-count 4)
      (define uint16x4 (fixed-array uint16 element-count))
      (define tuple1 (fixed-tuple (list uint16 uint64 uint16x4)))
      (define tuple2
        (fixed-tuple
         (list
          (fixed-tuple-super tuple1)
          uint16
          (fixed-tuple-super tuple1)
          tuple1)))

      (check-true (fixed-tuple? tuple1))
      (check-true (fixed-tuple? tuple2))

      (it "splices the elements of its supers into its own elements"
        (check-equal? (fixed-tuple-element-count tuple1) 3)
        (check-equal? (fixed-tuple-super-count tuple1) 0)
        (check-equal? (fixed-tuple-super-count tuple2) 2)
        (check-equal? (fixed-tuple-element-count tuple2)
                      (+ 2 (* 2 (fixed-tuple-element-count tuple1)))))
      (it "accepts a list without any components"
        (check-true (fixed-tuple? (fixed-tuple '()))))))

  (describe "fixed-record"
    (it "constructs a fixed-record? from a list of fixed-record-component? values"
      (define int64 (fixed-integer 64 1 #t 'little 'little))
      (define uint64 (fixed-integer 64 1 #f 'little 'little))
      (define record-with-id
        (fixed-record
         (list (fixed-record-field 'id uint64))))

      (define point
        (fixed-record
         (list (fixed-record-field 'x int64)
               (fixed-record-field 'y int64)
               (fixed-record-super 'id-provider record-with-id))))

      (define line-segment
        (fixed-record
         (list (fixed-record-field 'start  point)
               (fixed-record-field 'stop point)
               (fixed-record-super 'id-provider record-with-id))))

      (check-true (fixed-record? record-with-id))
      (check-equal? (fixed-record-supers record-with-id) '())
      (check-equal? (fixed-record-super-count record-with-id) 0)

      (check-true (fixed-record? point))
      (check-equal? (fixed-record-super-count point) 1)
      (check-equal? (fixed-record-supers point)
                    (list (fixed-record-super 'id-provider record-with-id)))

      (check-true (fixed-record? line-segment))
      (check-equal? (fixed-record-super-count line-segment) 1)
      (check-equal? (fixed-record-field-count line-segment) 3))

    (context "with a hierarchy of fixed-records defined"
      (define super0
        (fixed-record
         (list (fixed-record-field 'f00 uint16-little)
               (fixed-record-field 'f01 uint32-little))))

      (define super1a
        (fixed-record
         (list (fixed-record-super 'super0 super0)
               (fixed-record-field 'f1a0 uint64-little)
               (fixed-record-field 'f1a1 uint8-little))))

      (define super1b
        (fixed-record
         (list (fixed-record-field 'f1b0 uint8-little)
               (fixed-record-field 'f1b1 (fixed-array uint8-little 2)))))

      (define record
        (fixed-record
         (list (fixed-record-super 'super1a super1a)
               (fixed-record-field 'f uint8-little)
               (fixed-record-super 'super1b super1b))))

      (check-equal? (fixed-record-super-count record) 3)

      (check-equal? (length (fixed-record-supers record)) 3)

      (check-equal?
       (car (fixed-record-supers record))
       (fixed-record-super 'super1a super1a))

      (check-equal?
       (cadr (fixed-record-supers record))
       (fixed-record-super 'super0 super0))

      (check-equal?
       (caddr (fixed-record-supers record))
       (fixed-record-super 'super1b super1b))

      (check-equal?
       (fixed-record-fields record)
       (list
        (fixed-record-field 'f00 (fixed-integer 2 8 #f 'little 'little))
        (fixed-record-field 'f01 (fixed-integer 4 8 #f 'little 'little))
        (fixed-record-field 'f1a0 (fixed-integer 8 8 #f 'little 'little))
        (fixed-record-field 'f1a1 (fixed-integer 1 8 #f 'little 'little))
        (fixed-record-field 'f (fixed-integer 1 8 #f 'little 'little))
        (fixed-record-field 'f1b0 (fixed-integer 1 8 #f 'little 'little))
        (fixed-record-field
         'f1b1
         (fixed-array (fixed-integer 1 8 #f 'little 'little) 2)))))
    (it "fails when field names are duplicated"
      (check-exn
       exn:fail?
       (thunk
        (fixed-record
         (list (fixed-record-field 'x uint8-little)
               (fixed-record-field 'x uint8-little))))))
    (it "fails when field names are duplicated in supers"
      (check-exn exn:fail?
                 (thunk (fixed-record
                         (list (fixed-record-super 'super (fixed-record (fixed-record-field 'x uint8-little)))
                               (fixed-record-field 'x uint16-little))))))

    (it "fails when super names are duplicated"
      (check-exn exn:fail?
                 (thunk (fixed-record
                         (list (fixed-record-super 'super (fixed-record '()))
                               (fixed-record-super 'super (fixed-record '()))))))))

  (describe "fixed record types"
    (describe "fixed record constructor"
      (context "with a fixed record type"
        (define point-type (fixed-record
                            (list (fixed-record-field 'x  int64-little)
                                  (fixed-record-field 'y  int64-little)
                                  (fixed-record-field 'id uint8-little))))
        (define make-point (fixed-record-bits-constructor point-type))
        (define point-x  (fixed-record-bits-field-reader point-type  'x))
        (define point-y  (fixed-record-bits-field-reader point-type  'y))
        (define point-id (fixed-record-bits-field-reader point-type 'id))
        (define x   11)
        (define y   12)
        (define id 127)
        (check-true (procedure? make-point))
        (define point (make-point x y id))

        (check-equal? (point-x  point 0)  x)
        (check-equal? (point-y  point 0)  y)
        (check-equal? (point-id point 0) id))))

  (describe "fixed-size-type?"
    (it "is a predicate that recognizes fixed size types"
      (it "recognizes fixed integer types"
        (check-true (fixed-size-type? (fixed-integer 64 1 #t 'little 'little))))

      (it "recognizes fixed array types"
        (define uint8 (fixed-integer 8 1 #f 'little 'little))
        (define uint8x4 (fixed-array uint8 4))
        (check-true (fixed-size-type? uint8))
        (check-true (fixed-size-type? uint8x4)))

      (it "recognizes fixed tuple types"
        (define tuple (fixed-tuple
                       (list (fixed-integer 64 1 #t 'little 'little)
                             (fixed-integer 8 1 #t 'little 'little))))
        (check-true (fixed-size-type? tuple)))))

  (describe "enum-type"
    (define value-type uint8-little)
    (define values '((a . 1) (b . 2) (c . 3)))
    (define abc (enum-type value-type values))

    (it "constructs an enum type"
      (check-true (enum-type? abc)))

    (it "constructs a fixed-size-type?"
      (check-true (fixed-size-type? abc)))

    (it "has a size that is the same as its value type"
      (check-equal? (fixed-size-in-bits abc)
                    (fixed-size-in-bits value-type))))

  (describe "discriminated-union?"
    (define value-type uint8-little)
    (define values '((a . 1) (b . 2) (c . 3)))
    (define abc (enum-type value-type values))
    (define type-a (fixed-record (list (fixed-record-field 'field1 uint8-little))))
    (define type-b (fixed-record (list (fixed-record-field 'field1 uint16-little))))
    (define type-c (fixed-record (list (fixed-record-field 'field1 uint32-little))))
    (define some-type (discriminated-union abc
                        `((a . ,type-a)
                          (b . ,type-b)
                          (c . ,type-c))))
    (it "constructs a discriminated-union"
      (check-true (discriminated-union? some-type))))

  (describe "fixed-size-in-bits"
    (it "returns the size of fixed size types in bits"
      (context "with a fixed integer type defined"
          (define size 4)
          (define unit 8)
          (define uint32 (fixed-integer size unit #t 'little 'little))
        (it "returns the size of fixed integer types in bits"
          (check-equal? (fixed-size-in-bits uint32)
                        (* size unit)))
        (it "returns the size of fixed array types in bits"
          (define array-length 3)
          (check-equal? (fixed-size-in-bits (fixed-array uint32 3))
                        (* size unit array-length))
          (check-equal? (fixed-size-in-bits (fixed-array uint32 0)) 0))
        (it "returns the size of fixed record types in bits"
          (check-equal?
           (fixed-size-in-bits
            (fixed-record
             (list (fixed-record-field 'value (fixed-integer 8 1 #f 'little 'little)))))
           8))
        (it "returns the size of fixed string types in bits"
          (check-equal?
           (fixed-size-in-bits
            (fixed-string 10 #\space 'right))
           80))))))
