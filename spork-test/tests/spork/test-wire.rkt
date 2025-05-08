#lang racket

(module+ test
  (require
   spork/wire
   spork/wire-formats
   rackunit rackunit/spec)

  (describe "enum/wire"
   (it "defines enumerated types"
     (enum/wire thing uint8-big
       ([one 1]
        [two 2]))

     (check-true (thing? one))
     (check-true (thing? two))

     (check-true (thing-value? 1))
     (check-true (thing-value? 2))

     (check-equal? (thing-name->value one) 1)
     (check-equal? (thing-name->value two) 2)
     (check-equal? (thing-value->name 1) one)
     (check-equal? (thing-value->name 2) two)

     (check-true (enum-type? thing))

     (check-equal? (enum-type-value-type thing) uint8-big)))

  (describe "integer/wire"
    (it "defines an integer type"
      (context "with an integer type defined without any options"
        (integer/wire uint32)
        (check-true (fixed-integer? uint32))
        (it "defines unsigned integer types by default"
          (check-false (fixed-integer-signed? uint32)))
        (it "defines 4 byte integer types by default"
          (check-equal? (fixed-integer-size uint32) 4))
        (it "defineds types with 8 bits per byte by default"
          (check-equal? (fixed-integer-unit uint32) 8))
        (it "defines integer types with little endian byte order by default"
          (check-equal? (fixed-integer-endianness uint32) 'little))
        (it "defines integer types with little endian bit order by default"
          (check-equal? (fixed-integer-bit-endianness uint32) 'little)))
      (context "with a 16 bit signed integer type defined"
        (integer/wire int14
          #:signed? #t
          #:byte-count 2
          #:bits-per-byte 7
          #:byte-order big
          #:bit-order big)
        (check-true (fixed-integer? int14))
        (it "can optionally specify the signedness"
          (check-true (fixed-integer-signed? int14)))
        (it "can optionally specify the byte count"
          (check-equal? (fixed-integer-size int14) 2))
        (it "can optionally specify the number of bits per byte"
          (check-equal? (fixed-integer-unit int14) 7))
        (it "can optionally specify the byte-order"
          (check-equal? (fixed-integer-endianness int14) 'big))
        (it "can optionally specify the bit-order "
          (check-equal? (fixed-integer-bit-endianness int14) 'big)))))

  (describe "struct/wire"
    (void)))
