#lang errortrace racket

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
          (check-equal? (fixed-integer-bit-endianness uint32) 'little))
        (it "defines  a predicate"
          (check-true (uint32? 0))
          (check-true (uint32? (sub1 (expt 2 32))))
          (check-false (uint32? -1))
          (check-false (uint32? (expt 2 32)))))
      (context "with a 16 bit signed integer type defined"
        (integer/wire int14
          #:signed? #t
          #:byte-count 2
          #:bits-per-byte 7
          #:byte-order big
          #:bit-order little)
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
          (check-equal? (fixed-integer-bit-endianness int14) 'little)))))


  (describe "array/wire"
    (it "defines an array type"
      (context "with an array type defined"
        (integer/wire int64
          #:signed? #t
          #:byte-count 8)
        (array/wire int64x4 int64 4)
        (define x0 0)
        (define x1 1)
        (define x2 2)
        (define x3 3)
        (define xs (make-int64x4 x0 x1 x2 x3))

        (it "defines a constructor and a predicate for the array type"
          (check-true (int64x4? xs)))

        (it "defines element reader for the array type"
         (check-equal? (int64x4-ref xs 0) x0)
         (check-equal? (int64x4-ref xs 1) x1)
         (check-equal? (int64x4-ref xs 2) x2)
         (check-equal? (int64x4-ref xs 3) x3))

        (it "defines persistent element writers for the array type"
          (define index 2)
          (define new-element-value 11)
          (check-true (int64x4? (int64x4-set xs 2 11)))
          (check-equal? (int64x4-ref (int64x4-set xs index new-element-value) index) new-element-value)
          (check-equal? (int64x4-ref xs index) x2))

        (it "defines a function to convert from a list to an array"
          (check-equal? xs (list->int64x4 (list x0 x1 x2 x3))))

        (it "defines a function to convert an arry to a list"
          (check-equal? (int64x4->list xs) (list x0 x1 x2 x3))))))

  (describe "struct/wire"
    (context "with a simple record defined"
      (integer/wire coordinate #:byte-count 8 #:signed? #t)
      (array/wire coordinates coordinate 3)
      (integer/wire id-type)

      (struct/wire point
        ([coords : coordinates]
         [id     : id-type]))

      (check-true (fixed-record? point))

      (define p (make-point (make-coordinates 100 200 300) 10))
      (check-equal? (point-id p) 10)
      (check-equal? (point-coords p) (make-coordinates 100 200 300)))

    (describe "struct/wire"
      (context "with a hierarchy defined"
        (integer/wire id-type)
        (integer/wire coord
          #:byte-count 8
          #:signed? #t)
        (array/wire coords coord 3)

        (struct/wire entity-type
          ([id : id-type]))

        (check-true (fixed-record? entity-type))

        #;
        (struct/wire geometric-entity
          ([entity   : entity-type #:splice]
           [centroid : coords]))

        #;
        (check-true (fixed-record? geometric-entity))

        #;
        (struct/wire point
          ([geometric-entity : geometric-entity]))

        #;
        (check-true (fixed-record? point))))))
