#lang racket

(module+ test
  (require spork/bits rackunit rackunit/spec)

  (describe "bits"
    (describe "make-bits"
      (it "makes bits given the number of bits"
        (define n 16)
        (define bits (make-bits n))
        (check-true (bits? bits))
        (check-equal? n (bits-size bits))
        (check-true (bits-zeros? bits)))

      (it "takes an optional argument with the initial data"
        (define n 16)
        (define bits (make-bits n (sub1 (expt 2 16))))
        (check-true (bits? bits))
        (check-true (bits-ones? bits)))

      (it "does not accept sizes less than zero"
        (check-exn exn:fail? (thunk (make-bits -3))))

      (it "does not accept negative values for the initial data"
        (check-exn exn:fail? (thunk (make-bits 16 -3))))

      (it "does not accept initial data that is too big for the number of bits"
        (check-exn exn:fail? (thunk (make-bits 16 (expt 2 17))))))

    (describe "bits-ref"
      (it "references bit in some bits"
        (define bits (make-bits 16 (arithmetic-shift 1 8)))
        (check-equal? (bits-ref bits 8) 1))

      (it "does not accept indices that are negative or too large"
        (define bits (make-bits 16))
        (check-exn exn:fail? (thunk (bits-ref bits -1)))
        (check-exn exn:fail? (thunk (bits-ref bits 16)))))

    (describe "bits-set"
      (it "sets a bit in some bits persistently"
        (define bits (make-bits 16))
        (define new-bits (bits-set bits 8 1))
        (check-equal? (bits-ref new-bits 8) 1))

      (it "does not accept indices that are out of range"
        (define bits (make-bits 16))
        (check-exn exn:fail? (thunk (bits-ref bits -1)))
        (check-exn exn:fail? (thunk (bits-ref bits 18)))))

    (describe "bits-load-byte and bits-store-byte"
      (it "loads an unsigned byte from bits"
        (define value (random 4294967087))
        (define spec (byte-spec 32 128))
        (define bits (bits-store-byte (make-bits 512) spec value))
        (check-equal? (bits-load-byte bits spec) value))))

  (describe "byte-spec"
    (it "is a specification of a byte"
      (define spec (byte-spec 16 32))
      (check-true (byte-spec? spec))
      (check-equal? (byte-spec-size spec) 16)
      (check-equal? (byte-spec-position spec) 32))

    (it "does NOT accept negative sizes"
      (check-exn exn:fail? (thunk (byte-spec -10 16))))

    (it "does NOT accept negative positions"
      (check-exn exn:fail? (thunk (byte-spec 32 -16))))))
