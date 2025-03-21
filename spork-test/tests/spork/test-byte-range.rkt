#lang racket

(module+ test
  (require spork/byte-range rackunit rackunit/spec)
  (describe "byte-range"
    (it "accepts two bytes in ascending orderand returns a byte-range"
      (define range (byte-range 65 90))
      (check-true (byte-range? range)))

    (it "accepts two bytes that have the same value"
      (check-not-exn (thunk (byte-range 65 65))))

    (it "does NOT accept bytes that are in descending order"
      (check-exn exn:fail:contract? (thunk (byte-range 90 65))))

    (it "does NOT accept arguments with different types"
      (check-exn exn:fail:contract? (thunk (byte-range "elephant" 'seven)))))

  (describe "byte-range?"
    (it "is a predicate recognizing byte ranges"
      (check-true (byte-range? (byte-range 65 90))))

    (it "does not recognize values of other types"
      (check-false (byte-range? 88))))

  (context "with lower and upper bounds and a byte range defined "
    (define lower 65)
    (define upper 90)
    (define range (byte-range lower upper))

    (describe "byte-range-lower"
      (it "returns the lower bound of a byte range"
        (check-equal? lower (byte-range-lower range))))

    (describe "byte-range-upper"
      (it "returns the upper bound of a byte range"
        (check-equal? upper (byte-range-upper range))))

    (describe "byte-range-contains?"
      (it "returns true if the input byte is in the input byte range"
        (check-true (byte-range-contains range 70)))

      (it "returns false if the input byte is out of the range"
        (check-false (byte-range-contains range 64))
        (check-false (byte-range-contains range 91)))

      (it "has an inclusive lower bound"
        (check-true (byte-range-contains range lower)))

      (it "has an inclusive upper bound"
        (check-true (byte-range-contains range upper)))

      (it "does not accept different types for either argument"
        (check-exn exn:fail? (thunk (byte-range-contains "Hello, World!" lower)))
        (check-exn exn:fail? (thunk (byte-range-contains range -1))))))

  (describe "string->byte-range"
    (it "accepts a string and returns a byte-range"
      (check-true (byte-range? (string->byte-range "a-z"))))

    (it "does not accept a string where the first character is not a byte"
      (check-exn exn:fail:contract? (thunk (string->byte-range "α-b"))))

    (it "does not accept a string where the last character is not a byte"
      (check-exn exn:fail:contract? (thunk (string->byte-range "a-β"))))

    (it "does not accept a string where the middle character is not a dash"
      (check-exn exn:fail:contract? (thunk (string->byte-range "a+β"))))

    (it "does not accept a string with greater than 3 characters"
      (check-exn exn:fail:contract? (thunk (string->byte-range "too much stuff"))))

    (it "does not accept a string with fewer than 3 characters"
      (check-exn exn:fail:contract? (thunk (string->byte-range "ab")))))

  (context "with r as a terse alias for string->byte-range"
    (define r string->byte-range)

    (describe "byte-range<?"
      (it "includes pairs of byte ranges when the lower bound is less on the first range"
        (check-true (byte-range<? (r "A-C") (r "B-C")))
        (check-true (byte-range<? (r "A-C") (r "B-D"))))

      (it "includes pairs of byte ranges with the same lower bound if the upper bound of the first is lower"
        (check-true (byte-range<? (r "A-B") (r "A-C"))))

      (it "excludes pairs of byte ranges with higher lower bound for the first range"
        (check-false (byte-range<? (r "B-C") (r "A-C")))
        (check-false (byte-range<? (r "B-C") (r "A-C"))))

      (it "excludes pairs of byte ranges with equal lower bound if the upper bound of the first is higher or equal"
        (check-false (byte-range<? (r "A-C") (r "A-B")))
        (check-false (byte-range<? (r "A-C") (r "A-C")))))

    (describe "byte-range<=?"
      (it "includes pairs of byte ranges when the lower bound is less on the first range"
        (check-true (byte-range<=? (r "A-C") (r "B-C")))
        (check-true (byte-range<=? (r "A-C") (r "B-D"))))

      (it "includes pairs of byte ranges with the same lower bound if the upper bound of the first is lower"
        (check-true (byte-range<=? (r "A-B") (r "A-C"))))

      (it "excludes pairs of byte ranges with higher lower bound for the first range"
        (check-false (byte-range<=? (r "B-C") (r "A-C")))
        (check-false (byte-range<=? (r "B-C") (r "A-C"))))

      (it "excludes pairs of byte ranges with equal lower bound if the upper bound of the first is higher"
        (check-false (byte-range<=? (r "A-C") (r "A-B"))))

      (it "includes pairs of bytes where the lower and upper bounds are equal"
        (check-true (byte-range<=? (r "A-C") (r "A-C")))))

    (describe "byte-range>=?"
      (it "includes pairs of byte ranges when the lower bound is less on the second range"
        (check-true (byte-range>=? (r "B-C") (r "A-C")))
        (check-true (byte-range>=? (r "B-D") (r "A-C"))))

      (it "includes pairs of byte ranges with the same lower bound if the upper bound of the second is lower"
        (check-true (byte-range>=? (r "A-C") (r "A-B"))))

      (it "excludes pairs of byte ranges with higher lower bound for the second range"
        (check-false (byte-range>=? (r "A-C") (r "B-C")))
        (check-false (byte-range>=? (r "A-C") (r "B-D"))))

      (it "excludes pairs of byte ranges with equal lower bound if the upper bound of the second is higher or equal"
        (check-false (byte-range>=? (r "A-B") (r "A-C"))))

      (it "includes pairs of byte ranges that are equal"
        (check-true (byte-range>=? (r "A-C") (r "A-C")))))

    (describe "byte-range>?"
      (it "includes pairs of byte ranges when the lower bound is less on the second range"
        (check-true (byte-range>? (r "B-C") (r "A-C")))
        (check-true (byte-range>? (r "B-D") (r "A-C") )))

      (it "includes pairs of byte ranges with the same lower bound if the upper bound of the second is lower"
        (check-true (byte-range>? (r "A-C") (r "A-B"))))

      (it "excludes pairs of byte ranges with higher lower bound for the second range"
        (check-false (byte-range>? (r "A-C") (r "B-C")))
        (check-false (byte-range>? (r "A-C") (r "B-C"))))

      (it "excludes pairs of byte ranges with equal lower bound if the upper bound of the second is higher or equal"
        (check-false (byte-range>? (r "A-B") (r "A-C")))
        (check-false (byte-range>? (r "A-C") (r "A-C")))))

    (describe "byte-range-intersect?"
      (it "includes ranges that have common members"
        (check-true (byte-range-intersect? (r "A-C") (r "A-C")))
        (check-true (byte-range-intersect? (r "A-C") (r "B-D")))
        (check-true (byte-range-intersect? (r "A-C") (r "C-E")))

        (check-true (byte-range-intersect? (r "A-C") (r "A-C")))
        (check-true (byte-range-intersect? (r "B-D") (r "A-C")))
        (check-true (byte-range-intersect? (r "C-E") (r "A-C"))))

      (it "does not include pairs of ranges that don't have a commonad member"
        (check-false (byte-range-intersect? (r "A-C") (r "D-F")))
        (check-false (byte-range-intersect? (r "A-C") (r "H-J")))

        (check-false (byte-range-intersect? (r "D-F") (r "A-C")))
        (check-false (byte-range-intersect? (r "H-J") (r "A-C")))))


    (describe "byte-range-adjacent?"
      (it "inclused pairs of ranges that are adjacent"
        (check-true (byte-range-adjacent? (r "A-C") (r "D-F"))))

      (it "does not include pairs of ranges that intersect"
        (check-false (byte-range-adjacent? (r "A-C") (r "C-E"))))

      (it "does not include pair of ranges that are discontiguous"
        (check-false (byte-range-adjacent? (r "A-C") (r "E-G")))))


    (describe "byte-range-contiguous"
      (it "includes pairs of ranges that intersect"
        (check-true (byte-range-intersect? (r "A-C") (r "B-D"))))

      (it "includes pairs of ranges that are adjacent"
        (check-true (byte-range-adjacent? (r "A-C") (r "D-F"))))

      (it "does not include pairs of ranges that are discontiguous"
        (check-false (byte-range-adjacent? (r "A-C") (r "E-G")))))))
