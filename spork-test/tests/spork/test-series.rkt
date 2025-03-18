#lang racket

(module+ test
  (require spork/series spork srfi/26 rackunit rackunit/spec)

  (describe "type series type"
    (describe "series"
      (it  "it constructs a series"
        (check-true (series? (series (λ (i) (expt i -2))))))

      (it "accepts  a function mapping a natural number to a number"
        (check-not-exn (thunk (series (λ (i) (expt i -2))))))

      (it "does not accept a value that is not a function"
        (check-exn exn:fail? (thunk (series "Hello, World!"))))


      (it "does not accept a function with a different number of argument"
        (check-exn exn:fail? (thunk (series (thunk 8))))
        (check-exn exn:fail? (thunk (series (λ (x y) (expt x y))))))

      (it "does accept functions that are variadic if the arity includes 1"
        (check-not-exn (thunk (series +))))

      (it "will a fail when called if the function is  unary, but does not return a number"
        (check-exn exn:fail?
                   (thunk
                    (let ([f (series (λ (i) "Hello, World!"))])
                      (f 3))))))
    (describe "series?"
      (it "is the predicate for the series struct"
        (it "recognizes series instances"
          (check-true (series? (series (λ (i) (expt i -2))))))
        (it "does not recognize values of other types"
          (check-false (series? "An apple")))))

    (describe "series-partial-sums"
      (it "returns an infinite stream of the partial sums of the series"
        (define (f i) (expt 2 (- i)))
        (define xs (series-partial-sums (series f)))
        (check-true (stream? xs))
        (check-equal? (stream-first xs) (f 0))
        (check-equal? (stream-ref xs 1) (+ (f 0) (f 1)))
        (check-equal? (stream-ref xs 2) (+ (f 0) (f 1) (f 2))))))


  (describe "midpoint-transform"
    (it "is a sequence transform that returns a stream of the the midpoints of its input stream"
      (define (func i)
        (/ (expt -1 i)
           (add1 (* 2 i))))
      (define pi/4 (series func))
      (define partial-sums (series-partial-sums pi/4))
      (define midpoints (midpoint-transform partial-sums))
      (check-true (stream? midpoints))
      (check-equal?
       (stream-first midpoints)
       (/ (+ (stream-ref partial-sums 0)
             (stream-ref partial-sums 1))
          2))
      (check-equal?
       (stream-ref midpoints 9)
       (/ (+ (stream-ref partial-sums 9)
             (stream-ref partial-sums 10))
          2))))

  (describe "sequence transfroms"
    (context "with a series representation of pi"
      (define (func i)
        (/ (expt -1 i)
           (add1 (* 2 i))))
      (define pi/4 (/ pi 4))
      (define pi/4-series (series func))
      (define pi/4-sums (series-partial-sums pi/4-series))

      (check-equal?
       '(-0.21460183660255172
         0.11873149673078165
         -0.08126850326921842
         0.06158863958792449
         -0.04952247152318667)
       (call-with pi/4-sums
         ((cut stream-map (cut - pi/4 <>) <>)
          `>>> (cut stream-take <> 5)
          `>>> stream->list)))



      (check-equal?
       '(-0.04793516993588509
         -0.014601836602551765
         -0.005078027078742164
         -0.001903423904139001
         -0.000749022749737871)
       (call-with pi/4-sums
         (eulers-transform
          `>>> (cut stream-map (cut - pi/4 <>) <>)
          `>>> (cut stream-take <> 5)
          `>>> stream->list)))


      (check-equal?
       '(-0.21460183660255172
         0.11873149673078165
         -0.08126850326921842
         0.06158863958792449
         -0.04952247152318667)
       (call-with pi/4-sums
         ((cut stream-map (cut - pi/4 <>) <>)
          `>>> (cut stream-take <> 5)
          `>>> stream->list)))



      (check-equal?
       '(-0.21460183660255172
         0.01873149673078156
         -0.001903423904139001
         0.00020557820486311051
         -2.295143543129541e-5)
       (call-with pi/4-sums
         (van-wijngaarden-diagonal-transform
          `>>> (cut stream-map (cut - pi/4 <>) <>)
          `>>> (cut stream-take <> 5)
          `>>> stream->list)))


      (check-equal?
       '(-0.006268503269218351
         0.002064830064114953
         -0.0009113604120755125
         0.0004775284768133714
         -0.0002800472807623633)
       (call-with pi/4-sums
         (delta-squared-transform
          `>>> (cut stream-map (cut - pi/4 <>) <>)
          `>>> (cut stream-take <> 5)
          `>>> stream->list))))))
