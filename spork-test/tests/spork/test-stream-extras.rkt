#lang racket

(module+ test
  (require
   spork/stream-extras
   rackunit rackunit/spec)

  (describe "stream-return"
    (it "is the unit constructor for streams"
      (define xs (stream-return 'x))
      (check-true (stream? xs)))

    (it "is a function when used as an argument"
      (check-true (procedure? stream-return)))

    (it "is syntax transformer when applied"
      (define count 0)
      (define (counter x)
        (set! count (add1 count))
        x)

      (define xs (stream-return (counter 'x)))
      (it "evaluates its argument lazily"
        (check-equal? count 0))

      (check-equal? (stream-first xs) 'x)
      (check-equal? count 1)))


  (describe "stream-fmap"
    (it "maps a function over a stream"
      (define xs (stream-map sqr (in-naturals)))
      (check-true (stream? xs))
      (check-equal? (stream->list (stream-take xs 4)) '(0 1 4 9)))

    (it "is a function when used as an argument"
      (check-true (procedure? stream-fmap)))

    (it "is a syntax transformer when applied"
      (define count 0)
      (define (counter x)
        (set! count (add1 count))
        x)
      (define xs (stream-fmap sqr (counter (in-naturals))))
      (check-equal? count 0)
      (check-equal? (stream-first xs) 0)
      (check-equal? count 1)))

  (describe "stream-fapply"
    (it "maps a stream of functions over a stream of values"
      (define (twc x) (+ x x))
      (define xs (stream-fapply (stream twc sqr) (in-naturals)))
      (check-true (stream? xs))
      (check-equal? (stream->list (stream-take xs 4))
                    '(0 2 4 6))))

  (describe "stream-fapply-transpose"
    (it "maps a stream of functions over a stream of values"
      (define (twc x) (+ x x))
      (define xs (stream-fapply-transpose (stream twc sqr) (in-naturals)))
      (check-true (stream? xs))
      (check-equal? (stream->list (stream-take xs 4))
                    '(0 0 2 1))))

  (describe "stream-flatmap"
    (it "maps a stream constructor over a stream flattening the output to a stream"
      (define (dup x) (stream x x))
      (define xs (stream-flatmap dup (in-naturals)))
      (check-true (stream? xs))
      (check-equal? (stream->list (stream-take xs 4))
                    '(0 0 1 1))))

  (describe "stream-join"
    (it "joins the outermost level of multiple levels are stream context"
      (define (dup x) (stream x x))
      (define xss (stream-map dup (in-naturals)))
      (check-true (stream? xss))
      (check-true (stream? (stream-first xss)))

      (define ys (stream-join xss))
      (check-true (stream? ys))
      (check-equal? (stream->list (stream-take ys 4)) '(0 0 1 1))))

  (describe "nonempty-stream?"
    (it "is a predicate recognizing streams that are not empty"
      (check-false (nonempty-stream? empty-stream))
      (check-true (nonempty-stream? (in-naturals)))))

  (describe "nonempty-stream-duplicate"
    (it "duplicates a stream"
      (define xss (nonempty-stream-duplicate (in-naturals)))
      (check-true (stream? xss))
      (check-true (stream? (stream-first xss)))

      (check-equal?
       (stream->list
        (stream-take
         (stream-map
          (λ (xs)
            (stream->list (stream-take xs 4)))
          xss)
         4))
       '((0 1 2 3)
         (1 2 3 4)
         (2 3 4 5)
         (3 4 5 6)))))

  (describe "nonempty-stream-extend"
    (it "extends a stream destructor over a stream"
      (check-equal?
       (stream->list (stream-take (nonempty-stream-extend stream-first (in-naturals)) 4))
       '(0 1 2 3))))

  (describe "stream-iterate"
    (it "is makes a stream by iterating with a function over the intial value"
      (define xs (stream-iterate add1 0))
      (check-equal?
       (stream->list (stream-take xs 4))
       '(0 1 2 3)))))
