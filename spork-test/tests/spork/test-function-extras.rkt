#lang racket

(module+ test
  
  (require
   spork/function-extras spork/curried spork/functor
   rackunit rackunit/spec)
  
  (describe "the function context"
    (context "with some curried functions"
      (define-curried (add x y)
        (+ x y))

      (define-curried (mul x y)
        (* x y))
  
      (describe "function?"
        (it "returns true for functions (procedures with an arity that includes 1)"
          (check-true (function? sqr))
          (check-true (function? add)))

        (it "returns false for procedures greater arity"
          (check-false (function? (λ (x y) (* x y)))))

        (it "returns false for thunks"
          (check-false (function? (thunk 'x))))
        
        (it "returns false for other things"
          (check-false (function? "purple chicken"))))

      (describe "function-fmap"
        (it "performs functorial maping for functions (function composition)"
          (check-equal?
           ((function-fmap symbol->string (function-return 'x))
            'anything)
           "x")))
      
      (describe "function-return (const)"
        (it "places a value into a function context"
          (define f (function-return 'x))
          (check-true (function? f))
          (check-equal? (f 'really-anything) 'x)))

      (describe "function-fapply"
        (it "performs applicative mapping over values in a function context"
          (define f (function-fapply add sqr))
          (check-true (function? f))
          (check-equal? (f 3) 12)))

      (describe "function-flatmap"
        (it "performs monadic mapping over values in a function context"
          (define-curried (add x y)
            (+ x y))
          (define f (function-flatmap add sqr))
          (check-true (function? f))
          (check-equal? (f 3) 12)))


      (describe "function-arr"
        (it "lifts a function into the function arrow (it is identity)"
          (check-equal? (function-arr sqr) sqr)))

      (describe "function-fst"
        (it "returns a function applying the argument function to the first input"
          (check-equal? ((function-fst sqr) '(3 . x))
                        '(9 . x))))

      (describe "function-snd"
        (it "returns a function applying the argument function to the second input"
          (check-equal? ((function-snd sqr) '(x . 3))
                        '(x . 9))))

      (describe "function-split"
        (it "returns a function splitting the input between the two argument functions"
          (check-equal? ((function-split sqr symbol->string) '(3 . x))
                        '(9 . "x"))))

      (describe "function-fanout"
        (it "returns a function sending the input to both argument functions"
          (check-equal? ((function-fanout sqr number->string) 3)
                        '(9 . "3")))))))

