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
          (check-equal? (f 3) 12))))))

