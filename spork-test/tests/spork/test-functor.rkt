#lang racket

(module+ test
  (require
   rackunit rackunit/spec
   spork/functor spork/infix-notation)

  (describe "the trivial protocol"
    (context "with a trivial type"
      (struct wrapper
        (value)
        #:methods gen:trivial
        ((define (wrap-proc _) wrapper)
         (define (unwrap-proc _) wrapper-value))
        #:transparent)

      (it "supports the `wrap` and `unwrap` functions"

        (describe "wrap"
          (it "puts a value into an unresolved context"
            (check-true (unresolved? (wrap 'x)))))

        (describe "unwrap"
          (it "extract a value from an trivial context"
            (check-equal? (unwrap (wrapper 'x)) 'x))
          (it "can extract a value from an unresolved context"
            (check-equal? (unwrap (wrap 'x)) 'x))))

      (it "can be used to derive monad methods"
        (check-true (monad? (wrapper 'x)))
        (check-equal? (flatmap return (wrapper 'x)) (wrapper 'x))
        (check-equal? (join (wrapper (wrapper 'x))) (wrapper 'x)))

      (it "can be used to derive applicative methods"
        (check-true (applicative? (wrapper 'x)))
        (check-equal? (fapply (wrapper symbol->string) (wrapper 'x)) (wrapper "x"))
        (check-equal? (fapply (pure symbol->string) (wrapper 'x)) (wrapper "x"))
        (check-equal? (fapply (wrapper symbol->string) (pure 'x)) (wrapper "x")))

      (it "can be used to derive functor methods"
        (check-true (functor? (wrapper 'x)))
        (check-equal? (fmap symbol->string (wrapper 'x)) (wrapper "x")))

      (it "can be used to derive comonad methods"
        (check-true (comonad? (wrapper 'x)))
        (check-equal? (extract (wrapper 'x)) 'x)
        (check-equal? (duplicate (wrapper 'x)) (wrapper (wrapper 'x)))
        (check-equal? (extend (λ (wx) (symbol->string (extract wx))) (wrapper 'x))
                      (wrapper "x")))))

  (describe "the monad protocol"
    (context "with a monad type defined with return and flatmap"
      (struct maybe
        () #:transparent
        #:methods gen:monad
        ((define (return-proc _) just)
         (define (flatmap-proc _) maybe-flatmap)))
      (struct nothing maybe () #:transparent)
      (struct just maybe (value) #:transparent)

      (define (maybe-flatmap f mx)
        (match mx
          [(just x) (f x)]
          [(nothing) (nothing)]))

      (it "supports the `return` `flatmap' and `join' functions"
        (describe "return"
          (it "puts a value into an unresolved context"
            (check-true (unresolved? (return 'x)))))

        (describe "flatmap"
          (it "maps context constructor over a value int the context"
            (check-equal? (flatmap return (nothing)) (nothing))
            (check-equal? (flatmap return (just 'x)) (just 'x))
            (check-equal? (flatmap (λ (x) (just (symbol->string x))) (just 'x))
                          (just "x"))))
        (describe "join"
          (it "strips a layer of context"
            (check-equal? (join (just (just 'x))) (just 'x))
            (check-equal? (join (just (nothing))) (nothing))
            (check-equal? (join (nothing)) (nothing)))))

      (it "supports other monad functions"
        (context "with three constructors"
          (define (ctor1 x) (just (number->string x)))
          (define (ctor2 x) (if (> x 0) (just x) (nothing)))
          (define (ctor3 x) (if (zero? x) (nothing) (just (/ 100 x))))

          (describe "monad-compose (Kleisli composition)"
            (it "composes multiple monad constructors from right to left"
              (define func (monad-compose ctor1 ctor2 ctor3))
              (check-equal? (func 10) (just "10")))

            (it "acts as the identity function when provided with only one constructor"
              (define func (monad-compose ctor2))
              (check-equal? ctor2 func)
              (check-equal? (func 10) (just 10)))

            (it "return `return` when no constructors are provide"
              (check-equal? (monad-compose) return)))

          (describe "<=<"
            (it "is a synonym for monad-compose that may be preferable with infix notation"
              (define func (ctor1 `<=< ctor2 `<=< ctor3))
              (check-equal? (func 10) (just "10"))))

          (describe ">=>"
            (it "it performs right-to-left composition of monad constructors"
              (define func (ctor3 `>=> ctor2 `>=> ctor1))
              (check-equal? (func 10) (just "10"))))))

      (it "supports derivation of the applicative functor methods"
        (check-true (applicative? (just 'x)))
        (check-true (applicative? (nothing)))
        (check-equal? (fapply (just sqr) (just 3)) (just 9))
        (check-equal? (fapply (pure sqr) (just 3)) (just 9))
        (check-equal? (fapply (just sqr) (pure 3)) (just 9))
        (check-equal? (fapply (just sqr) (nothing)) (nothing))
        (check-equal? (fapply (nothing) (just 3)) (nothing)))

      (it "supports derivation of the functor methods"
        (check-true (functor? (just 'x)))
        (check-true (functor? (nothing)))
        (check-equal? (fmap symbol->string (just 'x)) (just "x"))
        (check-equal? (fmap symbol->string (nothing)) (nothing))))

    (context "with a monad type defined with return, join and fmap"
      (struct maybe
        () #:transparent
        #:methods gen:monad
        ((define (return-proc _) just)
         (define (join-proc _) maybe-join))
        #:methods gen:functor
        ((define (fmap-proc _) maybe-fmap)))

      (struct nothing maybe () #:transparent)

      (struct just maybe (value) #:transparent)

      (define (maybe-join mmx)
        (match mmx
          [(just (just x)) (just x)]
          [_ (nothing)]))

      (define (maybe-fmap f mx)
        (match mx
          [(just x) (just (f x))]
          [_ (nothing)]))

      (it "supports the `return` `flatmap' and `join' functions"
        (describe "return"
          (it "puts a value into an unresolved context"
            (check-true (unresolved? (return 'x)))))
        (describe "flatmap"
          (it "maps context constructor over a value int the context"
            (check-equal? (flatmap return (nothing)) (nothing))
            (check-equal? (flatmap return (just 'x)) (just 'x))
            (check-equal? (flatmap (λ (x) (just (symbol->string x))) (just 'x))
                          (just "x"))))
        (describe "join"
          (it "strips a layer of context"
            (check-equal? (join (just (just 'x))) (just 'x))
            (check-equal? (join (just (nothing))) (nothing))
            (check-equal? (join (nothing)) (nothing)))))

      (it "supports derivation of the applicative functor methods"
        (check-true (applicative? (just 'x)))
        (check-true (applicative? (nothing)))
        (check-equal? (fapply (just sqr) (just 3)) (just 9))
        (check-equal? (fapply (pure sqr) (just 3)) (just 9))
        (check-equal? (fapply (just sqr) (pure 3)) (just 9))
        (check-equal? (fapply (just sqr) (nothing)) (nothing))
        (check-equal? (fapply (nothing) (just 3)) (nothing)))

      (it "supports derivation of the functor methods"
        (check-true (functor? (just 'x)))
        (check-true (functor? (nothing)))
        (check-equal? (fmap symbol->string (just 'x)) (just "x"))
        (check-equal? (fmap symbol->string (nothing)) (nothing)))

      (it "supports monad binding syntax"
        (check-equal?
         (let/monad ([x (just 3)]
                     [y (just 4)])
           (return (+ x y)))
         (just 7))

        (check-equal?
         (let/monad ([x (nothing)]
                     [y (just 4)])
           (return (+ x y)))
         (nothing))

        (check-equal?
         (let/monad ([x (just 3)]
                     [y (nothing)])
           (return (+ x y)))
         (nothing)))))

  (describe "the applicative protocol"
    (context "with an applicative type"
      (struct ziplist
        (get)
        #:methods gen:applicative
        ((define (pure-proc _) make-ziplist)
         (define (fapply-proc _) zip-with))
        #:methods gen:functor
        ((define (fmap-proc _) ziplist-map))

        #:transparent)

      (define (make-ziplist . xs)
        (ziplist xs))

      (define (zip-with fs xs)
        (ziplist
         (for/list ([f (ziplist-get fs)]
                    [x (ziplist-get xs)])
           (f x))))

      (define (ziplist-map f xs)
        (ziplist (map f (ziplist-get xs))))

      (it "supports the `fapply` and `pure` functions"

        (describe "pure"
          (it "puts a value into an unresolved context"
            (check-true (unresolved? (pure 'x)))))

        (describe "fapply"
          (it "maps functions in a context over values in the same context"
            (check-equal?
             (fapply (make-ziplist symbol->string) (make-ziplist 'x))
             (make-ziplist "x"))

            (check-equal?
             (fapply (pure symbol->string) (make-ziplist 'x))
             (make-ziplist "x"))

            (check-equal?
             (fapply (make-ziplist symbol->string) (pure 'x))
             (make-ziplist "x")))))

      (it "supports derivation of functor methods"
        (check-true (functor? (make-ziplist 'x 'y 'z)))
        (check-equal? (fmap symbol->string (make-ziplist 'x 'y 'z))
                      (ziplist '("x" "y" "z"))))

      (it "supports applicative binding syntax"
        (check-equal?
         (let/applicative ([x (make-ziplist 1 2)]
                           [y (make-ziplist 3 4)])
           (+ x y))
         (make-ziplist 4 6)))))

  (describe "the functor protocol"
    (context "with a functor type"
      (struct named
        (name value)
        #:transparent
        #:methods gen:functor
        ((define (fmap-proc _) named-map)))

      (define (named-map f mx)
        (named (named-name mx) (f (named-value mx))))

      (it "supports the `fmap` function"
        (check-equal?
         (fmap symbol->string (named "Bob" 'x))
         (named "Bob" "x")))

      (it "supports functor binding syntax"
        (check-equal?
         (let/functor ([x (named "Bob" 3)]
                       [y (named "Bob" 4)])
           (+ x y))
         (named "Bob" (named "Bob" 7))))))


  (describe "the comonad protocol"
    (context "with a comonad type"
      (struct named
        (name value)
        #:transparent
        #:methods gen:comonad
        ((define (extract-proc _) named-value)
         (define (extend-proc _) named-extend)))

      (define (named-extend f mx)
        (named (named-name mx) (f mx)))

      (it "supports the comonad `extract`, `duplicate`, and `extend` functions"
        (describe "extract"
          (it "extracts a value from a comonad context"
            (check-equal? (extract (named "Bob" 'x)) 'x)))
        (describe "duplicate"
          (it "duplicates the named context"
            (check-equal? (duplicate (named "Bob" 'x)) (named "Bob" (named "Bob" 'x)))))

        (describe "extend"
          (it "extends the comonad context back over a value extracted from it"
            (check-equal? (extend (compose symbol->string extract) (named "Bob" 'x))
                          (named "Bob" "x")))))

      (it "supports derivation of functor methods"
        (check-true (functor? (named "Bob" 'x)))
        (check-equal? (fmap symbol->string (named "Bob" 'x))
                      (named "Bob" "x")))

      (it "supports additional comonad functions"
        (context "with three comonad destrutors"
          (define (dtor1 wx) (symbol->string (named-value wx)))
          (define (dtor2 wx) (let ([value (named-value wx)]) (string-append value value)))
          (define (dtor3 wx) (format "~a-~a" (named-name wx) (named-value wx)))

          (describe "comonad-compose (co-Kleisli composition)"
            (it "can compose multiple comonad destructors"
              (define func (comonad-compose dtor3 dtor2 dtor1))
              (check-equal? (func (named "Bob" 'x)) "Bob-xx"))

            (it "acts as the identity function with one argument"
              (check-equal? (comonad-compose dtor1) dtor1))

            (it "returns extract as the co-Kliesli unit with no arguments"
              (check-equal? (comonad-compose) extract)))

          (describe "=<="
            (it "is a symonym for comonad-commpose"
              (define dtor (dtor3 `=<= dtor2 `=<= dtor1))
              (check-equal? (dtor (named "Bob" 'x)) "Bob-xx")))

          (describe "=>="
            (it "is the left-to-right analog of =<="
              (define dtor (dtor1 `=>= dtor2 `=>= dtor3))
              (check-equal? (dtor (named "Bob" 'x)) "Bob-xx"))))))))
