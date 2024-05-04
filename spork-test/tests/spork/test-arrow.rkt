#lang racket

(module+ test
  (require
   spork/infix-notation spork/category spork/arrow spork/functor spork/function-extras
   spork/either
   rackunit rackunit/spec)

  (describe "the arrow protocol"
    (context "with a type implementing the arrow protocol"
      (struct simple
       (proc)
       #:methods gen:trivial
       ((define (wrap-proc this) simple)
        (define (unwrap-proc this) simple-proc))

       #:methods gen:category
       ((define (compose-proc this) simple-compose)
        (define (id-value this) simple-id))

       #:methods gen:arrow
       ((define (arr-proc this) simple)
        (define (fst-proc this) simple-fst)))

     (define simple-id (simple identity))
     (define (simple-compose f g)
       (match* (f g)
         [((simple f) (simple g)) (simple (compose f g))]))

     (define (simple-fst sf)
       (match-let ([(simple f) sf])
         (simple (match-lambda [(cons x y) (cons (f x) y)]))))

     (define (simple-run simple arg)
       ((unwrap simple) arg))

     (define (uncurry f)
       (match-lambda [(cons x y) (f x y)]))

     (it "provides `arr`, `fst`, `snd`, `split`, and `fanout` functions"
       (describe "arr"
         (it "lifts a function into an unresolved context"
           (check-true (arr? (arr symbol->string)))))

       (describe "fst"
         (it "returns an arrow applying the input arrow to the first input"
           (check-equal? (simple-run (fst (simple symbol->string)) '(x . y))
                         '("x" . y))))

       (describe "snd"
         (it "returns an arrow applying the input arrow to its second input"
           (check-equal? (simple-run (snd (simple symbol->string)) '(x . y))
                         '(x . "y"))))

       (describe "split"
         (it "returns an arrow splitting its input to the two input arrows"
           (check-equal?
            (simple-run (split (simple symbol?) (simple symbol->string)) '(x . y))
            '(#t . "y"))))

       (describe "fanout"
         (it "returns an arrow applying both input arrows to its input"
           (check-equal?
            (simple-run (fanout (simple symbol?) (simple symbol->string)) 'x)
            '(#t . "x")))))

     (it "provides operators for infix notation"
       (describe "&&&"
         (it "is the same as fanout"
           (check-equal?
            (simple-run ((simple symbol?) `&&& (simple symbol->string) ) 'x)
            '(#t . "x")))

         (it "it can be applied to multiple arguments"
           (check-equal?
            (simple-run ((simple exact?)        `&&&
                         (simple positive?) `&&&
                         (simple number->string))
                        10.2)
            '(#f . (#t . "10.2")))))

       (describe "***"
         (it "is the same as split for two arguments"
           (check-equal?
            (simple-run ((simple symbol?) `*** (simple symbol->string)) '(x . y))
            '(#t . "y"))

           (check-equal?
            (simple-run ((simple exact?) `***
                         (simple positive?) `***
                         (simple number->string))
                        '(3 . (10 . -2)))
            '(#t . (#t . "-2")))))

       (describe ">>>"
         (it "composes arrows from left to right."
           (check-equal? (simple-run ((simple symbol->string) `>>> (simple string-length)) 'abc)
                         3))))))

  (describe "arrow choice"
    (it "provides the choose-left, choose-right, choose and fanin functions"
      (describe "choose-left"
        (it "accepts an arrow and returns an arrow transforming `left` inputs"
          (define sqr-left (choose-left sqr))
          (check-equal? (sqr-left (left 3))  (left 9))
          (check-equal? (sqr-left (right 3)) (right 3))))

      (describe "choose-right"
        (it "accepts an arrow and returns an arrow transforming `right` inputs"
          (define sqr-right (choose-right sqr))
          (check-equal? (sqr-right (left 3))  (left 3))
          (check-equal? (sqr-right (right 3)) (right 9))))

      (describe "choose"
        (it "accepts too arrows and return and arrow transforming `left` and `right` inputs"
          (define f (choose sqr number->string))
          (check-equal? (f (left 3)) (left 9))
          (check-equal? (f (right 3)) (right "3"))))

      (describe "fanin"
        (it "accepts too arrows and return and arrow transforming `left` and `right` inputs"
          (define f (fanin number->string symbol->string))
          (check-equal? (f (left 3)) "3")
          (check-equal? (f (right 'x)) "x"))))

    (it "provides a couple of opperators corresponding to choose and fanin"
      (describe "+++"
        (it "is the same as choose for to inputs"
          (check-equal? ((sqr `+++ number->string) (left 3)) (left 9))
          (check-equal? ((sqr `+++ number->string) (right 3)) (right "3")))
        (it "is variadic"
          (check-equal? ((sqr `+++ symbol->string `+++ list) (left 3))
                        (left 9))
          (check-equal? ((sqr `+++ symbol->string `+++ list) (right (left 'x)))
                        (right (left "x")))
          (check-equal? ((sqr `+++ symbol->string `+++ list) (right (right 'x)))
                        (right (right '(x))))))
      (describe "///"
        (it "is the same as fanin for two inputs"
          (check-equal? ((number->string `/// symbol->string) (left 3))  "3")
          (check-equal? ((number->string `/// symbol->string) (right 'x)) "x"))
        (it "is variadic"
          (check-equal? ((number->string `/// symbol->string `/// identity) (left 3))
                        "3")
          (check-equal? ((number->string `/// symbol->string `/// identity) (right (left 'x)))
                        "x")
          (check-equal? ((number->string `/// symbol->string `/// identity) (right (right "y")))
                        "y")))))

  (describe "the function arrow"
    (describe "arr"
      (it "lifts a function into the function arrow, but doesn't really do anything"
        (check-equal? ((sqr `>>> (arr number->string)) 3) "9")
        (check-equal? (((arr sqr) `>>> number->string) 3) "9")
        (check-equal? (((arr sqr) `>>> (arr number->string)) 3) "9")))

    (describe "fst"
      (it "returns a function arrow applying the input arrow to the first input"
        (check-equal? ((fst sqr) '(3 . x)) '(9 . x))))

    (describe "snd"
      (it "returns a function arrow applying the input arrow to the second input"
        (check-equal? ((snd sqr) '(x . 3)) '(x . 9))))

    (describe "split"
      (it "returns a function splitting the input between the argument functions"
        (check-equal? ((split sqr symbol->string) '(3 . x)) '(9 . "x"))))

    (describe "fanout"
      (it "returns a function arrow consing the results of applying the input arrows"
        (check-equal? ((fanout sqr number->string) 3) '(9 . "3"))))))
