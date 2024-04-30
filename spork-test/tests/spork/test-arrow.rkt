#lang racket

(module+ test
  (require
   spork/infix-notation spork/category spork/arrow spork/functor
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
            '(#t . (#t . "-2")))))))))
