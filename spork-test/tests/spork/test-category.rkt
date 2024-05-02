#lang racket

(module+ test
  (require
   rackunit rackunit/spec
   spork/category spork/infix-notation spork/function-extras)

  (describe "categories"
    (context "with a wrapper around functions as a category"
     (struct simple
       (proc)
       #:methods gen:category
       ((define (compose-proc this) simple-compose)
        (define (id-value this) simple-id)))

     (define (simple-run arg simple)
       ((simple-proc simple) arg))

     (define simple-id (simple identity))

     (define (simple-compose f g)
       (match-let ([(simple f) f]
		   [(simple g) g])
         (simple (compose f g))))

     (describe "composition with the unresolve id"
       (check-equal? (simple-run 'x (simple symbol->string)) "x")
       (it "is identity on the right"
         (check-equal? (simple-run 'x ((simple symbol->string) `<< id)) "x"))
       (it "is identity on the left"
         (check-equal? (simple-run 'x (id `<< (simple symbol->string))) "x")))

     (describe "right-to-left composition"
       (it "composes elements from the right to left"
         (check-equal?
         (simple-run 'x ((simple string->list) `<< (simple symbol->string)))
         '(#\x))))

     (describe "left-to-right composition"
       (it "composes elements from left to right"
         (check-equal?
         (simple-run 'x (>> (simple symbol->string) (simple string->list)))
         '(#\x)))))

    (describe "category for functions"
      (context "with an unary procedure defined"
        (define (twc x) (+ x x))

        (it "is a function"
          (check-true (function? twc)))

        (it "is a category element"
          (check-true (category? twc)))

        (it "has a corresponding identity"
          (check-equal? (category-id twc) identity))

        (it "can be composed with other functions"
          (check-equal? ((sqr `<< twc) 3) 36)
          (check-equal? ((sqr `>> twc) 3) 18))))))
