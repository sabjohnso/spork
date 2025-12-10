#lang racket

(module+ test
  (require
   spork spork/monoid spork/group
   rackunit rackunit/spec)

  (struct sum
    (value)
    #:methods gen:trivial
    ((define (wrap-proc trivial) sum)
     (define (unwrap-proc trivial) sum-value))
    #:methods gen:group
    ((define (group-proc group) (lift2 +))
     (define (group-inverse-proc group) (fmap -))
     (define (group-identity group) (sum 0)))
    #:transparent)

  (struct product
    (value)
    #:methods gen:trivial
    ((define (wrap-proc trivial) product)
     (define (unwrap-proc trivial) product-value))
    #:methods gen:group
    ((define (group-proc group) (lift2 *))
     (define (group-inverse-proc group) (fmap /))
     (define (group-identity group) (sum 1)))
    #:transparent)


  (describe "group"
    (it "is a protocol for groups"
      (define-values (a b c) (values 3 4 5))
      (describe "<>"
        (it "is the group operator"
          (check-equal?
           (<> (sum a) (sum b))
           (sum (+ a b))))
        (it "is associative"
          (check-equal?
           (<> (sum a) (<> (sum b) (sum c)))
           (<> (<> (sum a) (sum b)) (sum c))))
        (it "is variadic"
          (check-equal?
           (<> (sum a) (sum b) (sum c))
           (sum (+ a b c))))
        (it "can be used with infix notation"
          (check-equal?
           ((sum a) `<> (sum b) `<> (sum c))
           (sum (a `+ b `+ c))))
        (it "has an elment that is identity on the left"
          (check-equal?
           ((sum a) `<> id/group)
           (sum a)))
        (it "has the same element that is identity on the right"
          (check-equal?
           (id/group `<> (sum a))
           (sum a)))
        (it "is generic an can be used with different types implementing the protocol"
          (check-equal?
           ((product a) `<> (product b) `<> (product c))
           (product (a `* b `* c)))))

      (describe "invert"
        (it "is the group inverse operator"
          (check-equal?
           (invert (sum a))
           (sum (- a)))

          (check-equal?
           ((sum a) `<> (invert (sum a)))
           (group-identity (sum a))))

        (it "returns identity element unchanged"
          (check-equal?
           (invert (group-identity (sum a)))
           (group-identity (sum a)))))))

  (describe "monoid"
    (define-values (a b c) (values 3 4 5))
    (it "is a protocol for monoids"
      (it "is a generalization of groups"
        (check-true (group? (sum a)))
        (check-true (monoid? (sum a)))

        (check-true (group? (product a)))
        (check-true (monoid? (product a)))

        (check-false (group? (list a)))
        (check-true (monoid? (list a))))

      (describe "++"
        (it "is the monoid operator"
          (check-equal?
           ((list a) `++ (list b))
           (list a b)))

        (it "is associative"
          (check-equal?
           ((list a) `++ ((list b) `++ (list c)))
           (((list a) `++ (list b)) `++ (list c))))

        (it "has identity id/monoid on the left"
          (check-equal?
           ((list a) `++ id/monoid)
           (list a)))

        (it "has identity id/monoid on the right"
          (check-equal?
           (id/monoid `++ (list a))
           (list a)))))))
