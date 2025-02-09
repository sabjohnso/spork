#lang racket

(module+ test
  (require
   (for-syntax racket)
   spork/delegation spork/phase-invariant rackunit rackunit/spec)

  (define/phase-invariant a<%> (interface () a))
  (define/phase-invariant b<%> (interface () b))
  (define/phase-invariant c<%> (interface () c))

  (define/phase-invariant other-a<%> (interface (a<%>)))
  (define other-a (new (class* object% (other-a<%>) (super-new) (define/public (a) (void)))))

  (define a (new (class* object% (a<%>) (super-new) (define/public (a) (void)))))
  (define b (new (class* object% (b<%>) (super-new) (define/public (b) (void)))))
  (define c (new (class* object% (c<%>) (super-new) (define/public (c) (void)))))

  (define ab
    (new (class* object%
             (a<%> b<%>)
           (super-new)
           (define/public (a) (void))
           (define/public (b) (void)))))

  (describe "delegator"
    (it "it accepts a mapping from objects to interfaces"
      (check-not-exn
       (thunk
        (delegator
         ([a (a<%>)]))))

      (context "with a delegator"
        (define delegating-a-to-a
          (delegator
           ([a (a<%>)])))

        (check-not-exn (thunk (send delegating-a-to-a a)))))

    (it "accepts multiple objects"
      (check-not-exn
       (thunk
        (delegator
         ([a (a<%>)]
          [b (b<%>)]
          [c (c<%>)]))))

      (context "with a delgegator"
        (define delegating
          (delegator
           ([a (a<%>)]
            [b (b<%>)]
            [c (c<%>)])))
        (check-not-exn (thunk (send delegating a)))
        (check-not-exn (thunk (send delegating b)))
        (check-not-exn (thunk (send delegating c)))))


    (it "can map each object to multiple interfaces"
      (check-not-exn
       (thunk
        (delegator
         ([ab (a<%> b<%>)]
          [c (c<%>)]))))

      (context "with a delgegator"
        (define delegating
          (delegator
           ([ab (a<%> b<%>)]
            [c (c<%>)])))
        (check-not-exn (thunk (send delegating a)))
        (check-not-exn (thunk (send delegating b)))
        (check-not-exn (thunk (send delegating c)))))

    (it "can map an object to interfaces that share a method name"
      (check-not-exn
       (thunk
        (delegator
         ([other-a (a<%> other-a<%>)])))))

    (it "can map object that have a common method name but don't share interfaces"
      (check-not-exn
       (thunk
        (delegator
         ([a (a<%>)]
          [ab (b<%>)])))))

    (it "cannot map two objects to the same interface"
      (check-exn
       exn:fail?
       (thunk
        (expand-syntax
         #'(delegator
            ([a (a<%>)]
             [ab (a<%>)]))))))

    (it "cannot map two object to the same method name even for different interfaces"
      (check-exn
       exn:fail?
       (thunk
        (expand-syntax
         #'(delegator
            ([a (a<%>)]
             [other-a (other-a<%>)]))))))

    (it "cannot map an object to an interface it's class doesn't implement"
      (context "with a poorly defined delegator"
        (check-exn exn:fail? (thunk (delegator ([a (b<%>)])))))))

  (module operator racket
    (provide operator<%>)
    (define operator<%>
      (interface ()
        [operate-on (->m number? number?)])))

  (require
   (for-syntax (submod "." operator))
   (submod "." operator))


  (describe "deletator usage"
    (it "can be used to reduce the the interface of an object"
      (define operator-with-3%
        (class* object%
            (operator<%>)
          (super-new)
          (define value 3)
          (define/public (get-value) value)
          (abstract operate-on)))

      (define add3
        (let ([unprotected-add3
               (new (class operator-with-3%
                      (super-new)
                      (inherit get-value)
                      (define/override (operate-on x)
                        (+ x (get-value)))))])
          (delegator
           ([unprotected-add3 (operator<%>)]))))
      (check-equal? (send add3 operate-on 4) 7)

      (check-exn exn:fail? (thunk (send add3 get-value))))))
