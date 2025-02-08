#lang racket

(require
 spork/tape
 rackunit rackunit/spec)

(describe "tape"
  (it "describes a data structure with a sequence of values and a position"
    (context "with a tape define with 4 elements"
      (define xs (make-tape 'a 'b 'c 'd))
      (define xs-at-back (tape-fast-fwd xs))
      (describe "tape-position"
        (it "is initially 0"
          (check-equal? (tape-position xs) 0))

        (it "will be one if the tape is move forward one step"
          (check-equal? (tape-position (tape-fwd xs)) 1))

        (it "will be two if the tape is move forward by 2 steps"
          (check-equal? (tape-position (tape-fwd-by xs 2)) 2))

        (it "will provide the specified position after moving to a specified position the tape"
          (check-equal? (tape-position (tape-move-to xs 2)) 2)))

      (describe "tape-length"
        (it "returns the total length of the tape regarless of its position"
          (check-equal? (tape-length xs) 4)
          (check-equal? (tape-length (tape-move-to xs 2)) 4)))

      (describe "tape-remaining"
        (it "returns the remaining number of elements in a tape"
          (it "it will initially be the same as the length"
            (check-equal? (tape-remaining xs)
                          (tape-length xs)))
          (it "will decrement by one for each element consumed"
            (check-equal? (tape-remaining (tape-fwd xs))
                          (sub1 (tape-length xs))))))
        (describe "tape-fwd"
          (it "moves tape forward by one step"
            (check-equal? (tape-position (tape-fwd xs)) 1))

          (it "doesn't move the tape forward when at the end of the tape"
            (check-true (tape-at-back? xs-at-back))
            (check-equal? (tape-position (tape-fwd xs-at-back))
                          (tape-position xs-at-back))))

        (describe "tape-bwd"
          (it "moves a tape backward by one step"
            (check-equal? (tape-position (tape-bwd xs-at-back))
                          (sub1 (tape-length xs))))
          (it "does not move a tape that is already at the front"
            (check-true (tape-at-front? xs))
            (check-equal? (tape-position (tape-bwd xs))
                          (tape-position xs))))

        (describe "tape-fast-fwd"
          (it "moves a tape to the back"
            (check-true (tape-at-back? (tape-fast-fwd xs)))))

        (describe "tape-rewind"
          (it "moves a tape to the front"
            (check-true (tape-at-front? (tape-rewind xs-at-back)))))

        (describe "tape-read"
          (it "will return the current value of the tape"
            (check-equal? (tape-read xs) 'a)
            (check-equal? (tape-read (tape-fwd xs)) 'b))

          (it "it is an error to try and read a tape that is at the back"
            (check-exn exn:fail? (thunk (tape-read xs-at-back)))))

        (describe "tape-write"
          (it "replaces the current value of the tape with the input value"
            (let ([ys (tape-write xs 'x)])
              (check-equal? (tape-position ys) (tape-position xs))
              (check-equal? (tape-read ys) 'x))))

        (describe "tape-insert"
          (it "adds a new element to the tape"
            (let ([ys (tape-insert xs 'x)])
              (check-equal? (tape-length ys) (add1 (tape-length xs)))
              (check-equal? (tape-read ys) 'x)
              (check-equal? (tape-read (tape-fwd ys)) 'a))))

        (describe "tape-refabs"
          (it "returns the element of the tape at the absolute input position"
            (check-equal? (tape-refabs xs 1) 'b)
            (check-equal? (tape-refabs (tape-fwd xs) 1) 'b)))

        (describe "tape-refabs"
          (it "returns the element of the tape at the relative to the current position"
            (check-equal? (tape-refrel xs 1) 'b)
            (check-equal? (tape-refrel (tape-fwd xs) 1) 'c)))

        (describe "tape-push-back"
          (it "writes an item at the back of the tape"
            (check-equal?
             (tape-rewind (tape-push-back (list->tape '(a b c d)) 'e))
             (list->tape '(a b c d e)))))

        (describe "tape-push-front"
          (it "wites an item at the front of the tape"
            (check-equal?
             (tape-push-front (list->tape '(2 3 4)) 1)
             (list->tape '(1 2 3 4)))))

        (describe "tape-pop-back"
          (it "removes and returns a value from the back of a tape"
            (let*-values ([(xs) (list->tape '(1 2 3))]
                          [(value new-tape) (tape-pop-back xs)])
              (check-equal? 3 value)
              (check-equal? new-tape (tape-fast-fwd (list->tape '(1 2)))))))

        (describe "tape-pop-front"
          (it "removes and returns a value from the front of a tape"
            (let*-values ([(xs) (list->tape '(1 2 3))]
                          [(x xs) (tape-pop-front xs)])
              (check-equal? 1 x)
              (check-equal? (tape '(2 3) '()) xs)))))

  (it "can be constructed from a list"
    (check-equal?
     (list->tape '(a b c d))
     (tape '(a b c d) '())))

  (it "can be constructed from a vector"
    (check-equal?
     (vector->tape #(a b c d))
     (tape '(a b c d) '())))))
