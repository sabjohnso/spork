#lang racket

(module+ test
  (require spork/mutable-collections/stack spork/optional rackunit rackunit/spec)

  (describe "stacks"
    (describe "make-stack"
      (it "makes an empty stack"
        (check-true (stack? (make-stack)))
        (check-true (stack-empty? (make-stack)))))

    (describe "stack?"
      (it "is a predicate recognizing stacks"
        (check-true (stack? (make-stack)))
        (check-false (stack? "something completely different"))))

    (describe "stack-push!"
      (it "pushes a value onto a stack"
        (define stack (make-stack))
        (stack-push! stack 'x)
        (check-equal? (some 'x) (stack-pop! stack))))

    (describe "stack-pop!"
      (it "takes values off the stack in LIFO order"
        (define stack (make-stack))
        (stack-push! stack 'x)
        (stack-push! stack 'y)
        (check-equal? (some 'y) (stack-pop! stack))
        (check-equal? (some 'x) (stack-pop! stack))
        (check-equal? (none) (stack-pop! stack))))

    (describe "stack-clear!"
      (it "clears a stack"
        (define stack (make-stack))
        (stack-push! stack 'x)
        (stack-push! stack 'y)
        (stack-clear! stack)
        (check-true (stack-empty? stack))))

    (context "with an empty stack"
      (define stack (make-stack))
      (it "it safe for futures"
        (let ([n 1000])
          (for/async ([i n])
            (stack-push! stack i))

          (for/async ([i n])
            (check-false (none? (stack-pop! stack))))

          (check-true (stack-empty? stack)))))))
