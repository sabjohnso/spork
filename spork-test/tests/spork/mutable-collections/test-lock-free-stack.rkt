#lang racket

(module+ test
  (require spork/mutable-collections/lock-free-stack spork/optional rackunit rackunit/spec)

  (describe "lock-free stacks"
    (describe "make-lock-free-stack"
      (it "makes an empty lock-free stack"
        (check-true (stack? (make-lock-free-stack)))
        (check-true (lock-free-stack? (make-lock-free-stack)))
        (check-true (stack-empty? (make-lock-free-stack)))))

    (describe "stack?"
      (it "is a predicate recognizing stacks"
        (check-true (stack? (make-lock-free-stack)))
        (check-false (stack? "something completely different"))))

    (describe "stack-push!"
      (it "pushes a value onto a stack"
        (define stack (make-lock-free-stack))
        (stack-push! stack 'x)
        (check-equal? (some 'x) (stack-pop! stack))))

    (describe "stack-pop!"
      (it "takes values off the stack in LIFO order"
        (define stack (make-lock-free-stack))
        (stack-push! stack 'x)
        (stack-push! stack 'y)
        (check-equal? (some 'y) (stack-pop! stack))
        (check-equal? (some 'x) (stack-pop! stack))
        (check-equal? (none) (stack-pop! stack))))

    (describe "stack-clear!"
      (it "clears a stack"
        (define stack (make-lock-free-stack))
        (stack-push! stack 'x)
        (stack-push! stack 'y)
        (stack-clear! stack)
        (check-true (stack-empty? stack))))))
