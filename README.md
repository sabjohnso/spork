Spork
=====

This library implements a number of utilities.

## Basic Utilities
### Infix Notation

notation when it truely improves the readability or the clarity of the
intent of the code. This is accomplished without introducing ambiguity
and the need for precidence rules to alleviate that ambiguity, because
doing so directly conflict with the intent of introducing the notation.

```racket
(3 `+ 4 `+ 5) ; => 12
```

```racket
(f `compose g `compose h)
```

### Discriminated Unions
```racket
(union optional
  ((none)
   (some value))
  #:methods gen:monad
  ((define (return-proc _) some)
   (define (flatmap-proc _) optional-flatmap)))

(define (optional-flatmap f mx)
  (match mx
    [(some x) (f x)]
    [(none)   (none)]))
```

### Tags (vacuous singletons)
```racket
(define-tag mytag)
(mytag? mytag) ; => #t
(tag? mytag) ; => #t
```

### Curried Functions
```racket
(define-curried/contract (add x y)
  (curried-> number? number? number?)
  (+ x y))

(add 3 4) ; => 7
((add 3) 4) ; => 7
```

## Abstractions with binding syntax

### Functors
```racket
(let/functor ([x '(1 2)]
              [y '(3 4)])
  (+ x y))
;; => '((4 5) (5 6))
```

### Applicative Functors
```racket
(let/appl ([x '(1 2)]
           [y '(3 4)])
  (+ x y))
;; => '(4 5 5 6)
```

### Monads
```racket
(let/monad ([x '(1 2)]
            [y '(3 4)])
  (return (+ x y)))
;; => '(4 5 5 6)
```

```racket
(let/monad ([x '(1 2)]
            [y '(3 4)])
  (list x (+ x y)))
;; => '(1 4 1 5 2 5 2 6)
```
