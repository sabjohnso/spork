Spork
=====

This library implements a number of utilities.

## Basic Utilities
### Infix Notation
The intent of the infix notation extension is to provide infix
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
