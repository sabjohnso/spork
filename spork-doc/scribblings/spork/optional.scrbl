#lang scribble/doc

@begin[
  (require
    (for-label racket (except-in spork #%app))
    racket  racket/sandbox    
    scribble/eval (except-in scribble/manual link))

  (define optional-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket  #:requires '(spork/functor))))]


@title[#:tag "optional"]{Optional}
@defmodule[spork/optional]
@section{Optional Guide}
The @deftech{optional} type encapsulates an optional value. A value of
type @tech{optional} a either contains a value of type a (represented
as @racket[(some value)], or it is empty (represented as
@racket[(none)]). Using @tech{optional} is beneficial for cases with
values are optional. Additionally, @tech{optional} is a good way to
deal with errors or exceptional cases when details of the error do not
not provide significant value.


@examples[#:eval optional-eval
  (define (safe-divide x y)
    (if (zero? y) (none)
      (some (/ x y))))]

The @tech{optional} type is also a @tech{monad}, and hence, also an
@tech{applicative functor} and a @tech{functor}. It is a simple kind
of error monad, where all errors are represented by Nothing. A richer
error monad can be built using the @tech{expect} type.

@section{Optional API Reference}
@defproc[(some [v any/c]) optional?]{

}
@defproc[(none) optional?]{
  Return @racket[#t] if the argument is an optional value.  Otherwise, return @racket[#f].}

@defproc[(optional? [v any/c]) boolean?]{
  Return @racket[#t] if the input is an optional value.}

@defproc[(some? [v optional?]) boolean?]{
  Return @racket[#t] if the argument was constructed with @racket[some].}

@defproc[(none? [v optional?]) boolean?]{
  Return @racket[#t] if the argument was constructed with @racket[none].}
