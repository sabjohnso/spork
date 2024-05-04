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

@title[#:tag "expect"]{Expect}
@defmodule[spork/expect]
@section{Expect Guide}
The @deftech{expect} type enacapsulates an @deftech{expected} value or
an @deftech{unexpected} value, where @tech{expected} values are
results of a successful process and @tech{unexpected} messages are
explanations of a failed process. The @tech{expect} type is intended
for use where there is value in conveying some details of an error or
exceptional case, but resorting to raising an exception is execessive.

@examples[#:eval optional-eval
  (define (safe-divide x y)
    (if (zero? y) (unexpected "division by zero")
      (expected (/ x y))))]

The @tech{expect} type is a 
@seclink["Monad" #:doc '(lib "scribblings/spork.scrbl") "monad"]
and hence, also an
@seclink["Applicative" #:doc '(lib "scribblings/spork.scrbl")
"applicative functor"]
and a
@seclink["Functors" #:doc '(lib "scribblings/spork.scrbl")
"functor"]. This means that it can take full advantage of the
functionality associated with those protocols.

@section{Exepct API Reference}

@defproc[(expected [v any/c]) expect?]{
  Construct an @tech{expected} value.}

@defproc[(unexpected [msg any/c]) expect?]{
  Construct an @tech{unexpected} message.}

@defproc[(expect? [v any/c]) boolean?]{
  Return @racket[#t] if the argument is an @tech{expect} value.
  Otherwise, return @racket[#f].}

@defproc[(expected? [v expect?]) boolean?]{
  Return @racket[#t] if the @tech{expect} argument is an @tech{expected} value.
  Otherwise, return @racket[#f]. }

@defproc[(unexpected? [v expect?]) boolean?]{
  Return @racket[#t] if the @tech{expect} argument is an @tech{unexpected} message.
  Otherwise, return @racket[#f].}




