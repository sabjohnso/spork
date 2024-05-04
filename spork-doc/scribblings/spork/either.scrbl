#lang scribble/manual

@begin[
  (require
    (for-label spork))]

@title{Either}
@defmodule[spork/either]

@section{Either Guide}
The @deftech{either} type represents a choice of two posibilities
@racket[left] or @racket[right].  The @tech{either} type is a
@seclink["Monad" #:doc '(lib "scribblings/spork.scrbl") "monad"],
and hence, also an
@seclink["Applicative" #:doc '(lib "scribblings/spork.scrbl") "applicative functor"]
and a
@seclink["Functors" #:doc '(lib "scribblings/spork.scrbl") "functor"].

@section{Either API Reference}
@defproc[(left [v any/c]) either]{
  Construct an @tech{either} value satisfying @racket[left?]. }

@defproc[(right [v any/c]) either]{
  Construct an @tech{either} value satisfying @racket[right?]. }

@defproc[(either? [v any/c]) boolean?]{
  Return @racket[#t] if the input is an @tech{either} value.
  Otherwise, return @racket[#f]. }

@defproc[(left? [v either?]) boolean?]{
  Return @racket[#t] if the input @tech{either} input was constructed
  with @racket[left].  Otherwise, return @racket[#f]. }

@defproc[(right? [v either?]) boolean?]{
  Return @racket[#t] if the input @tech{either} input was constructed
  with @racket[right].  Otherwise, return @racket[#f]. }
