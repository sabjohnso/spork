#lang scribble/manual

@begin[
 (require
   (for-label (except-in racket #%app) spork)
   racket racket/sandbox scribble/eval)
 (define kleisli-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket #:requires '(spork))))]

@title[#:tag "kleisli"]{Kleisli}
@defmodule[spork/kleisli]
@section{Kleisli Guide}
In Spork, the @racket[kleisli] type represents a structure used with
monads to encapsulate monadic constructors -- functions returning a
monadic value from an unadorned input. The @racket[kleisli] type
facilitates composition of monadic constructors by implementing the
@racket[gen:arrow] and @racket[gen:arrow-choice] protocols.

@section{Kleisli API Reference}
The API for @racket[kleisli] is minimal with the expectation being
that almost all operations on @racket[kleisli] values will be done
with @racket[gen:arrow] operators.

@defproc[(kleisli? [v any/c]) boolean?]{
  Return @racket[#t] if the input is an instance of @racket[kleisli].
  Otherwise, return @racket[#f].}

@defproc[(kleisli [f (-> any/c monad?)]) kleisl?]{
  Accept a monadic constructor, returning a @racket[kleisli] structure.}

@defproc[(kleisli-proc [kf kleisli?]) (-> any/c monad?)]{
  Return the monadic constructor encapulated in the input
  @racket[kleisli] structure. }

@defproc[(kleisli-run [kf kleisli?] [v any/c]) monad?]{
  Return the result of applying monadic constructor encapsulated ing
  the @racket[kleisli] structure to the input value.}
