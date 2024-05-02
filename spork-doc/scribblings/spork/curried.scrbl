#lang scribble/doc

@begin[
  (require
    (for-label racket spork/curried)
  (except-in scribble/manual link) racket racket/sandbox scribble/eval)]

@title[#:tag "curried"]{Curried}
@defmodule[spork/curried]
@section{Curried Guide}
@section{Curried API Reference}
@deftogether[(
  @defform[(lambda-curried formals body ...+)]
  @defform/subs[
    (λ-curried formals body ...+)
    ([formals (id ...+)])])]{
  Produces a curried function.}

@defform/subs[
  (define-curried (head formals ...+) body ...+)
  ([head id]
   [formals (id ...+)])]{
  Defines a function
}

@defform/subs[
  (curried-> dom ...+ range)
  ([dom dom-expr]
   [range range-expr 
          (values range-expr ...)
          any])]{
  A curried function contract.}
   

