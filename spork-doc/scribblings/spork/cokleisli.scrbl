#lang scribble/manual

@begin[
 (require scribble/eval racket/sandbox)
 (define cokleisli-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket #:requires '(spork))))]

@title[#:tag "cokleisli"]{Co-Kleisli}
@defmodule[spork/cokleisli]
@section{Co-Kleisli Guide}
The @racket[cokleisli] type is a structure encapsulting comonadic destructors
-- functions accepting a comonad and returning an unadorned value. The
@racket[cokleisli] type implements the @racket[gen:arrow] and
@racket[gen:arrow-choice] protocols to facilitate composition of
comonadic destructors.

@section{Co-Kleisli API Reference}
The API for @racket[cokleisli] is minimal with the expectation being
that almost all operations on @racket[cokleisli] values will be done
with @racket[gen:arrow] operators.
@defproc[(cokleisli? [v any/c]) boolean?]{}
@defproc[(cokleisli [f (-> comonad? any/c)]) cokleisli?]{}
@defproc[(cokleisli-proc [ckf cokleisli?]) (-> comonad? any/c)]{}
@defproc[(cokleisli-run [ckf cokleisli?] [wx comonad?]) any/c]{}
