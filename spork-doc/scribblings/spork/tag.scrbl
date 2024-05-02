#lang scribble/doc

@begin[
  (require 
    (for-label spork spork/tag)
    (except-in scribble/manual link) racket racket/sandbox scribble/eval
    (except-in spork #%app))

  (define tag-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket  #:requires '(spork/tag))))]
    

@title[#:tag "tag"]{Tag}
@defmodule[spork/tag]
@section{Tag Guide}
@section{Tag API Reference}
@defform[(define-tag id)]{
  Defines a vacuous singleton and a predicate recognizing it:
  @itemize[
    @item{@racketidfont{id}: the singleton value}
    @item{@racketidfont{id?}: the predicate}]}

@defproc[(tag? [v any/c]) boolean?]{
  Returns @racket[#t] if and only if, @racket[v] is a tag.}

@examples[#:eval tag-eval
  (define-tag my-tag)
  (tag? my-tag?)
  (my-tag? my-tag)
  (my-tag? "something completely different")]
