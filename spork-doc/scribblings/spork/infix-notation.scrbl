#lang scribble/doc

@begin[
  (require
     scribble/eval
     (except-in scribble/manual link)
     (except-in racket #%app) racket/sandbox spork/infix-notation)

  (define infix-notation-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(spork))))]

@title[#:tag "infix"]{Infix Notation}
@defmodule[spork/infix-notation]
@section{Infix Notation Guide}
@examples[#:eval infix-notation-eval
  (1 `+ 2 `+ 3)

  ((pure (λ-curried (x y) (+ x y))) `<*> (list 1 2) `<*> (list 3 4))

  ((sqr `>> number->string) 3)

  (1 `+ 3 `- 2)]
