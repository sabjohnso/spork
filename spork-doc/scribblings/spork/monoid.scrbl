#lang scribble/doc

@begin[
 (require
   scribble/eval
   (except-in racket #%app) racket/sandbox racket/generic
   (except-in scribble/manual link)
   (for-syntax racket scribble/eval spork/monoid)
   (for-label racket spork/monoid racket/generic syntax/stx))]

@(define monoid-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket #:requires '(spork))))

@title[#:tag "monoid"]{Monoid}
@defmodule[spork/monoid]
Monoids
