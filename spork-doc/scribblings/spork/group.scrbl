#lang scribble/doc

@begin[
 (require
   scribble/eval
   (except-in racket #%app) racket/sandbox racket/generic
   (except-in scribble/manual link)
   (for-syntax racket scribble/eval spork/group)
   (for-label racket spork/group racket/generic syntax/stx))]

@(define group-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket #:requires '(spork))))

@title[#:tag "group"]{Group}
@defmodule[spork/group]
Groups
