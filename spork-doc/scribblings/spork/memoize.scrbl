#lang scribble/doc

@begin[
  (require
     (for-label spork/memoize (except-in racket #%app))
     (except-in scribble/manual link)
     racket racket/sandbox
     scribble/eval
     (except-in spork #%app))
  (define memoize-eval
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(spork/memoize))))
]

@title[#:tag "Memoize"]{Memoize}
@defmodule[spork/memoize]
@section{Memoize Guide}
@section{Memoize API Reference}
