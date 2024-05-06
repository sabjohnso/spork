#lang scribble/doc
@begin[
  (require
    (for-label (except-in racket #%app) spork)
    racket racket/sandbox
    scribble/eval (except-in scribble/manual link))

  (define env-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(spork))))]

@title[#:tag "Env"]{Env}
@defmodule[spork/env]
@section{Env Guide}

@section{Env API Reference}
@defproc[(env? [v any/c]) boolean?]{}
@defproc[(env (-> any/c any/c)) env?]{}
@defproc[(env-run [e any/c] [mv env?]) any/c]{}
@defproc[(env-return [v any/c]) env?]
@defthing[env-ask env?]{}
@defproc[(env-select [f (-> any/c any/c)] [mv env?]) env?]{}
@defproc[(env-local [f (-> any/c any/c)] [mx env?]) env?]{}
