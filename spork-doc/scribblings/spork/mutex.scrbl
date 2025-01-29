#lang scribble/doc

@begin[
  (require
    (for-label spork/mutex (except-in racket #%app))
    (except-in scribble/manual link)
    racket racket/sandbox
    scribble/eval
    (except-in spork #%app))

  (define mutex-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(spork/mutex))))]
@title[#:tag "mutex"]{Mutex}
@defmodule[spork/mutex]
Warning: mutexes should not be used with futures as some futures may be suspended at any time,
which may leave a mutex in deadlock.
@section{Mutex Guide}
@section{Mutex API Reference}
@defproc[(mutex? [v any/c]) boolean?]{}
@defproc[(make-mutex) mutex?]{}
@defproc[(with-mutex [m mutex?] [thunk (-> any/c)]) any/c]{}
