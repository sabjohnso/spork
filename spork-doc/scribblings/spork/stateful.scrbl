#lang scribble/doc
@begin[
  (require
    (for-label (except-in racket #%app) spork)
    racket racket/sandbox
    scribble/eval (except-in scribble/manual link))

  (define stateful-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(spork))))]

@title[#:tag "state"]{Stateful}
@defmodule[spork/stateful]{}
@section{Stateful Guide}
@examples[#:eval stateful-eval
  (define-curried (update input state)
           (match* (input state)
             [('on  `(off ,score1 ,score2)) `(on  ,score1        ,score2)]
             [('off `(on  ,score1 ,score2)) `(off ,score1        ,score2)]
             [('+   `(on  ,score1 ,score2)) `(on  ,(add1 score1) ,score2)]
             [('-   `(on  ,score1 ,score2)) `(on  ,score1        ,(add1 score2))]
             [(bad-input state) state]))

  (define (game input)
    (if (null? input) stateful-get
      (begin/monad
        (stateful-modify (update (car input)))
        (game (cdr input)))))

  (stateful-exec '(off 0 0) (game '(on + - + + - + 'off)))]
@section{Stateful API Reference}
@defproc[(stateful? [v any/c]) boolean?]{}
@defproc[(stateful-run [s any/c] [mx stateful?]) pair?]{}
@defproc[(stateful-exec [s any/c] [mx stateful?]) any/c]{}
@defproc[(stateful-eval [s any/c] [mx stateful?]) any/c]{}
@defproc[(stateful-return [v any/c]) stateful?]{}
@defthing[stateful-get stateful?]{}
@defproc[(stateful-select [f (-> any/c any/c)]) stateful?]{}
@defproc[(stateful-put [s any/c]) stateful?]{}
@defproc[(stateful-modify [f (-> any/c any/c)]) stateful?]{}
