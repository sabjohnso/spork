#lang scribble/doc

@begin[
 (require
  (for-label (except-in racket #%app) spork/parst)
  racket racket/sandbox
  scribble/eval (except-in scribble/manual link))

 (define parst-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
   (make-evaluator 'racket #:requires '(spork/parst))))]

@title[#:tag "Parst"]{Parst}
@defmodule[spork/parst]
@section{Parst Guide}

@section{Parst API Reference}

@defproc[(parser? [v any/c]) boolean?]{}
@defproc[(parse-result? [v any/c]) boolean?]{}
@defproc[(parse-result-value [result parse-result?]) any/c]{}
@defproc[(parse-result-remaining-input [result parse-result?]) stream?]{}

@defproc[(parse [parser parser?] [input stream?]) (stream/c parse-result?)]
@defproc[(parse-success? [results (stream/c parse-result?)]) boolean?]
@defproc[(parse-failure? [results (stream/c parse-result?)]) boolean?]
@defproc[(parse-results-value-set  (stream/c parse-result?)) set?]{}

@subsection{Primitive Parsers and Combinators}

@subsubsection{parser-return}
@defproc[(parser-return [v any/c]) (and/c parser? parser-return?)]{}
@defproc[(parser-return? [v any/c]) boolean?]{}

@subsubsection{parser-fail}
@defproc[(parser-fail) (and/c parser? parser-fail?)]{}
@defproc[(parser-fail? [v any/c]) boolean?]{}

@subsubsection{parser-item}
@defproc[(parser-item) (and/c parser? parser-item?)]{}
@defproc[(parser-item? [v any/c]) boolean?]{}

@subsubsection{parser-empty}
@defproc[(parser-empty [v any/c]) (and/c parser? parser-item?)]{}
@defproc[(parser-empty? [v any/c]) boolean?]{}

@subsubsection{parser-disj}
@defproc[(parser-disj [p1 parser?] [p2 parser?]) (and/c parser? parser-disj?)]{}
@defproc[(parser-disj? [v any/c]) boolean?]{}

@subsubsection{parser-alt}
@defproc[(parser-alt [p1 parser?] [p2 parser?]) (and/c parser? parser-alt?)]{}
@defproc[(parser-alt? [v any/c]) boolean?]{}

@subsubsection{parser-bind}
@defproc[(parser-bind [p1 parser?] [proc (-> any/c parser?)]) (and/c parser? parser-bind?)]{}
@defproc[(parser-bind? [v any/c]) boolean?]{}

@subsubsection{parser-push}
@defproc[(parser-push [v any/c]) (and/c parser? parser-push?)]{}
@defproc[(parser-push? [v any/c]) boolean?]{}

@subsection{parser-peek}
@defproc [(parser-peek [parser parser?]) (and/c parser? parser-peek?)]{}
@defproc [(parser-peek? [v any/c]) boolean?]{}

@subsection{Functor Parsers}

@defproc[(parser-fmap [proc (-> any/c any/c)] [parser parser?]) parser?]{}
@defproc[(parser-fapply [parser-proc parser?] [parser-value parser?]) parser?]{}
@defproc[(parser-flatmap [proc (-> any/c parser?)] [parser parser?]) parser?]{}

@subsection{Compound Parsers}

@defproc[(parser-disj* [parsers parser?] ...) parser?]{}
@defproc[(parser-alt* [parsers parser?] ...) parser?]{}
@defproc[(parser-sat [parser predicate/c]) parser?]{}
@defproc[(parser-eq? [v any/c]) parser?]{}
@defproc[(parser-eqv? [v any/c]) parser?]{}
@defproc[(parser-equal? [v any/c]) parser?]{}
@defproc[(parser-optional-greedy [parser parser?] [default any/c]) parser?]{}
@defproc[(parser-optional-nongreedy [parser parser?] [default any/c]) parser?]{}
@defproc[(parser-zero-or-more-greedy [parser parser?]) parser?]{}
@defproc[(parser-zero-or-more-nongreedy [parser parser?]) parser?]{}
@defproc[(parser-one-or-more-greedy [parser parser?]) parser?]{}
@defproc[(parser-one-or-more-nongreedy [parser parser?]) parser?]{}
@defproc[(parser-sequence [parsers parser?] ...) parser?]{}
@defproc[(parser-unordered-sequence-greedy [parsers parser?] ...) parser?]{}
@defproc[(parser-unordered-sequence-nongreedy [parsers parser?] ...) parser?]{}
@defproc[(parser-fst [parser1 parser?] [parser2 parser?]) parser?]{}
@defproc[(parser-snd [parser1 parser?] [parser2 parser?]) parser?]{}
