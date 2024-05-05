#lang scribble/doc

@begin[(require (except-in scribble/manual link) racket racket/sandbox scribble/eval)]

@title[#:tag "arrow"]{Arrow}
@defmodule[spork/arrow]
@section{Arrow Guide}
@subsection{Using Arrows}
@subsection{Using Conditional Arrows}

@section{Arrow API Reference}
@subsection{Category}
@defthing[gen:category any/c]{
}

@subsection{Arrows}
@defthing[gen:arrow any/c]{
  @itemize[
    @item{@racketidfont{arr-proc} : accepts an arrow and returns a
    procedure to lift a function into the arrow's context.}

    @item{@racketidfont{fst-proc} : accepts an arrow and returns a function accepting
      an arrow and returning an arrow applying the argument arrow to
      the first input value.}

    @item{@racketidfont{snd-proc} : accepts an arrow and returns a
      function accepting an arrow and returning an arrow applying the
      argument arrow to the second input value.}

    @item{@racketidfont{split-proc} : accepts an arrow and returns and
      a procedure accepting two arrows and return in an arrow that
      accepts a pair and applies the argument arrows to the @racket[car]
      and @racket[cdr] of its input.}

    @item{@racketidfont{fanout-proc} : accepts an arrow for method
       lookup and returns the method which accepts to argument arrows,
       and returns an arrow. The returned arrow accepts an argument
       and returns a pair consisting of the results of applying the
       first argument arrow and the second argument arrow to the
       input. }

     @item{@racketidfont{arrow-comp-proc}: accpets an arrow for method
       lookup and reeturns teh method which composes two arrows. }

     @item{@racketidfont{arrow-id-value}: accepts an arror for lookup
       and returns the identity element for the arrow. }]}

@defproc[(arr [v (-> any/c any/c)]) arrow?]{
  Lift a function into an unresolved arrow context. }

@defproc[(fst [a arrow?]) arrow?]{
  Return an arrow acting on the first item of a pair with the argument
  arrow. }

@defproc[(snd [a arrow?]) arrow?]{
  Return an arrow acting on the second item of a pair with the
  argument arrow. }

@subsubsection{Split}
@defproc[(split [a arrow?] [b arrow?]) arrow?]{
  Return an arrow acting on both items in a pair with the first and
  second argument arrows. }

@defproc[(*** [a arrow?] ...) arrow?]{
  Return an arrow splitting the input across the argument arrows.
}
@defproc[(^** [f function?] [a arrow?]) arrow?]{}
@defproc[(**^[a arrow?] [f function?] ) arrow?]{}
@defproc[(^*^[f function?] [g function?] ) arrow?]{}

@subsubsection{Fanout}
@defproc[(fanout [a arrow?] [b arrow?]) arrow?]{}
@defproc[(&&& [a arrow?] ...) arrow?]{
  Return an arrow sending the input to each of the argument arrrows.
}
@defproc[(^&& [f function?] [a arrow?]) arrow?]{}
@defproc[(&&^[a arrow?] [f function?] ) arrow?]{}
@defproc[(^&^[f function?] [g function?] ) arrow?]{}

@subsubsection{Right-to-left arrow composition}
@defproc[(<<< [a arrow?] ...) arrow?]{
  Right-to-left composition of arrows.
}
@defproc[(^<< [f function?] [a arrow?]) arrow?]{}
@defproc[(<<^[a arrow?] [f function?] ) arrow?]{}
@defproc[(^<^[f function?] [g function?] ) arrow?]{}


@subsubsection{Left-to-right arrow composition}
@defproc[(>>> [a arrow?] ...) arrow?]{
  Left-to-right composition of arrows.
}
@defproc[(^>> [f function?] [a arrow?]) arrow?]{}
@defproc[(>>^[a arrow?] [f function?] ) arrow?]{}
@defproc[(^>^[f function?] [g function?] ) arrow?]{}

@subsection{Arrow Choice}
@defthing[gen:arrow-choice any/c]{}

@defproc[(choose-left [f arrow-choice?]) arrow-choice?]{}

@defproc[(choose-right [f arrow-choice?]) arrow-choice?]{}

@defproc[(choose [f arrow-choice?] [g arrow-choice?]) arrow-choice?]{}
@defproc[(fanin [f arrow-choice?] [g arrow-choice?]) arrow-choice?]{}

@subsubsection{Choose}
@defproc[(+++ [f arrow-choice?] [g arrow-choice?] [h arrow-choice] ...) arrow-choice?]{}

@defproc[(^++ [f function?] [a arrow-choice?]) arrow-choice?]{}
@defproc[(++^[a arrow-choice?] [f function?] ) arrow-choice?]{}
@defproc[(^+^[f function?] [g function?] ) arrow-choice?]{}

@defproc[(/// [f arrow-choice?] [g arrow-choice?] [h arrow-choice]...) arrow-choice?]{}
@defproc[(^// [f function?] [a arrow-choice?]) arrow-choice?]{}
@defproc[(//^[a arrow-choice?] [f function?] ) arrow-choice?]{}
@defproc[(^/^[f function?] [g function?] ) arrow-choice?]{}
