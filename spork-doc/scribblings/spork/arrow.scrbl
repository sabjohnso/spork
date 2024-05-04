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
    @item{fst-proc : accepts an arrow and returns a function
      accepting an arrow
    }
    @item{flatmap-proc : accepts one argument and returns the
      procedure monadic mapping.}
    @item{join-proc : accepts one argument and returns the procedure
      for flattening the monad context.}]
}


