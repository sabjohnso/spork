#lang scribble/doc

@begin[
  (require
    scribble/eval
    (except-in racket #%app) racket/sandbox  racket/generic 
                (except-in scribble/manual link)
         (for-syntax racket scribble/eval spork/functor)
         (for-label racket spork/functor racket/generic syntax/stx))]

@(define functor-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
      (make-evaluator 'racket  #:requires '(spork))))
      

@title[#:tag "functor"]{Functor}
@defmodule[spork/functor]
Functors, Applicative Functors, Monads, and Comonads

This library provides an extensive framework for working with
functors, applicative functors, monads, and comonads, tailored for
Racket programmers interested in functional programming paradigms. It
defines generic interfaces and implementations, extending the
capabilities for handling common functional patterns in Racket.

The provided module includes a variety of generic interfaces and
concrete implementations for dealing with trivial, comonad, monad, and
applicative functor structures. It includes procedures for
transforming, composing, and manipulating these structures.

Each function, macro, and syntax extension provided by the module is
documented with examples, partitioned for @tech{functor},
@tech{applicative functor}, @tech{monad}, @tech{comonad} and
@tech{trivial} contexts.

@section{Functor Guide}
@subsection{Using Functors}
@subsection{Using Applicative Functors}
@subsection{Using Monads}
@subsection{Using Comonads}
@subsection{Using Trivials}
@subsection{Using Built-in Contexts}
@subsubsection{Using Lists}
@subsubsection{Using Nonempty Lists}
@subsubsection{Using Vectors}
@subsubsection{Using Streams}
@subsubsection{Using Nonempty Streams}
@subsubsection{Using Pairs}
@subsubsection{Using Functions}
@subsubsection{Using Thunks}
@subsubsection{Using Futures}
@subsection{Using Sportk Defined Contexts}
@subsubsection{Using Optional Monad}
@subsubsection{Using Either Monad}
@subsubsection{Using Expect Monad}
@subsubsection{Using Environment Monad}
@subsubsection{Using State Monad}


@section{Functor API Reference}
Each function, macro, and syntax extension provided by the module is
documented with examples.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Functors}

A @deftech{functor} is a programming context that permits the
application of a function to each element within an instance of that
context, preserving its shape while potentially changing its
contents. More succinctly, it is a context that permits mapping.

The @tech{functor} protocol is intended to enable a more uniform usage
of the @tech{functor} concept in Racket while eliminating some
boilerplate code. It is enabled by the @racket[gen:functor] generic
interface.

The @tech{functor} protocol intends to standardizes the implementation
of the @tech{functor} concept across various data structures and other
contexts, thereby reducing repetitive boilerplate code. This protocol
is facilitated through the generic interface @racket[gen:functor], which
provides a unified framework for implementing the necessary components
to support functor operations. By using this protocol, different data
types can uniformly support functorial operations, enhancing code
modularity and interoperability.

@defthing[gen:functor any/c]
 Associates a required fmap-proc method with a structure type to
 implement a generic interface
 (see  @secref["struct-generics" #:doc '(lib "scribblings/reference/reference.scrbl")])
 for functors.

 To supply method implementations, the @racket[#:methods] keyword
 should be used in s structure type definition.  The following method
 should be implemented:

@itemize[
  @item{fmap-proc : accepts one argument and returns the
    procedure for mapping a function on unembellished values over
    the values wrapped up in the contex.}]

@examples[#:eval functor-eval
  (struct my-struct (value)
    #:methods gen:functor
   ((define (fmap-proc _) my-struct-fmap)))

  (define (my-struct-fmap f mx)
    (my-struct (f (my-struct-value mx))))

  (define my-value (my-struct 'x))
  (functor? my-value)
  (fmap symbol->string my-value)]

@defproc[(functor? [v any/c]) boolean?]{
  Checks whether the provided value @racket[v] implements the
  @tech{functor} protocol. This function returns:

  @itemize[
    @item{@racket[#t]: if @racket[v] implements the @tech{functor} protocol}
    @item{@racket[#f]: otherwise}]}

@defproc[(fmap [f (-> any/c any/c)] [mx functor?]) functor?]
Applies a function to the values contained in a functor @tech{functor}
context while preserving the structure of the context.


@defproc[(<$> [f (-> any/c any/c)] [mx functor?]) functor?]
A synonym for @racket[fmap] that many be preferable to @racket[fmap] with
infix notation.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Applicative}

@defthing[gen:applicative any/c]{
  @itemize[
    @item{pure-proc : accepts one argument and returns the
      procedure for injecting the value into the applicative context.}
    @item{fapply-proc : mapps a function in the applicative context
      over a value in the applicative context.}]}

@defproc[(applicative? [v any/c]) boolean?]{
  Recognizes values that implement the @racket[gen:applicative] protocol.}

@defproc[(pure [v any/c]) applicative?]{
  Similar to return for monads, wraps a value in an applicative functor
  context.

  Note, because the intended context cannot be determined from
  an application of @racket[pure], the value is wrapped in an unresolved
  context until it can be resolved. At that point, the value is removed
  from the unresolved context (it is actually a
  @secref["Trivial" #:doc '(lib "scribblings/spork.scrbl")] context)
  and injected into the intended context. }

@defproc[(fapply [mf applicative?] [mx applicative?]) applicative?]{
  Applies a function wrapped in an applicative context to a value
  wrapped in the same applicative context.}

@defproc[(fapply* [mf applicative?] [mx applicative?] [mys applicative?] ... ) applicative?]{
  Successively applies a function wrapped in an applicative context to values
  wrapped in the same applicative context.}

@defproc[(<*> [mf applicative?] [mx applicative?]) applicative?]{
  A synonym for @racket[fapply*] that may be preferable with infix notation.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Monad}

@defthing[gen:monad any/c]{
  @itemize[
    @item{return-proc : accepts one argument and returns the
      procedure for injecting the value into the monad context.}
    @item{flatmap-proc : accepts one argument and returns the
      procedure monadic mapping.}
    @item{join-proc : accepts one argument and returns the procedure
      for flattening the monad context.}]}

@defproc[(monad? [v any/c]) boolean?]

@defproc[(return [x any/c]) monad?]
Wraps a value in a monad context, similar to the behavior of wrap in a
trivial context. 

@defproc[(flatmap [f (-> any/c monad?)] [mx monad?]) monad?]{
  Maps a function over a monad and flattens the result, chaining monad
  transformations.}

@defproc[(join [mmx monad?]) monad?]{
  Flattens a nested monad structure by one level, merging two layers of
  monadic context into one.}

@defproc[(>=> [fs (-> any/c monad?)] ...) (-> any/c monad?)]{
 Left-to-right Kleisli composition}

@defproc[(<=< [fs (-> any/c monad?)] ...) (-> any/c monad?)]{
 Right-to-left Kleisli composition}

@defproc[(>>= [mx monad?] [f (-> any/c monad?)] ...) monad?]{
  Monad pipe.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Comonad}
A @deftech{comonad} context is the categorical dual of a @tech{monad}.

@defthing[gen:comonad any/c]{
  @itemize[
    @item{extract-proc : accepts one argument and returns the
      procedure for injecting the value into the monad context.}
    @item{extend-proc : accepts one argument and returns the
      procedure comonadic mapping.}
    @item{duplicate-proc : accepts one argument and returns the procedure
      for flattening the monad context.}]}

@defproc[(comonad? [v any/c]) boolean?]{
  Recognizes values that implement @racket[gen:comonad].}

@defproc[(extract [wx comonad?]) any/c]
Extracts the contained value.

@defproc[(duplicate [wx comonad?]) comonad?]
Duplicates the comonad structure.

@defproc[(extend [f (-> comonad? any/c)] [wx comonad?]) comonad?]
Applies a function to the comonad and wraps the result into the same
comonad context.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection{Trivial}

A @deftech{trivial} context is a context that provides the facilities to
readily inject or extract a value. As such, they are isomorphic with
unembellished values and are monads and comonads.

@defthing[gen:trivial any/c]{
  @itemize[
    @item{wrap-proc : accepts one argument and returns the
      procedure for injecting the value into the trivial context.}
    @item{unwrap-proc : accepts one argument and returns the
      procedure for extracting a value from the trivial context.}]}

@defproc[(trivial? [v any/c]) boolean?]{
  Checks whether the argument @racket[v] implements the @racket[gen:trivial]
  protocol.}

@defproc[(wrap [v any/c]) trivial?]{
  Returns a value in a trivial context. Application of wrap lack the
  information that is necessary to resolve the intended context and
  returns a value in an unresolved context, which will be resolved
  when sufficient information is available to do so.}

@defproc[(unwrap [tv trivial?]) any/c]{
  Extracts the value from a trivial context, effectively reversing the
  wrap operation.}
  
@itemize[
  @item{thunk}
  @item{future}]

@subsection{Built-in Contexts}
@subsubsection{Lists}
@subsubsection{Nonempty Lists}
@subsubsection{Vectors}
@subsubsection{Streams}
@subsubsection{Nonempty Streams}
@subsubsection{Pairs}
@subsubsection{Functions}
@subsubsection{Thunks}
@subsubsection{Futures}

@subsection{Spork Defined Contexts}
@subsubsection{Optional Monad}
@subsubsection{Either Monad}
@subsubsection{Expect Monad}
@subsubsection{Environment Monad}
@subsubsection{State Monad}

