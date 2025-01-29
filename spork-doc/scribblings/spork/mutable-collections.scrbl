#lang scribble/doc

@begin[
 (require
   (for-syntax racket scribble/eval spork/functor)
   (for-label racket spork/optional spork/functor spork/mutable-collections/stack racket/generic syntax/stx)
   scribble/eval
   (except-in racket #%app)
   (except-in scribble/manual link)
   racket/generic
   racket/sandbox)
]

@title{Mutable Collections}
@section{Stack}
@defmodule[spork/mutable-collections/stack]
@defproc[(stack? [v any/c]) boolean?]{
  Return @racket[#t] if the argument is a stack value.  Otherwise, return @racket[#f].
}
@defproc[(make-stack) stack?]{
  Return and empty @racket[stack?].
}
@defproc[(stack-push! [stack stack?] [v any/c]) void?]{
  Push a value onto the stack.
}

@defproc[(stack-pop! [stack stack?]) optional?]{
  Remove a value from the stack and return it.
}
@defproc[(stack-clear! [stack stack?]) void?]{
  Remove all items from the stack.
}
@defproc[(stack-empty? [stack stack?]) boolean?]{
  Return @racket[#t] if the stack is empty.  Otherwise, return @racket[#f].
}
