#lang scribble/doc

@begin[
 (require
   (for-syntax racket scribble/eval spork/functor)
   (for-label racket racket/generic syntax/stx spork/optional spork/functor
     spork/mutable-collections/stack
     spork/mutable-collections/queue)
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
  Return @racket[#t] if the argument is a stack.  Otherwise, return @racket[#f].
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

@section{Queue}
@defmodule[spork/mutable-collections/queue]
@defproc[(queue? [v any/c]) boolean?]{
  Return @racket[#t] if the argument is a queue.  Otherwise, return @racket[#f].
}
@defproc[(make-queue) queue?]{
  Return and empty @racket[queue?].
}
@defproc[(queue-push-back! [queue queue?] [v any/c]) void?]{
  Push a value onto the back of a queue.
}
@defproc[(queue-pop-front! [queue queue?]) optional?]{
  Pop a value from the front of a queue.
}
@defproc[(queue-clear! [queue queue?]) optional?]{
  Remove all values from a queue.
}
@defproc[(queue-empty? [queue queue?]) boolean?]{
  Return @racket[#t] if the input queue is empty.  Otherwise, return @racket[#f].
}
