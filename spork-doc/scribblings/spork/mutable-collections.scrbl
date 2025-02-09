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



@section{Deque}
@defmodule[spork/mutable-collections/deque]

@defproc[(deque? [v any/c]) boolean?]{
}

@defproc[(make-deque) deque?]{
}

@defproc[(deque-push-back! [deque deque?] [v any/c]) void?]{
}

@defproc[(deque-push-front! [deque deque?] [v any/c]) void?]{
}

@defproc[(deque-pop-back! [deque deque?]) optional?]{
}

@defproc[(deque-pop-front! [deque deque?]) optional?]{
}

@defproc[(deque-clear! [deque deque?]) void?]{
}

@defproc[(deque-empty? [deque deque?]) boolean?]{
}

@section{Minibus}

@defmodule[spork/mutable-collections/minibus]

@definterface[receiver<%> ()]{
  @defmethod*[(((on-message [tag any/c] [data any/c]) void?))]{
   Accept a tag and data, taking whatever action is appropriate for the
   the receiver type, tag and data.  An implementation of @racket[receiver<%>]
   must accept any tag and data values, but may chose to ignore them when
   appropriate.
  }

  @defproc[(reciever? [v any/c]) boolean?]{
    Return @racket[#t] if the input implements @racket[receiver<%>].  Otherwise,
    return @racket[#f].
  }
}

@defstruct*[route ([emitter any/c] [receiver receiver?])]{
  A @racket[route] describes a connection between and emitter and a receiver.
}

@defstruct*[message ([tag any/c] [data any/c])]{
  A @racket[message] describes tagged data to be sent from an emitter to
  a receiver.
}

@definterface[minibus<%> ()]{
  @defmethod*[(((add-route [route route?]) void?))]{
    Add a new route to the minibus.
  }
  @defmethod*[(((remove-route [route route?]) void?))]{
    Remove  aroute from the minibus
  }
  @defmethod*[(((handle-mesage [emitter any/c] [message message?]) void?))]{
    Handle the input message, delivering it to all receivers of the input emitter.
  }
  @defmethod*[(((run) void?))]{
    Run the minibus.
  }
  @defmethod*[(((stop) void?))]{
    Stop the minibus
  }
  @defmethod*[(((running?) boolean?))]{
    Return @racket[#t] if the minibus is running. Otherwise, return @racket[#f].
  }
  @defmethod*[(((stopping?) boolean?))]{
    Return @racket[#t] if the minibus is running but currently in the process of stopping.
    Otherwise, return false.
  }
  @defmethod*[(((queueing?) boolean?))]{
    Return @racket[#t] if the minibus has messages in it's queue.  Otherwise, return
    @racket[#f].
  }

  @defproc[(minibus? [v any/c]) boolean?]{
    Return @racket[#t] if the argument is an instance of a type implementing @racket[minibus<%>].
    Otherwise, return @racket[#f].
  }
}

@defproc[(make-minibus) minibus?]{
  Return a new minibus.
}

@defproc[(minibus-add-route! [minibus minibus?] [route route?]) void?]{
  Add a new route to the minibus.  If the minibus already contains the route,
  no action is taken.
}

@defproc[(minibus-remove-route! [route route?]) void?]{
  Remove a route from a minibus.  If the minibus does not contain the route, No
  action is taken.
}

@defproc[(minibus-handle-messge! [minibus minibus?] [emitter any/c] [message message?]) void?]{
  Handle a message form the input emitter, delivering it to all routes from
  the input emitter.
}

@defproc[(minibus-run! [minibus minibus?]) void?]{
   Start running the minibus.  If the minibus is already running, no action is taken.
}

@defproc[(minibus-stop! [minibus minibus?]) void]{
  Start shutdown of the minibus.  If the minibus is already shutting down, or not running, no
  action is taken.
}

@defproc[(minibus-running? [minibus minibus?]) boolean?]{
  Return @racket[#t] if the minibus is running. Otherwise, return @racket[#f].
}

@defproc[(minibus-stopping? [minibus minibus?]) boolean?]{
  Return @racket[#t] if the minibus is in the process of stopping.  Otherwise,
  return @racket[#f].
}

@defproc[(minibus-queueing? [minibus minibus?]) boolean?]{
  Return @racket[#t] if the minibus is holding messages that are not yet delivered.
  Otherwise, return @racket[#f].
}
