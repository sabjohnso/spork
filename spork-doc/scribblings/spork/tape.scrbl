#lang scribble/doc

@begin[
  (require
    (for-label (except-in racket #%app) spork spork/tape)
    racket racket/sandbox racket spork/tape
    scribble/eval (except-in scribble/manual link))

  (define tape-eval
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-memory-limit 50])
      (make-evaluator 'racket #:requires '(spork spork/tape))))]

@title[#:tag "tape"]{Tape}
@defmodule[spork/tape]
@section{Tape API Reference}
A tape is a data structure representing a sequence of values with a
position, and is often referred to as a zipper.  Here, the name tape
is chosen for the analogy to a reel-to-reel tape player.

@defproc[(tape? [v any/c]) boolean?]{
  Returns @racket[#t] if v is a tape. Otherwise, returns @racket[#f].
}

@defproc[(tape [feed list?] [takeup list?]) tape?]{
  Returns a tape with the elements of the first argument list as
  feed and the elements of the second arguemnt list as takeup.
}

@defproc[(tape-feed [tp tape?]) list?]{
  Return the list of elements in the tape's feed reel.
}

@defproc[(tape-takeup [tp tape?]) list?]){
  Return the list of element in the tape's takeup real.
}

@defproc[(make-tape [v any/c] ...) tape?]{
  Returns a tape with the arguments as elements.
}

@defproc[(list->tape [lst list?]) tape?]{
  Returns a tape with the elements of the input list as elements of the tape.
}

@defproc[(tape->list [tp tape?]) list?]{
  Returns a list with the elements of the input tape.
}

@defproc[(vector->tape [vec vector?]) tape?]{
  Returns a tape with the elements of the input vector as elements of the tape.
}

@defproc[(tape-empty? [tp tape?]) boolean?]{
  Returns @racket[#t] if the input tape is empty. Otherwise, returns @racket[#f].
}

@defproc[(tape-at-front? [tp tape?]) boolean?]{
  Returns @racket[#t] if the tape is at the first position.
}

@defproc[(tape-at-back? [tp tape?]) boolean?]{
  Returns @racket[#t] if the tape is at the last position.
}

@defproc[(tape-position? [tp tape?]) natural-number/c]{
  Returns the current position of the input tape.
}

@defproc[(tape-remaining? [tp tape?]) natural-number/c]{
  Returns the number of elements left in the feed real.
}

@defproc[(tape-length [tp tape?]) natural-number/c]{
  Returns the total number of elements in the tape.
}

@defproc[(tape-fwd [tp tape?]) tape?]{
  Returns the taped moved forward by one element.
}

@defproc[(tape-fwd-by [tp tape?] [n natural-number/c]) tape?]{
  Returns the taped moved forward by the specified number of
  elements.
}

@defproc[(tape-fast-fwd [tp tape?]) tape?]{
  Returns the tape moved forwared to the end.
}

@defproc[(tape-bwd [tp tape?]) tape?]{
  Returns the tape moved backward by one elment.
}

@defproc[(tape-bwd-by [tp tape?] [n natural-number/c]) tape?]{
  Returns the tape moved backward by the specified number of
  elements.
}

@defproc[(tape-rewind [tp tape?]) tape?]{
  Returns the tape moved back to the begining.
}

@defproc[(tape-move-by [tp tape?] [n exact-integer?])  tape?]{
  Returns the tape moved by the specified number of elements.
}

@defproc[(tape-move-to [tp tape?] [n natural-number/c]) tape?]{
  Returns the tape moved to the specified position.
}

@defproc[(tape-append [tp tape?] ...) tape?]{
  Returns a tape with the elements of all of the input tapes.
}

@defproc[(tape-splice [tp tape?] ...) tape?]{
  Returns a tape with the elements of all of the input tapes.
}

@defproc[(tape-read [tp tape?] ...) any/c]{
  Returns the value of the element at the current position
  of the tape.
}

@defproc[(tape-refabs [tp tape?] [n natural-number/c]) any/c]{
  Returns the value of the element of the tape at the specified
  absolution position.
}

@defproc[(tape-refrel [tp tape?] [n exact-integer?]) any/c]{
  Returns the value of the element of the tape at the specified
  relative-offset from the current position.
}

@defproc[(tape-write [vs tape?] [v any/c]) tape?]{
  Returns a tape with the current value replaced by the input value.
}

@defproc[(tape-insert [vs tape?] [v any/c]) tape?]{
  Returns a tape with the input value inserted before the current value.
}

@defproc[(tape-reverse [tp tape?]) tape?]{
  Return a tape with the elements reverse.
}
