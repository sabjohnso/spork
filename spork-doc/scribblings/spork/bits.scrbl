#lang scribble/doc

@begin[
 (require
   (for-syntax racket scribble/eval spork/functor)
   (for-label racket racket/generic syntax/stx spork/optional spork/functor
     spork/bits)
   scribble/eval
   (except-in racket #%app)
   (except-in scribble/manual link)
   racket/generic
   racket/sandbox)
]

@title{Bits}
@defmodule[spork/bits]
@defproc[(bits? [v any/c]) boolean?]{
 Return @racket[#t] if the argument is a @racketidfont{bits} object. Otherwise, return @racket[#f].
}

@defproc[(make-bits [size natural-number/c] [initial-data natural-number/c 0]) bits?]{
 Return a @racketidfont{bits} value value with @racketidfont{size} bits. The individual bits are set
 according to @racketidfont{initial-data}.
}

@defproc[(bits-ref [bs bits?] [index natural-number/c]) bit/c]{
 Return the value of the specified bit.
}

@defproc[(bits-set [bs bits?] [index natural-number/c] [b bit/c]) bits?]{
  Set the specified bit to the value of the input bit.
}

@defstruct*[byte-spec ([size natural-number/c] [position natural-number/c])]{
  A data structure for specifying a contiguous range of bits within a @racketidfont{bits}
  object.
}

@defproc[(bits-load-byte [bs bits?] [spec byte-spec?]) natural-number/c]{
  Load a specified range of bits.
}

@defproc[(bits-store-byte [bs bits?] [spec byte-spec?] [value natural-number/c]) bits?]{
  Store bits in a specified range.
}
