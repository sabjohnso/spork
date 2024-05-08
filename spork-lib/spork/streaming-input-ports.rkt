#lang racket
;; This module introduces ports as streams which differs from the
;; usual Racket treatment of ports. A file or string maybe read in as a stream
;; of characters, bytes, line strings or line bytes.  The port that is
;; opened for the stream is closed when the stream is garbage collected.
;;
;; Additionally, one may stream from an already open port, but the port must
;; be managed outside of the stream.  This is unsafe.

(provide
 (contract-out
  [gzipped-file->line-stream (-> path-string? stream?)]
  [gzipped-file->bytes-line-stream (-> path-string? stream?)]
  [gzipped-file->char-stream (-> path-string? stream?)]
  [gzipped-file->byte-stream (-> path-string? stream?)]
  [gzipped-file->sexp-stream (-> path-string? stream?)]

  [file->line-stream (-> path-string? stream?)]
  [file->bytes-line-stream (-> path-string? stream?)]
  [file->char-stream (-> path-string? stream?)]
  [file->byte-stream (-> path-string? stream?)]
  [file->sexp-stream (-> path-string? stream?)]

  [string->line-stream (-> string stream?)]
  [string->bytes-line-stream (-> string? stream?)]
  [string->char-stream (-> string? stream?)]
  [string->byte-stream (-> string? stream?)]

  [bytes->line-stream (-> bytes stream?)]
  [bytes->bytes-line-stream (-> bytes stream?)]
  [bytes->char-stream (-> bytes stream?)]
  [bytes->byte-stream (-> bytes stream?)]

  [unsafe-input-port->line-stream (-> input-port? stream?)]
  [unsafe-input-port->bytes-line-stream (-> input-port? stream?)]
  [unsafe-input-port->char-stream (-> input-port? stream?)]
  [unsafe-input-port->byte-stream (-> input-port? stream?)]))

(require
 racket/fasl
 spork/curried spork/functor)

;;
;; ... compressed input files
;;
(define (gzipped-file->line-stream path)
  (managed-input-port->line-stream (open-managed-gzipped-input-file path)))

(define (gzipped-file->bytes-line-stream path)
  (managed-input-port->bytes-line-stream (open-managed-gzipped-input-file path)))

(define (gzipped-file->char-stream path)
  (managed-input-port->char-stream (open-managed-gzipped-input-file path)))

(define (gzipped-file->byte-stream path)
  (managed-input-port->byte-stream (open-managed-gzipped-input-file path)))

(define (gzipped-file->sexp-stream path)
  (managed-input-port->sexp-stream (open-managed-gzipped-input-file path)))

;;
;; ... input-files
;;
(define (file->line-stream path)
  (managed-input-port->line-stream (open-managed-input-file path)))

(define (file->bytes-line-stream path)
  (managed-input-port->bytes-line-stream (open-managed-input-file path)))

(define (file->char-stream path)
  (managed-input-port->char-stream (open-managed-input-file path)))

(define (file->byte-stream path)
  (managed-input-port->byte-stream (open-managed-input-file path)))

(define (file->sexp-stream path)
  (managed-input-port->sexp-stream (open-managed-input-file path)))


;;
;; ... input strings
;;
(define (string->line-stream string)
  (managed-input-port->line-stream (open-managed-input-string string)))

(define (string->bytes-line-stream string)
  (managed-input-port->bytes-line-stream (open-managed-input-string string)))

(define (string->char-stream str)
  (managed-input-port->char-stream (open-managed-input-string str)))

(define (string->byte-stream string)
  (managed-input-port->byte-stream (open-managed-input-string string)))


;;
;; ... input-bytes
;;
(define (bytes->line-stream path)
  (managed-input-port->line-stream (open-managed-input-bytes path)))

(define (bytes->bytes-line-stream path)
  (managed-input-port->bytes-line-stream (open-managed-input-bytes path)))

(define (bytes->char-stream path)
  (managed-input-port->char-stream (open-managed-input-bytes path)))

(define (bytes->byte-stream path)
  (managed-input-port->byte-stream (open-managed-input-bytes path)))


;;
;; ... input-port
;;
(define (unsafe-input-port->line-stream inp)
  (stream-lazy
   (let ([line (read-line inp)])
     (if (eof-object? line) empty-stream
       (stream-cons line (unsafe-input-port->line-stream inp))))))

(define (unsafe-input-port->bytes-line-stream inp)
  (stream-lazy
   (let ([line (read-bytes-line inp)])
     (if (eof-object? line) empty-stream
       (stream-cons line (unsafe-input-port->bytes-line-stream inp))))))

(define (unsafe-input-port->char-stream inp)
  (stream-lazy
   (let ([char (read-char inp)])
     (if (eof-object? char) empty-stream
       (stream-cons char (unsafe-input-port->char-stream inp))))))

(define (unsafe-input-port->byte-stream inp)
  (stream-lazy
   (let ([byte (read-byte inp)])
     (if (eof-object? byte) empty-stream
       (stream-cons byte (unsafe-input-port->byte-stream inp))))))

;;
;; ... managed-ports to streams
;;
(define (managed-input-port->line-stream inp)
  (match (managed-read-line inp)
    [(and (? string?) string) (stream-cons string (managed-input-port->line-stream inp))]
    [(? eof-object?) empty-stream]))

(define (managed-input-port->bytes-line-stream inp)
  (match (managed-read-bytes-line inp)
    [(and (? bytes?) bytes) (stream-cons bytes (managed-input-port->bytes-line-stream inp))]
    [(? eof-object?) empty-stream]))

(define (managed-input-port->char-stream inp)
  (match (managed-read-char inp)
    [(and (? char?) char) (stream-cons char (managed-input-port->char-stream inp))]
    [(? eof-object?) empty-stream]))

(define (managed-input-port->byte-stream inp)
  (match (managed-read-byte inp)
    [(and (? byte?) byte) (stream-cons byte (managed-input-port->byte-stream inp))]
    [(? eof-object?) empty-stream]))

(define (managed-input-port->sexp-stream inp)
  (match (managed-peek-byte inp)
    [(? eof-object?) empty-stream]
    [_ (stream-cons (managed-read-fasl inp) (managed-input-port->sexp-stream inp))]))

(define managed-peek-byte (compose extract (fmap peek-byte)))
(define managed-read-char (compose extract (fmap read-char)))
(define managed-read-byte (compose extract (fmap read-byte)))
(define managed-read-line (compose extract (fmap read-line)))
(define managed-read-bytes-line (compose extract (fmap read-bytes-line)))
(define managed-read-fasl (compose extract (fmap fasl->s-exp)))

(define (open-managed-input-file path)
  (wrap-and-register-input-port (open-input-file path)))

(define (open-managed-input-string str)
  (wrap-and-register-input-port (open-input-string str)))

(define (open-managed-input-bytes bytes)
  (wrap-and-register-input-port (open-input-bytes bytes)))

(define zcat (find-executable-path "zcat"))

(define (open-managed-gzipped-input-file path)
  (let-values ([(subproc from-zcat to-zcat error-from-zcat) (subprocess #f #f #f zcat path)])
    (close-output-port to-zcat)
    (let ([from-zcat-managed (managed-input-port from-zcat)])
      (will-register will-executor from-zcat-managed
		     (λ (managed)
		       (fmap close-input-port managed)
		       (close-input-port error-from-zcat)))
      from-zcat-managed)))

(struct managed-input-port
  (input-port)
  #:methods gen:trivial
  [(define (wrap-proc trivial) managed-input-port)
   (define (unwrap-proc trivial) managed-input-port-input-port)])

(define (wrap-and-register-input-port inp)
  (let ([inp-managed (managed-input-port inp)])
    (will-register will-executor inp-managed (fmap close-input-port))
    inp-managed))

(define will-executor
  (make-will-executor))

(define current-initial-executor-interval-milliseconds
  (make-parameter 0.1))

(define current-max-executor-interval-milliseconds
  (make-parameter 100.0))

(define current-executor-backoff-factor
  (make-parameter 1.1))

(let ()
  (thread
   (thunk
    (let loop ([interval (current-initial-executor-interval-milliseconds)])
      (if (will-try-execute will-executor)
          (loop (current-initial-executor-interval-milliseconds))
        (begin
          (sleep interval)
          (loop (max (* interval (current-executor-backoff-factor))
                     (current-max-executor-interval-milliseconds))))))))
  (void))
