#lang racket

(provide
 (contract-out
  [core-runner<%> interface?]
  [runner<%> interface?]
  [runnable<%> interface?]
  [runner% (implementation?/c runner<%>)]
  [abstract-runnable% (implementation?/c runnable<%>)]))

(struct status
  (running stopping))

(define core-runner<%>
  (interface ()
    [stop (->m void?)]
    [running? (->m boolean?)]
    [stopping? (->m boolean?)]))

(define runner<%>
  (interface (core-runner<%>)
    [run-thunk (->m (-> any/c) void?)]))

(define runnable<%>
  (interface (core-runner<%>)
    [run (->m void?)]))

(define runner%
  (class* object%
      (runner<%>)
    (super-new)
    (define runner-status (box (status #f #f)))

    (define/public (run-thunk proc)
      (when (should-start)
        (thread (thunk (run-loop proc))))
      (void))

    (define/public (stop)
      (let loop ([current-status (unbox runner-status)])
        (when (and (status-running current-status)
                   (not (status-stopping current-status)))
          (when (not (box-cas! runner-status current-status (status #t #t)))
            (loop)))))

    (define/public (running?)
      (status-running (unbox runner-status)))

    (define/public (stopping?)
      (status-stopping (unbox runner-status)))

    (define (should-start)
      (let ([current-status (unbox runner-status)])
        (and (not (status-running current-status))
             (box-cas! runner-status current-status (status #t #f)))))

    (define (run-loop proc)
      (define (loop)
        (when (should-continue)
          (proc)
          (loop)))
      (loop))

    (define (should-continue)
      (let loop ([current-status (unbox runner-status)])
        (if (status-stopping current-status)
            (begin
              (set-box! runner-status (status #f #f))
              #f)
          #t)))))

(define abstract-runnable%
  (class* runner%
      (runnable<%>)
    (super-new)

    (inherit run-thunk)

    (define/public (run)
      (run-thunk (get-thunk)))

    (abstract get-thunk)))
