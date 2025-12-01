#lang racket

(provide
 struct/stateful
 (contract-out
  (struct stateful ([proc (-> any/c pair?)]))
  [stateful-run (-> any/c stateful? pair?)]
  [stateful-exec (-> any/c stateful? any/c)]
  [stateful-eval (-> any/c stateful? any/c)]
  [stateful-return (-> any/c stateful?)]
  [stateful-get stateful?]
  [stateful-select (-> (-> any/c any/c) stateful?)]
  [stateful-put (-> any/c stateful?)]
  [stateful-modify (-> (-> any/c any/c) stateful?)]
  [undefined-value? predicate/c]
  [undefined-value undefined-value?]))

(require
 (for-syntax racket racket/syntax syntax/parse)
 spork/tag spork/functor spork/struct-extras)

(struct stateful
  (proc)
  #:methods gen:monad
  ((define (return-proc _) stateful-return)
   (define (flatmap-proc _) stateful-flatmap)))

(define (stateful-run s mx)
  ((stateful-proc mx) s))

(define (stateful-eval s mx)
  (car (stateful-run s mx)))

(define (stateful-exec s mx)
  (cdr (stateful-run s mx)))

(define (stateful-return x)
  (stateful (λ (s) (cons x s))))

(define (stateful-flatmap f mx)
  (stateful
   (λ (s)
     (match-let ([(cons x s) (stateful-run s mx)])
       (stateful-run s (f x))))))

(define-tag undefined-value)

(define stateful-get
  (stateful (λ (s) (cons s s))))

(define (stateful-select f)
  (stateful (λ (s) (cons (f s) s))))

(define (stateful-put s)
  (stateful (λ (_) (cons undefined-value s))))

(define (stateful-modify f)
  (stateful (λ (s) (cons undefined-value (f s)))))

(begin-for-syntax
 (define (make-field-state-readers struct-name/stx field-names/stx)
   (for/list ([field-name/stx (syntax-e field-names/stx )])
     (make-field-state-reader struct-name/stx field-name/stx)))

 (define (make-field-state-reader struct-name/stx field-name/stx)
   (with-syntax ([field-reader (format-id field-name/stx "~a-~a" struct-name/stx field-name/stx)]
                 [field-reader/stateful (format-id field-name/stx "~a-~a/stateful" struct-name/stx field-name/stx)])
        (syntax/loc field-name/stx
          (define field-reader/stateful (stateful-select field-reader)))))

 (define (make-field-state-writers struct-name/stx field-names/stx)
   (for/list ([field-name/stx (syntax-e field-names/stx)])
     (make-field-state-writer struct-name/stx field-name/stx)))

 (define  (make-field-state-writer struct-name/stx field-name/stx)
   (with-syntax ([struct-name struct-name/stx]
                 [field-name field-name/stx]
                 [reader (format-id field-name/stx "~a-~a" struct-name/stx field-name/stx)]
                 [set/stateful (format-id field-name/stx "set-~a-~a/stateful" struct-name/stx field-name/stx)]
                 [s (generate-temporary 's)]
                 [v (generate-temporary 'v)])
     (syntax/loc field-name/stx
       (define (set/stateful v)
         (stateful-modify
          (λ (s)
            (struct-copy struct-name s
              [field-name v]))))))))

(define-syntax (struct/stateful stx)
  (syntax-parse stx
    [(_ struct-name:id (fields:field-decl ...) options:struct-options)
     (with-syntax ([(reader-defs ...) (make-field-state-readers #'struct-name #'(fields.field-name ...))]
                   [(writer-defs ...) (make-field-state-writers #'struct-name #'(fields.field-name ...))]
                   [(options ...) #'options])
       (syntax/loc stx
         (begin
           (struct struct-name
             (fields ...)
             options ...)
           reader-defs ...
           writer-defs ...)))]))
