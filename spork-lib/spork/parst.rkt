#lang racket

(provide
 (contract-out
  (struct parse-result ([value any/c] [remaining-input stream?]))
  [parse-results-value-set (-> (stream/c parse-result?) set?)]
  [parse-failure? (-> (stream/c parse-result?) boolean?)]
  [parse-success? (-> (stream/c parse-result?) boolean?)]
  [parser? predicate/c]
  [parse (-> parser? stream? (stream/c parse-result?))]

  (struct parser-return ([value any/c]))
  (struct parser-fail ())
  (struct parser-item ())
  (struct parser-empty ([value any/c]))
  (struct parser-disj ([first parser?] [second parser?]))
  (struct parser-alt ([first parser?] [second parser?]))
  (struct parser-bind ([parser parser?] [proc (-> any/c parser?)]))
  (struct parser-push ([value any/c]))
  (struct parser-peek ([parser parser?]))

  [parser-fmap (-> (-> any/c any/c) parser? parser?)]
  [parser-fapply (-> parser? parser? parser?)]
  [parser-flatmap (-> (-> any/c parser?) parser? parser?)]

  [parser-disj* (->* (parser?) #:rest (listof parser?)  parser?)]
  [parser-alt* (->* (parser?) #:rest (listof parser?)  parser?)]
  [parser-sat (-> predicate/c parser?)]
  [parser-eq? (-> any/c parser?)]
  [parser-eqv? (-> any/c parser?)]
  [parser-equal? (-> any/c parser?)]
  [parser-optional-greedy (-> parser? any/c parser?)]
  [parser-optional-nongreedy (-> parser? any/c parser?)]
  [parser-one-or-more-greedy (-> parser? parser?)]
  [parser-one-or-more-nongreedy (-> parser? parser?)]
  [parser-zero-or-more-greedy (-> parser? parser?)]
  [parser-zero-or-more-nongreedy (-> parser? parser?)]
  [parser-sequence (->* (parser?) #:rest (listof parser?) parser?)]
  [parser-descend (-> parser? parser?)]
  [parser-unordered-sequence-greedy (->* () #:rest (listof parser?) parser?)]
  [parser-unordered-sequence-nongreedy (->* () #:rest (listof parser?) parser?)]
  [parser-fst (-> parser? parser? parser?)]
  [parser-snd (-> parser? parser? parser?)]))

(require
 srfi/26
 spork/stream-extras spork/functor)

(struct parser
  ()
  #:methods gen:monad
  ((define (return-proc _) parser-return)
   (define (flatmap-proc _) parser-flatmap))
  #:transparent)

(struct parser-return parser (value) #:transparent)
(struct parser-fail parser () #:transparent)
(struct parser-item parser () #:transparent)
(struct parser-empty parser (value) #:transparent)
(struct parser-disj parser (first second) #:transparent)
(struct parser-alt parser (first second) #:transparent)
(struct parser-bind parser (parser proc) #:transparent)
(struct parser-push parser (value) #:transparent)
(struct parser-peek parser (parser) #:transparent)

(struct parse-result
  (value remaining-input)
  #:transparent)

(define (parse-results-value-set results)
  (list->set (stream->list (stream-map parse-result-value results))))

(define (parse-failure? x)
  (stream-empty? x))

(define (parse-success? x)
  (not (parse-failure? x)))

(define (parse parser input)
  (match parser
    [(parser-return value) (stream (parse-result value input))]

    [(parser-fail) empty-stream]

    [(parser-item)
     (if (stream-empty? input) (parse (parser-fail) input)
       (parse (parser-return (stream-first input)) (stream-rest input)))]

    [(parser-empty value)
     (if (stream-empty? input) (parse (parser-return value) input)
       (parse (parser-fail) input))]

    [(parser-disj parser1 parser2)
     (stream-append
      (parse parser1 input)
      (parse parser2 input))]

    [(parser-alt parser1 parser2)
     (let ([results (parse parser1 input)])
       (if (parse-success? results) results
         (parse parser2 input)))]

    [(parser-bind parser proc)
     (let/monad ([(parse-result value remaining-input) (parse parser input)])
       (parse (proc value) remaining-input))]

    [(parser-push value)
     (parse (parser-return value) (stream-cons value input))]

    [(parser-peek parser)
     (let/monad ([(parse-result value _) (parse parser input)])
       (parse (parser-return value) input))]

    [(? unresolved?)
     (parse (parser-return (unresolved-value parser)) input)]))

(define (parser-fmap f parser)
  (fmap f parser))

(define (parser-fapply parser-f parser-x)
  (let/applicative ([f parser-f]
                    [x parser-x])
    (f x)))

(define  (parser-flatmap proc parser)
  (parser-bind parser proc))

(define (parser-disj* parser . parsers)
  (if (null? parsers) parser
    (parser-disj parser (apply parser-disj* parsers))))

(define (parser-alt* parser . parsers)
  (if (null? parsers) parser
    (parser-alt parser (apply parser-alt* parsers))))

(define (parser-sat pred?)
  (let/monad ([x (parser-item)])
    (if (pred? x) (parser-return x)
      (parser-fail))))

(define (parser-optional-nongreedy parser default-value)
  (parser-disj parser (parser-return default-value)))

(define (parser-optional-greedy parser default-value)
  (parser-alt parser (parser-return default-value)))

(define (parser-zero-or-more-nongreedy parser)
  (define (recur accum)
    (let/monad ([x parser])
      (parser-disj (recur (cons x accum))
            (parser-return (reverse (cons x accum))))))
  (parser-disj
   (recur '())
   (parser-return '())))

(define (parser-zero-or-more-greedy parser)
  (define (recur accum)
    (let/monad ([x parser])
      (parser-alt (recur (cons x accum))
           (parser-return (reverse (cons x accum))))))
  (parser-alt
   (recur '())
   (parser-return '())))

(define (parser-one-or-more-nongreedy parser)
  (define (recur accum)
    (let/monad ([x parser])
      (parser-disj (recur (cons x accum))
                   (parser-return (reverse (cons x accum))))))
  (recur '()))

(define (parser-one-or-more-greedy parser)
  (define (recur accum)
    (let/monad ([x parser])
      (parser-alt (recur (cons x accum))
            (parser-return (reverse (cons x accum))))))
  (recur '()))

(define (parser-sequence . parsers)
  (define (recur parsers accum)
    (if (null? parsers) (parser-return (reverse accum))
      (let/monad ([x (car parsers)])
        (recur (cdr parsers) (cons x accum)))))
  (recur parsers '()))

(define (parser-descend parser)
  (let/monad ([inner-input (parser-sat stream?)])
    (let ([results (parse parser inner-input)])
     (if (parse-success? results)
         (apply parser-disj* (stream->list (stream-map parser-return results)))
       (parser-fail)))))

(define (parser-cons parser1 parser2)
  (let/monad ([x parser1]
              [y parser2])
    (parser-return (cons x y))))

(define (parser-car parser)
  (let/monad ([x parser])
    (if (pair? x)
        (parser-return (car x))
      (parser-fail))))

(define (parser-cdr parser)
  (let/monad ([x parser])
    (if (pair? x)
        (parser-return (cdr x))
      (parser-fail))))

(define (parser-fst parser1 parser2)
  (let/monad ([(list x _) (parser-sequence parser1 parser2)])
    (parser-return x)))

(define (parser-snd parser1 parser2)
  (let/monad ([(list _ y) (parser-sequence parser1 parser2)])
     (parser-return y)))

(define (parser-unordered-sequence-greedy . parsers)
  (apply parser-alt*
     (map (λ (parsers) (apply parser-sequence parsers))
          (permutations (cons parser parsers)))))

(define (parser-unordered-sequence-nongreedy . parsers)
  (apply parser-disj*
     (map (λ (parsers) (apply parser-sequence parsers))
          (permutations (cons parser parsers)))))

(define (parser-eq? x)
  (parser-sat (cut eq? x <>)))

(define (parser-eqv? x)
  (parser-sat (cut eqv? x <>)))

(define (parser-equal? x)
  (parser-sat (cut equal? x <>)))
