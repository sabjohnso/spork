#lang racket

(provide
 gen:trivial
 gen:comonad
 gen:monad
 gen:applicative
 gen:functor
 gen:monad-zero
 gen:monad-plus

 begin/monad
 let/monad
 let/applicative
 let/functor

 (contract-out
  [unresolved? predicate/c]
  [unresolved-value (-> unresolved? any/c)]

  [trivial? predicate/c]
  [unwrap (-> trivial? any/c)]
  [wrap (-> any/c trivial?)]

  [comonad? predicate/c]
  [extract (-> comonad? any/c)]
  [duplicate (-> comonad? comonad?)]
  [extend (-> (-> comonad? any/c) comonad? comonad?)]
  [comonad-compose (->* () () #:rest (listof (-> comonad? any/c)) (-> comonad? any/c))]
  [=<= (->* () () #:rest (listof (-> comonad? any/c)) (-> comonad? any/c))]
  [=>= (->* () () #:rest (listof (-> comonad? any/c)) (-> comonad? any/c))]

  [monad? predicate/c]
  [return (-> any/c monad?)]
  [flatmap (curried-> (-> any/c monad?) monad? monad?)]
  [join (-> monad? monad?)]
  [monad-compose (->* () () #:rest (listof (-> any/c monad?)) (-> any/c monad?))]
  [>>= (->* (monad?) () #:rest (listof (-> any/c monad?)) monad?)]
  [>=> (->* () () #:rest (listof (-> any/c monad?)) (-> any/c monad?))]
  [<=< (->* () () #:rest (listof (-> any/c monad?)) (-> any/c monad?))]

  [applicative? predicate/c]
  [pure (-> any/c applicative?)]
  [fapply (curried-> applicative? applicative? applicative?)]
  [fapply* (->* (applicative? applicative?) () #:rest (listof applicative?) applicative?)]
  [<*> (->* (applicative? applicative?) () #:rest (listof applicative?) applicative?)]

  [functor? predicate/c]
  [fmap (curried-> (-> any/c any/c) functor? functor?)]
  [<$> (-> (-> any/c any/c) functor? functor?)]

  [monad-zero? predicate/c]
  [mzero? predicate/c]
  [mzero mzero?]

  [monad-plus? predicate/c]
  [mplus (-> monad-plus? monad-plus? monad-plus?)]
  (<+> (->* () () #:rest (listof monad-plus?) monad-plus?))))

(require
 (for-syntax racket racket/syntax syntax/parse)
 racket/generic
 spork/list-extras
 spork/function-extras
 spork/vector-extras
 spork/stream-extras
 spork/sequence-extras
 spork/thunk-extras
 spork/future-extras
 spork/pair-extras
 spork/either
 spork/curried
 spork/tag)

;; The struct unresolved is a trivial, temporary context
;; used to hold a value injected into some context
;; until that context is resolved.
(struct unresolved (value) #:transparent)

;; Trivial
;; =======
;;
(define-generics trivial
  (wrap-proc trivial)
  (unwrap-proc trivial)

  #:requires
  (wrap-proc unwrap-proc)

  #:fast-defaults
  ([unresolved?
    (define (wrap-proc unresolved) unresolved)
    (define (unwrap-proc unresolved) unresolved-value)])
  #:defaults
  ([thunk?
    (define (wrap-proc thunk) thunk-wrap)
    (define (unwrap-proc thunk) thunk-unwrap)]
   [future?
    (define (wrap-proc future) future-wrap)
    (define (unwrap-proc future) future-unwrap)]))

;; wrap
;; ----
;; wrap returns a value in an unresolved context as
;; there is not sufficient information to resolve
;; the intended context in the application of wrap.
;;
(define (wrap x) (unresolved x))

;; unwrap
;; ------
;;
(define (unwrap tx)
  ((unwrap-proc tx) tx))


;; trivial-flatmap
;; ---------------
;;
(define (trivial-flatmap f mx)
  (f (unwrap mx)))

;; trivial-fmap
;; ------------
;;
(define (trivial-fmap f mx)
  ((wrap-proc mx) (f (unwrap mx))))

;; trivial-extend
(define (trivial-extend f wx)
  ((wrap-proc wx) (f wx)))

;; Comonad
;; =======
;;
(define-generics comonad
  (extract-proc comonad)
  (duplicate-proc comonad)
  (extend-proc comonad)

  #:requires
  (extract)

  #:fast-defaults
  ([nonempty-list?
    (define (extract-proc list) car)
    (define (duplicate-proc list) nonempty-list-duplicate)
    (define (extend-proc list) nonempty-list-extend)]
   [nonempty-stream?
    (define (extract-proc stream) stream-first)
    (define (duplicate-proc stream) nonempty-stream-duplicate)
    (define (extend-proc stream) nonempty-stream-extend)]
   [pair?
    (define (extract-proc pair) car)
    (define (duplicate-proc pair) pair-duplicate)
    (define (extend-proc pair) pair-extend)])

  #:defaults
  ([trivial?
    (define (extract-proc trivial) unwrap)
    (define (duplicate-proc trivial) (wrap-proc trivial))
    (define (extend-proc trivial) trivial-extend)])

  #:fallbacks
  ((define (duplicate-proc comonad) derived-duplicate)
   (define (extend-proc comonad) derived-extend)))

(define (extract wx)
  ((extract-proc wx) wx))

(define (duplicate wx)
  ((duplicate-proc wx) wx))

(define (extend f wx)
  ((extend-proc wx) f wx))

(define (comonad-compose-2 f g)
  (compose f (curry extend g)))

(define (comonad-compose . fs)
  (cond [(= (length fs) 2) (comonad-compose-2 (car fs) (cadr fs))]
        [(> (length fs) 2)
         (apply comonad-compose
                (comonad-compose-2 (car fs) (cadr fs))
                (cddr fs))]
        [(= (length fs) 1) (car fs)]
        [(zero? (length fs)) extract]))
(define (=<= . fs)
  (apply comonad-compose fs))

(define (=>= . fs)
  (apply comonad-compose (reverse fs)))

(define (derived-duplicate wx)
  (extend identity wx))

(define (derived-extend f wx)
  (fmap f (duplicate wx)))

(define (comonad-fmap f wx)
  (extend (compose f extract) wx))

;; Monad
;; =====
;;
(define-generics monad
  (return-proc monad)
  (flatmap-proc monad)
  (join-proc monad)

  #:requires
  (return-proc)

  #:fast-defaults
  ([either?
    (define (return-proc either) left)
    (define (flatmap-proc either) either-flatmap)]
   [list?
    (define (return-proc list) list-return)
    (define (flatmap-proc list) list-flatmap)
    (define (join-proc list) list-join)]

   [vector?
    (define (return-proc vector) vector-return)
    (define (flatmap-proc vector) vector-flatmap)
    (define (join-proc vector) vector-join)]

   [stream?
    (define (return-proc stream) stream-return)
    (define (flatmap-proc stream) stream-flatmap)
    (define (join-proc stream) stream-join)])

  #:defaults
  ([function?
    (define (return-proc function) function-return)
    (define (flatmap-proc function) function-flatmap)
    (define (join-proc function) function-join)]

   [trivial?
    (define (return-proc trivial) (wrap-proc trivial))
    (define (flatmap-proc trivial) trivial-flatmap)
    (define (join-proc trivial) (unwrap-proc trivial))])

  #:fallbacks
  ((define (join-proc monad) derived-join)
   (define (flatmap-proc monad)  derived-flatmap)))

;; return
;; ------
;;
;; The intended context cannot be resolved with a call to return, so the
;; input is returned in the unresolved context. This is the same behavior
;; as `wrap`.
;;
(define (return x) (unresolved x))

;; flatmap
;; -------
;;
;; Map a context constructor values in that context, joining the results
;;
;; If the outer context is no resolved, simply apply the constructor
;; to the value from the unresolve context.
;;
;; Otherwise, with the resolved flatmap, map a transformed function that
;; add context resolution to the input function.
;;
(define-curried (flatmap f mx)
  (if (unresolved? mx) (f (unwrap mx))
    (let ([flatmap (flatmap-proc mx)]
          [return (return-proc mx)])
      (flatmap (λ (x)
                 (let ([my (f x)])
                   (if (unresolved? my) (return (unwrap my))
                     my)))
               mx))))

(define (>>= mx . fs)
  (cond [(= (length fs) 1) (flatmap (car fs) mx)]
        [(null? fs) mx]
        [#t (apply >>= (flatmap (car fs) mx) (cdr fs))]))

;; join
;; ----
;;
;; Strip one context layer.
;;
;; If the outer context is not resolved. Just unwrap it, whichn
;; is the correct treatment whether or not the inner context is
;; resolved.
;;
;; Otherwise, the outer context is resolved, but the inner context
;; may or may not be. An idempotent transform is applied to the inner
;; context to ensure that it is resolved in the call to the
;; join method for the outer context.
;;
(define (join mmx)
  (if (unresolved? mmx)
      (unwrap mmx)
    (let ([join (join-proc mmx)]
          [mmx (fmap (λ (mx)
                       (if (unresolved? mx) ((return-proc mmx) (unwrap mx))
                         mx)) mmx)])
      (join mmx))))

;; derived-join
;; ------------
(define (derived-join mmx)
  (flatmap identity mmx))

;; derived-flatmap
;; ---------------
(define (derived-flatmap f mx)
  (join (fmap f mx)))

(define (monad-compose-2 f g)
  (compose (curry flatmap f) g))

(define (monad-compose . fs)
  (cond [(= (length fs) 2) (monad-compose-2 (car fs) (cadr fs))]
        [(> (length fs) 2)
         (apply monad-compose (monad-compose-2 (car fs) (cadr fs)) (cddr fs))]
        [(= (length fs) 1) (car fs)]
        [(null? fs) return]))

(define (<=< . fs)
  (apply monad-compose fs))

(define (>=> . fs)
  (apply monad-compose (reverse fs)))

;; monad-fapply
;; ------------
(define (monad-fapply mf mx)
  (flatmap (λ (f) (flatmap (λ (x) (return (f x))) mx)) mf))

(define-syntax (begin/monad stx)
  (syntax-parse stx
    [(_ e:expr) (syntax/loc stx e)]
    [(_ e:expr es:expr ...+)
     (with-syntax ([ignored (generate-temporary 'ignored)])
       (syntax/loc stx
         (flatmap (λ (ignored) (begin/monad es ...)) e)))]))

(define-syntax (let/monad stx)
  (syntax-parse stx
    [(_ ([x:id mx:expr]) e es ...)
     (syntax/loc stx
       (flatmap (λ (x) (begin/monad e es ...)) mx))]

    [(_ ([target:expr source:expr]) body:expr ...+)
     (syntax/loc stx
       (flatmap (match-lambda [target (begin/monad body ...)]) source))]

    [(_ ([x:expr mx:expr] [y:expr my:expr] ...+) e es ...)
     (syntax/loc stx
       (let/monad ([x mx])
         (let/monad ([y my] ...)
           e es ...)))]))

;; Applicative
;; ===========
;;
(define-generics applicative
  (pure-proc applicative)
  (fapply-proc applicative)

  #:requires
  (pure-proc fapply-proc)

  #:fast-defaults
  ([list?
    (define (pure-proc list) list-return)
    (define (fapply-proc list) list-fapply)]
   [vector?
    (define (pure-proc vector) vector-return)
    (define (fapply-proc vector) vector-fapply)]
   [stream?
    (define (pure-proc stream) stream-return)
    (define (fapply-proc stream) stream-fapply)])

  #:defaults
  ([monad?
    (define (pure-proc monad) (return-proc monad))
    (define (fapply-proc monad) monad-fapply)]))

(define (pure x) (unresolved x))

(define-curried (fapply mf mx)
  (match* (mf mx)
    [((unresolved f) (unresolved x)) (unresolved (f x))]
    [((unresolved f) mx)
     (let ([fapply (fapply-proc mx)]
           [pure (pure-proc mx)])
       (fapply (pure f) mx))]
    [(mf (unresolved x))
     (let ([fapply (fapply-proc mf)]
           [pure (pure-proc mf)])
       (fapply mf (pure x)))]
    [(mf mx)
     (let ([fapply (fapply-proc mf)])
       (fapply mf mx))]))

(define (applicative-fmap f mx)
  (fapply (pure f) mx))

(define (fapply* mf mx . mys)
  (if (null? mys) (fapply mf mx)
    (apply fapply* (fapply mf mx) mys)))

(define (<*> mf mx . mys)
  (apply fapply* mf mx mys))


(define-syntax (curried-match-lambda stx)
  (syntax-parse stx
    [(_ (pattern:expr) es:expr ...+)
     (syntax/loc stx
       (match-lambda [pattern es ...]))]
    [(_ (pattern:expr patterns:expr ...+) es:expr ...+)
     (syntax/loc stx
       (match-lambda [pattern (curried-match-lambda (patterns ...) es ...)]))]))

(define-syntax (let/applicative stx)
  (syntax-parse stx
    [(_ ([x:id mx:expr]) es ...+)
     (syntax/loc stx
       (fmap (λ (x) es ...) mx))]

    [(_ ([target:expr source:expr]) es:expr ...+)
     (syntax/loc stx
       (fmap (match-lambda [target es ...]) source))]

    [(_ ([x:id mx:expr]
         [ys:id mys:expr] ...+)
        es:expr ...+)
     (syntax/loc stx
       (fapply* (fmap (curry (λ (x ys ...) es ...)) mx) mys ...))]

    [(_ ([target:expr source:expr]
         [targets:expr sources:expr] ...+)
        es:expr ...+)
     (syntax/loc stx
       (fapply* (fmap (curried-match-lambda (target targets ...) es ...) source) sources ...))]))


;; Functor
;; =======
;;
(define-generics functor
  (fmap-proc functor)

  #:requires
  (fmap-proc)

  #:fast-defaults
  ([list?
    (define (fmap-proc list) list-fmap)]
   [vector?
    (define (fmap-proc vector) vector-fmap)]
   [stream?
    (define (fmap-proc stream) stream-fmap)])

  #:defaults
  ([applicative?
    (define (fmap-proc applicative) applicative-fmap)]
   [comonad?
    (define (fmap-proc comonad) comonad-fmap)]))

(define-curried (fmap f mx)
  (let ([fmap (fmap-proc mx)])
    (fmap f mx)))

(define (<$> f mx)
  (fmap f mx))

(define-syntax (let/functor stx)
  (syntax-parse stx
    [(_ ([x:id mx:expr]) es ...+)
     (syntax/loc stx
       (fmap (λ (x) es ...) mx))]

    [(_ ([target:expr source:expr]) es:expr ...+)
     (syntax/loc stx
       (fmap (match-lambda [target es ...]) source))]

    [(_ ([x:expr mx:expr] [ys:expr mys:expr] ...+) es ...+)
     (syntax/loc stx
       (let/functor ([x mx])
         (let/functor ([ys mys] ...)
           es ...)))]))

(define-tag mzero)
(define (mzero-mplus x y)
  (if (mzero? x) y x))

;;
;; Monad Zero
;; ==========
;;
(define-generics monad-zero
  (mzero-value monad-zero)
  #:fast-defaults
  ([list?
    (define (mzero-value list) '())]
   [vector?
    (define (mzero-value vector) #())]
   [stream?
    (define (mzero-value stream) empty-stream)]))

;;
;; Monad Plus
;; ==========
;;
(define-generics monad-plus
  (mplus-proc monad-plus)
  #:fast-defaults
  ([mzero? (define (mplus-proc -mzero) mzero-mplus)]
   [list? (define (mplus-proc list) append)]
   [vector? (define (mplus-proc vector) vector-append)]
   [stream? (define (mplus-proc stream) stream-append)]))

(define (mplus mx my)
  (match* (mx my)
    [((? mzero?) my) my]
    [(mx (? mzero?)) mx]
    [(mx my) ((mplus-proc mx) mx my)]))

(define (<+> . mxs)
  (cond [(= (length mxs) 2) (mplus (car mxs) (cadr mxs))]
        [(> (length mxs) 2) (apply <+> (mplus (car mxs) (cadr mxs)) (cddr mxs))]
        [(= (length mxs) 1) (car mxs)]
        [(null? mxs) mzero]))
