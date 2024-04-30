#lang racket

(provide
 gen:trivial
 gen:comonad
 gen:monad
 gen:applicative
 gen:functor

 begin/monad
 let/monad
 let/applicative
 let/functor

 (contract-out
  [unresolved? predicate/c]

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
  [flatmap (-> (-> any/c monad?) monad? monad?)]
  [join (-> monad? monad?)]
  [monad-compose (->* () () #:rest (listof (-> any/c monad?)) (-> any/c monad?))]
  [>=> (->* () () #:rest (listof (-> any/c monad?)) (-> any/c monad?))]
  [<=< (->* () () #:rest (listof (-> any/c monad?)) (-> any/c monad?))]

  [applicative? predicate/c]
  [pure (-> any/c applicative?)]
  [fapply (-> applicative? applicative? applicative?)]

  [functor? predicate/c]
  [fmap (-> (-> any/c any/c) functor? functor?)]))

(require
 (for-syntax racket racket/syntax syntax/parse)
 racket/generic
 spork/list-extras
 spork/pair-extras)

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

  #:fast-defaults
  ([unresolved?
    (define (wrap-proc unresolved) unresolved)
    (define (unwrap-proc unresolved) unresolved-value)]))

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

  #:fast-defaults
  ([pair?
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

  #:fast-defaults
  ([list?
    (define (return-proc list) list-return)
    (define (flatmap-proc list) list-flatmap)]
   [vector?]
   [stream?]
   [sequence?])

  #:defaults
  ([trivial?
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
(define (flatmap f mx)
  (if (unresolved? mx) (f (unwrap mx))
    (let ([flatmap (flatmap-proc mx)]
          [return (return-proc mx)])
      (flatmap (λ (x)
                 (let ([my (f x)])
                   (if (unresolved? my) (return (unwrap my))
                     my)))
               mx))))

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
     (with-syntax ([ignored (generate-temporaries '("ignored"))])
       (syntax/loc stx
         (flatmap (λ (ignored) (begin/monad es ...)) e)))]))

(define-syntax (let/monad stx)
  (syntax-parse stx
    [(_ ([x:id mx:expr]) (begin/monad e es ...))
     (syntax/loc stx
       (flatmap (λ (x) (begin/monad e es ...)) mx))]

    [(_ ([x:id mx:expr] [y:id my:expr] ...+) e es ...)
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

  #:defaults
  ([monad?
    (define (pure-proc monad) (return-proc monad))
    (define (fapply-proc monad) monad-fapply)]))

(define (pure x) (unresolved x))

(define (fapply mf mx)
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


(define-syntax (let/applicative stx)
  (syntax-parse stx
    [(_ ([x:id mx:expr]) es ...+)
     (syntax/loc stx
       (fmap (λ (x) es ...) mx))]
    [(_ ([x:id mx:expr]
         [ys:id mys:expr] ...+)
        es ...)
     (syntax/loc stx
       (fapply* (fmap (curry (λ (x ys ...) es ...)) mx) mys ...))]))


;; Functor
;; =======
;;
(define-generics functor
  (fmap-proc functor)

  #:defaults
  ([applicative?
    (define (fmap-proc applicative) applicative-fmap)]
   [comonad?
    (define (fmap-proc comonad) comonad-fmap)]))

(define (fmap f mx)
  (let ([fmap (fmap-proc mx)])
    (fmap f mx)))

(define-syntax (let/functor stx)
  (syntax-parse stx
    [(_ ([x:id mx:expr]) es ...+)
     (syntax/loc stx
       (fmap (λ (x) es ...) mx))]

    [(_ ([x:id mx:expr] [ys:id mys:expr] ...+) es ...+)
     (syntax/loc stx
       (let/functor ([x mx])
         (let/functor ([ys mys] ...)
           es ...)))]))
