#lang racket

(provide (all-from-out (submod "." arrow-details)))

(module arrow-details racket
  (provide
   id id? id-comp
   gen:arrow
   (struct-out arr)
   (contract-out
    [arrow? predicate/c]
    [fst        (-> arrow? arrow?)]
    [snd        (-> arrow? arrow?)]
    [fanout     (-> arrow? arrow? arrow?)]
    [&&&        (->* () () #:rest (listof arrow?) arrow?)]
    [split      (-> arrow? arrow? arrow?)]
    [***        (->* () () #:rest (listof arrow?) arrow?)]
    [arrow-comp (-> arrow? arrow? arrow?)]
    [<<<        (->* () () #:rest (listof arrow?) arrow?)]
    [>>>        (->* () () #:rest (listof arrow?) arrow?)]
    [choose-left (-> arrow-choice? arrow-choice?)]
    [choose-right (-> arrow-choice? arrow-choice?)]
    [choose (-> arrow-choice? arrow-choice? arrow-choice?)]
    [fanin (-> arrow-choice? arrow-choice? arrow-choice?)]
    [+++ (->* (arrow-choice? arrow-choice?) () #:rest (listof arrow-choice?) arrow-choice?)]
    [/// (->* (arrow-choice? arrow-choice?) () #:rest (listof arrow-choice?) arrow-choice?)]))

  (require racket/generic spork/curried spork/category spork/function-extras spork/either)

   (define (id-comp id1 id2) id)

   (struct arr
     (fun)
     #:property prop:procedure (struct-field-index fun)
     #:transparent)

   (define (arr-fst f)
     (match-let ([(arr f) f])
       (arr (match-lambda [(cons x y) (cons (f x) y)]))))

   (define (arr-comp f g)
     (match-let ([(arr f) f]
		 [(arr g) g])
       (arr (compose f g))))

   (define arr-id (arr identity))

   (define-generics arrow
     (arr-proc arrow)
     (split-proc arrow)
     (fst-proc arrow)
     (snd-proc arrow)
     (fanout-proc arrow)
     (arrow-comp-proc arrow)
     (arrow-id-value arrow)

     #:fast-defaults
     ([arr?
       (define (arr-proc arrow) arr)
       (define (fst-proc arrow) arr-fst)
       (define (arrow-comp-proc arrow) arr-comp)
       (define (arrow-id-value arrow) arr-id)])

     #:defaults
     ([function?
       (define (arr-proc function) function-arr)
       (define (fst-proc function) function-fst)
       (define (snd-proc function) function-snd)
       (define (split-proc function) function-split)
       (define (fanout-proc function) function-fanout)])


     #:fallbacks
     ((define (fst-proc arrow) derived-fst)
      (define (snd-proc arrow) derived-snd)
      (define (split-proc arrow) derived-split)
      (define (fanout-proc arrow) derived-fanout)
      (define (arrow-comp-proc arrow) category-compose)
      (define (arrow-id-value arrow) id)

       ))

   (define (fst f)
     ((fst-proc f) f))

   (define (snd f)
     ((snd-proc f) f))

   (define (arrow-comp f g)
     (match* (f g)
       [((arr f) (arr g)) (arr (compose f g))]
       [((arr f) g) ((arrow-comp-proc g) ((arr-proc g) f) g)]
       [(f (arr g)) ((arrow-comp-proc f) f ((arr-proc f) g))]
       [(f g) ((arrow-comp-proc f) f g)]))

   (define (<<< . fs)
     (match fs
       [(list) arr-id]
       [(list f) f]
       [(list f g) (arrow-comp f g)]
       [(list f g h hs ...) (apply <<< (arrow-comp f g) h hs)]))

   (define (>>> . fs)
     (apply <<< (reverse fs)))

   (define (fanout f g)
     (match* (f g)
       [((arr f) (arr g)) ((fanout-proc (arr f)) (arr f) (arr g))]
       [((arr f) g) ((fanout-proc g) ((arr-proc g) f) g)]
       [(f (arr g)) ((fanout-proc f) f ((arr-proc f) g))]
       [(f g) ((fanout-proc f) f g)]))

   (define (&&& . fs)
     (cond [(= (length fs) 2) (fanout (car fs) (cadr fs))]
           [(> (length fs) 2) (fanout (car fs) (apply &&& (cdr fs)))]
           [(= (length fs) 1) (car fs)]
           [(null? fs) id]))

   (define (split f g)
     (match* (f g)
       [((arr f) (arr g)) ((split-proc (arr f)) (arr f) (arr g))]
       [((arr f) g) ((split-proc g) ((arr-proc g) f) g)]
       [(f (arr g)) ((split-proc f) f ((arr-proc f) g))]
       [(f g) ((split-proc f) f g)]))

   (define (*** . fs)
     (cond [(= (length fs) 2) (split (car fs) (cadr fs))]
           [(> (length fs) 2) (split (car fs) (apply *** (cdr fs)))]
           [(= (length fs) 1) (car fs)]
           [(null? fs) id]))

   (define (swap xy)
     (match-let ([(cons x y) xy])
       (cons y x)))

   (define (derived-fst f)
     ((split-proc f) f (arrow-id-value f)))

   (define (derived-snd f)
     (>>> (arr swap) (fst f) (arr swap)))

   (define (derived-split f g)
     (>>> (fst f) (snd g)))

   (define (dup x)
     (cons x x))

   (define (derived-fanout f g)
     (>>> (arr dup) (split f g)))


   (define-generics arrow-choice
     (choose-left-proc arrow-choice)
     (choose-right-proc arrow-choice)
     (choose-proc arrow-choice)
     (fanin-proc arrow-choice)

     #:defaults
     ([function?
       (define (choose-left-proc function) function-choose-left)
       (define (choose-right-proc function) function-choose-right)
       (define (choose-proc function) function-choose)
       (define (fanin-proc function) function-fanin)])

     #:fallbacks
     ((define (choose-left-proc arrow-choice) derived-choose-left)
      (define (choose-right-proc arrow-choice) derived-choose-right)
      (define (choose-proc arrow-choice) derived-choose)
      (define (fanin-proc arrow-choice) derived-fanin)))

   (define (choose-left f)
     ((choose-left-proc f) f))

   (define (choose-right f)
     ((choose-right-proc f) f))

   (define-curried (choose f g)
     (match* (f g)
       [((arr f) (arr g)) (arr (choose f g))]
       [(f (arr g)) ((choose-proc f) f ((arr-proc f) g))]
       [((arr f) g) ((choose-proc g) ((arr-proc g) f) g)]
       [(f g) ((choose-proc f) f g)]))

   (define-curried (fanin f g)
     (match* (f g)
       [((arr f) (arr g)) (arr (fanin f g))]
       [(f (arr g)) ((fanin-proc f) f ((arr-proc f) g))]
       [((arr f) g) ((fanin-proc g) ((arr-proc g) f) g)]
       [(f g) ((fanin-proc f) f g)]))

   (define (+++ f g . hs)
     (if (null? hs) (choose f g)
       (choose f (apply +++ g hs))))

   (define (/// f g . hs)
     (if (null? hs) (fanin f g)
       (fanin f (apply /// g hs))))

   (define (derived-choose-left f)
     (choose f (arr identity)))

   (define (derived-choose-right f)
     (choose (arr identity) f))

   (define (derived-choose f g)
     ((choose-left f) `>>> (arr either-swap) `>>> (choose-left g) `>>> (arr either-swap)))

   (define (derived-fanin f g)
     ((choose f g) `>>>
      (arr (match-lambda
             [(left x) x]
             [(right y) y])))))

(require (submod "." arrow-details))
