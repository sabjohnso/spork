#lang racket

(provide
 (contract-out
  (struct kleisli ([ctor (-> any/c monad?)]))
  [kleisli-run (-> kleisli? any/c monad?)]))

(require spork/functor spork/category spork/arrow spork/infix-notation spork/either)

(struct kleisli
  (ctor)
  #:methods gen:category
  ((define (compose-proc category) kleisli-compose)
   (define (id-value category) kleisli-id))
  #:methods gen:arrow
  ((define (arr-proc arrow) kleisli-arr)
   (define (split-proc arrow) kleisli-split)
   (define (arrow-comp-proc arrow) kleisli-compose)
   (define (arrow-id-value arrow) kleisli-id))
  #:methods gen:arrow-choice
  ((define (choose-left-proc arrow-choice) kleisli-choose-left)
   (define (choose-right-proc arrow-choice) kleisli-choose-right)
   (define (choose-proc arrow-choice) kleisli-choose)
   (define (fanin-proc arrow-choice) kleisli-fanin))
  #:transparent)

(define (kleisli-run kf x)
  ((kleisli-ctor kf) x))

(define (kleisli-compose kf kg)
  (kleisli ((kleisli-ctor kf) `<=< (kleisli-ctor kg))))

(define kleisli-id
  (kleisli return))

(define (kleisli-arr f)
  (kleisli (compose return f)))

(define (kleisli-fst f)
  (kleisli
   (λ (xs)
     (let/applicative ([y1 (kleisli-run f (car xs))]
                       [y2 (return (cdr xs))])
       (cons y1 y2)))))

(define (kleisli-snd f)
  (kleisli
   (λ (xs)
     (let/applicative ([y1 (return (car xs))]
                       [y2 (kleisli-run f (cdr xs))])
       (cons y1 y2)))))

(define (kleisli-split f g)
  (kleisli
   (λ (xs)
     (let/applicative ([y1 (kleisli-run f (car xs))]
                       [y2 (kleisli-run g (cdr xs))])
       (cons y1 y2)))))

(define (kleisli-fanout f g)
  (kleisli
   (λ (x)
     (let/applicative ([y1 (kleisli-run f x)]
                       [y2 (kleisli-run g x)])
       (cons y1 y2)))))



(define (kleisli-choose-left f)
  (kleisli
   (λ (mx)
     (match mx
       [(left x) (fmap left (kleisli-run f x))]
       [(right y) (fmap right (return y))]))))

(define (kleisli-choose-right g)
  (kleisli
   (λ (mx)
     (match mx
       [(left x) (return (left x))]
       [(right y) (fmap right (kleisli-run g y))]))))


(define (kleisli-choose f g)
  (kleisli
   (λ (mx)
     (match mx
       [(left x) (fmap left (kleisli-run f x))]
       [(right y) (fmap left (kleisli-run g y))]))))

(define (kleisli-fanin f g)
  (kleisli
   (λ (mx)
     (match mx
       [(left x) (kleisli-run f x)]
       [(right x) (kleisli-run g x)]))))
