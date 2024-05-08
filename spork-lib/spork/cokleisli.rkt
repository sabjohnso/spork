#lang racket

(provide
 (contract-out
  (struct cokleisli ([proc (-> comonad? any/c)]))
  [cokleisli-run (-> cokleisli? comonad? any/c)]))

(require spork/functor spork/either spork/arrow spork/infix-notation)

(struct cokleisli
  (proc)
  #:methods gen:arrow
  ((define (arrow-comp-proc arrow) cokleisli-compose)
   (define (arrow-id-value arrow) cokleisli-id)
   (define (arrow-fanout-proc arrow) cokleisli-fanout)
   (define (arrow-split-proc arrow) cokleisli-split)
   (define (arrow-fst-proc arrow) cokleisli-fst)
   (define (arrow-snd-proc arrow) cokleisli-snd))

  #:methods gen:arrow-choice
  ((define (choose-left-proc arrow-choice) cokleisli-choose-left)
   (define (choose-right-proc arrow-choice) cokleisli-choose-right)
   (define (choose-proc arrow-choice) cokleisli-choose)
   (define (fanin-proc arrow-choice) cokleisli-fanin)))


(define (cokleisli-run ckf wx)
  ((cokleisli-proc ckf) wx))

(define (cokleisli-compose ckf ckg)
  (match-let ([(cokleisli f) ckf]
              [(cokleisli g) ckg])
    (cokleisli (comonad-compose f g))))

(define cokleisli-id (cokleisli extract))

(define (cokleisli-arr f)
  (cokleisli (compose f extract)))

(define (cokleisli-fanout ckf ckg)
  (match-let ([(cokleisli f) ckf]
              [(cokleisli g) ckg])
    (cokleisli (fanout f g))))

(define (cokleisli-split ckf ckg)
  (match-let ([(cokleisli f) ckf]
              [(cokleisli g) ckg])
    (cokleisli
     (fanout (compose f (fmap car))
             (compose g (fmap cdr))))))

(define (cokleisli-fst ckf)
  (cokleisli-split ckf cokleisli-id))

(define (cokleisli-snd ckg)
  (cokleisli-split cokleisli-id ckg))

(define (cokleisli-choose ckf ckg)
  (match-let ([(cokleisli f) ckf]
              [(cokleisli g) ckg])
    (cokleisli
     (λ (wx)
       (match (extract wx)
         [(left x) (left (f (fmap (const x) wx)))]
         [(right y) (right (g (fmap (const y) wx)))])))))

(define (cokleisli-choose-left ckf)
  (match-let ([(cokleisli f) ckf])
    (cokleisli
     (λ (wx)
       (match (extract wx)
         [(left x) (left (f (fmap (const x) wx)))]
         [(right y) (right y)])))))

(define (cokleisli-choose-right ckg)
  (match-let ([(cokleisli g) ckg])
    (cokleisli
     (λ (wx)
       (match (extract wx)
         [(left x) (left x)]
         [(right y) (right (g (fmap (const y) wx)))])))))

(define (cokleisli-fanin ckf ckg)
  (match-let ([(cokleisli f) ckf]
              [(cokleisli g) ckg])
    (cokleisli
     (λ (wx)
       (match (extract wx)
         [(left x) (f (extend (const x) wx))]
         [(right y) (g (extend (const y) wx))])))))
