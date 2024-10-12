#lang racket

(provide
 (contract-out
  (struct tape ([feed list?] [takeup list?]))
  [empty-tape tape?]
  [tape-empty? (-> tape? boolean?)]
  [tape-at-front? (-> tape? boolean?)]
  [tape-at-back? (-> tape? boolean?)]
  [make-tape (->* () () #:rest list? tape?)]
  [list->tape (-> list? tape?)]
  [vector->tape (-> vector? tape?)]
  [tape-position (-> tape? natural-number/c)]
  [tape-remaining (-> tape? natural-number/c)]
  [tape-length (-> tape? natural-number/c)]
  [tape-fwd (-> tape? tape?)]
  [tape-bwd (-> tape? tape?)]
  [tape-fwd-by (-> tape? exact-integer? tape?)]
  [tape-bwd-by (-> tape? exact-integer? tape?)]
  [tape-move-by (-> tape? exact-integer? tape?)]
  [tape-move-to (-> tape? exact-integer? tape?)]
  [tape-fast-fwd (-> tape? tape?)]
  [tape-rewind (-> tape? tape?)]
  [tape-splice (->* () () #:rest (listof tape?) tape?)]
  [tape-append (->* () () #:rest (listof tape?) tape?)]
  [tape-read (-> (and/c tape? (not/c tape-at-back?)) any/c)]
  [tape-refabs (-> tape? natural-number/c any/c)]
  [tape-refrel (-> tape? exact-integer? any/c)]
  [tape-write (-> (and/c tape? (not/c tape-at-back?)) any/c tape?)]
  [tape-insert (-> tape? any/c tape?)]
  [tape-join (-> tape? tape?)]
  [tape->list (-> tape? list?)]
  [tape-reverse (-> tape? tape?)]))

(require spork/list-extras spork/functor)

(struct tape
  (feed takeup)

  #:methods gen:functor
  ((define (fmap-proc tape) tape-fmap))

  #:methods gen:monad
  ((define (return-proc tape) tape-return)
   (define (join-proc tape) tape-join))

  #:transparent)

(define empty-tape
  (tape '() '()))

(define (tape->list xs)
  (match-let ([(tape feed takeup) xs])
    (rappend takeup feed)))

(define (tape-position xs)
  (length (tape-takeup xs)))

(define (tape-remaining xs)
  (length (tape-feed xs)))

(define (tape-length xs)
  (+ (tape-position xs)
     (tape-remaining xs)))

(define (tape-empty? xs)
  (and (null? (tape-feed xs))
       (null? (tape-takeup xs))))

(define (tape-at-front? xs)
  (null? (tape-takeup xs)))

(define (tape-at-back? xs)
  (null? (tape-feed xs)))

(define (tape-fmap f xs)
  (tape (list-fmap f (tape-feed xs))
        (list-fmap f (tape-takeup xs))))

(define (tape-return x) (tape (list x) '()))

(define (tape-join xss)
  (tape (tape->list (apply tape-append (tape-feed xss)))
        (tape->list (apply tape-append (fmap tape-reverse (tape-takeup xss))))))

(define (tape-reverse xs)
  (tape (tape-takeup xs)
        (tape-feed xs)))

(define (list->tape xs)
  (tape xs '()))

(define (vector->tape xs)
  (list->tape (vector->list xs)))

(define (make-tape . xs)
  (list->tape xs))

(define (tape-splice/2 xs ys)
  (match-let ([(tape xfeed xtakeup) xs]
              [(tape yfeed ytakeup) ys])
    (tape (append xfeed yfeed)
          (append xtakeup ytakeup))))

(define (tape-append/2 xs ys)
  (match-let ([(tape feed takeup) xs])
    (tape (append feed (tape->list ys)) takeup)))

(define (tape-append . xs)
  (let ([n (length xs)])
    (cond [(= n 2) (tape-append/2 (car xs) (cadr xs))]
          [(> n 2) (tape-append (car xs) (apply tape-append (cdr xs)))]
          [(= n 1) (car xs)]
          [(null? xs) empty-tape])))

(define (tape-splice . xs)
  (cond [(= (length xs) 2) (tape-splice/2 (car xs) (cadr xs))]
        [(> (length xs) 2) (tape-splice/2 (car xs) (apply tape-splice (cdr xs)))]
        [(= (length xs) 1) (car xs)]
        [(null? xs) empty-tape]))

(define/match (tape-fwd xs)
  [((tape (list x xs ...) ys)) (tape xs (cons x ys))]
  [((tape '() ys)) (tape '() ys)])

(define/match (tape-bwd xs)
  [((tape xs (list y ys ...))) (tape (cons y xs) ys)]
  [((tape xs '())) (tape xs '())])

(define  (tape-fwd-by xs n)
  (if (> n 0)
      (tape-fwd-by (tape-fwd xs) (sub1 n))
    xs))

(define (tape-bwd-by xs n)
  (if (> n 0)
      (tape-bwd-by (tape-bwd xs) (sub1 n))
    xs))

(define (tape-move-by xs n)
  (if (> n 0)
      (tape-fwd-by xs n)
    (tape-bwd-by xs (abs n))))

(define (tape-move-to xs n)
  (let ([pos (tape-position xs)])
    (if (< pos n) (tape-fwd-by xs (- n pos))
      (tape-bwd-by xs (- pos n)))))

(define (tape-fast-fwd xs)
  (match-let ([(tape xs ys) xs])
    (tape '() (rappend xs ys))))

(define (tape-rewind xs)
  (match-let ([(tape xs ys) xs])
    (tape (rappend ys xs) '())))

(define (tape-insert xs x)
  (match-let ([(tape xs ys) xs])
    (tape (cons x xs) ys)))

(define (tape-cut xs)
  (match-let ([(tape (list _ xs ...) ys) xs])
    (tape xs ys)))

(define (tape-write xs x)
  (match xs
    [(tape (list _ xs ...) takeup) (tape (cons x xs) takeup)]
    [(tape '() _) (error "Cannot write to the back of a tape")]))

(define/match (tape-read xs)
  [((tape (list x _ ...) _)) x]
  [((tape '() _)) (error "Cannot read from the back of a tape")])

(define (tape-refabs xs n)
  (tape-read (tape-move-to xs n)))

(define (tape-refrel xs n)
  (tape-read (tape-move-by xs n)))
