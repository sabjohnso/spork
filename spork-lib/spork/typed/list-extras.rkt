#lang typed/racket

(provide
 rappend
 product
 return
 join
 fmap
 fapply
 lift2
 flatmap
 bind
 zip zip-with
 let/f let/a let/m)

(: return (∀ (A) (-> A (Listof A))))
(define (return x)
  (list x))

(: rappend (∀ (A) (-> (Listof A) (Listof A) (Listof A))))
(define (rappend xs ys)
  (match xs
    [(list x xs ...) (rappend xs (cons x ys))]
    [(list) ys]))

(: fmap (∀ (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (fmap f xs)
  (map f xs))

(: fapply (∀ (A B)  (-> (Listof (-> A B)) (Listof A) (Listof B))))
(define (fapply fs xs)
  (let loop ([fs fs]
             [accum : (Listof B) '()])
    (match fs
      [(list f fs ...) (loop fs (rappend (fmap f xs) accum))]
      [(list) (reverse accum)])))

(: product (∀ (A B) (-> (Listof A) (Listof B) (Listof (List A B)))))
(define (product xs ys)
  (let loop ([xs xs]
             [accum : (Listof (List A B)) '()])
    (match xs
      [(list x xs ...) (loop xs (rappend (fmap (λ ([y : B]) (list x y)) ys) accum))]
      [(list) (reverse accum)])))

(: lift2 (∀ (A B C) (-> (-> A B C) (Listof A) (Listof B) (Listof C))))
(define (lift2 f xs ys)
  (fmap (λ ([xy : (List A B)]) (apply f xy))
        (product xs ys)))

(: flatmap (∀ (A B) (-> (-> A (Listof B)) (Listof A) (Listof B))))
(define (flatmap f xs)
  (let loop ([xs xs]
             [accum : (Listof B) '()])
    (match xs
      [(list x xs ...) (loop xs (rappend (f x) accum))]
      [(list) accum])))

(: bind (∀ (A B) (-> (Listof A) (-> A (Listof B)) (Listof B))))
(define (bind xs f)
  (flatmap f xs))


(: join (∀ (A) (-> (Listof (Listof A)) (Listof A))))
(define (join xss)
  (let ([xss xss]
        [accum : (Listof A) '()])
    (match xss
      [(list xs xss ...) (rappend xs accum)]
      [(list) (reverse accum)])))


(: zip (∀ (A B) (-> (Listof A) (Listof B) (Listof (List A B)))))
(define (zip xs ys)
  (let loop ([xs xs]
             [ys ys]
             [accum : (Listof (List A B)) '()])
    (match* (xs ys)
      [((list x xs ...) (list y ys ...))
       (loop xs ys (cons (list x y) accum))]
      [(_ _) (reverse accum)])))

(: zip-with (∀ (A B C) (-> (-> A B C) (Listof A) (Listof B) (Listof C))))
(define (zip-with f xs ys)
  (let loop ([xs xs]
             [ys ys]
             [accum : (Listof C) '()])
   (match* (xs ys)
     [((list x xs ...) (list y ys ...))
      (loop xs ys (cons (f x y) accum))]
     [(_ _) (reverse accum)])))

(define-syntax let/f
  (syntax-rules (:)
    [(_ ([x : t e]) body more-body ...)
     (fmap (λ ([x : t]) body more-body ...) e)]
    [(_ (binding more-bindings ...)
        body more-body ...)
     (let/f (binding)
       (let/f (more-bindings ...)
         body more-body ...))]))

(define-syntax curried-function
  (syntax-rules (:)
    [(_ ([x : t _] bindings ...) body)
     (λ ([x : t]) (curried-function (bindings ...) body))]
    [(_ () body) body]))

(define-syntax applicative-apply
  (syntax-rules (:)
    [(_ fs ([_ : _ e] bindings ...))
     (applicative-apply (fapply fs e) (bindings ...))]
    [(_ () xs) xs]))

(define-syntax let/a
  (syntax-rules (:)
    [(_ ([x : t e]) body)
     (let/f ([x : t e])
       body)]
    [(_ ([x : t e] bindngs ...) body)
     (applicative-apply (fmap (curried-function bindings body) e) bindings)]))

(define-syntax let/m
  (syntax-rules (:)
    [(_ ([x : t e]) body)
     (flatmap (λ ([x : t]) body) e)]
    [(_ (binding bindings ...) body)
     (let/m (binding)
       (let/m (bindings ...)
         body))]))
