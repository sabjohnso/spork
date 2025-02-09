#lang racket

(provide delegator)
(require
 (for-syntax spork/list-extras spork/functor racket racket/syntax syntax/parse syntax/contract))

(define-for-syntax (make-unique xs)
  (set->list (list->set xs)))

(define-for-syntax (validate-unique-method-names stx)
  (syntax-parse stx
    [(delegator ((obj (ifc ...)) ...))
     (let ([delegate-method-sets
            (for/hash ([obj/obj-ifcs (syntax-e #'((obj (ifc ...)) ...))])
              (syntax-parse obj/obj-ifcs
                [(obj (ifcs ...))
                 (values #'obj ((compose list->set join)
                                (map interface->method-names
                                     (map eval (syntax-e #'(ifcs ...))))))]))])


       (let loop ([delegates (hash-keys delegate-method-sets)])
         (when (not (null? delegates))
           (let ([current-delegate (car delegates)]
                 [remaining-delegates (cdr delegates)])
             (for ([other-delegate remaining-delegates])
               (let ([intersection
                      (set-intersect
                       (hash-ref delegate-method-sets current-delegate)
                       (hash-ref delegate-method-sets other-delegate))])
                 (when (not (set-empty? intersection))
                   (raise-syntax-error
                    'bad-delegation
                    (format "~none or more methods ~a are delegated to two or more delegates: ~a and ~a~n"
                      (set->list intersection)
                      (syntax->datum current-delegate)
                      (syntax->datum other-delegate))
                    stx
                    current-delegate)))))))
       (void))]))

(define-syntax (delegator stx)
  (syntax-parse stx
    [(delegator
      ([obj (ifc ...)] ...))
     (validate-unique-method-names stx)
     (with-syntax* ([(args) (generate-temporaries '(args))]
                    [((ifc-id ...) ...)
                     (for/list ([obj-ifcs (syntax-e #'((ifc ...) ...))])
                       (generate-temporaries (syntax-e obj-ifcs)))]
                    [(obj-ctc ...)
                     (for/list ([obj-ifc-ids (syntax-e #'((ifc ...) ...))]
                                [obj (syntax-e #'(obj ...))])
                       (with-syntax* ([(obj-ifc-id ...) obj-ifc-ids])
                         (syntax/loc stx
                           (and/c (is-a?/c obj-ifc-id) ...))))]
                    [(all-ifc-ids ...)
                     (let/monad ([obj-ifc-ids (syntax-e #'((ifc-id ...) ...))])
                       (syntax-e obj-ifc-ids))]
                    [(all-ifcs ...)
                     (let/monad ([obj-ifcs (syntax-e #'((ifc ...) ...))])
                       (syntax-e obj-ifcs))]
                    [(obj-id ...) (generate-temporaries #'(obj ...))]

                    [(method-definitions ...)
                     (let/monad ([obj-id/ifcs (syntax-e  #'((obj-id (ifc ...)) ...))])
                       (syntax-parse obj-id/ifcs
                         [(obj (ifcs ...))
                          (let ([method-names
                                 (make-unique
                                  (let/monad ([ifc (syntax-e #'(ifcs ...))])
                                    (interface->method-names (eval ifc))))])
                            (for/list ([method-name method-names])
                              (with-syntax ([method-name method-name])
                                (syntax/loc stx
                                  (define/public (method-name . args)
                                    (send/apply obj method-name args))))))]))])



       (syntax/loc stx
         (let ([all-ifc-ids all-ifcs] ...)
           (define/contract obj-id obj-ctc obj) ...
           (new
               (class* object%
                   (all-ifc-ids ...)
                 (super-new)
                 (define obj-id obj) ...
                 method-definitions ...)))))]))
