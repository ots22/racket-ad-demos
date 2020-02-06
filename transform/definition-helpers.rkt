#lang racket

;(provide dfn-build)
(provide (for-syntax dfn-build)
         (for-syntax adjoint-trace+terms))

(require (for-syntax racket)
         (for-syntax syntax/stx)
         syntax/id-table (for-syntax syntax/id-table)
         (for-syntax racket/syntax)
         "../trace/util.rkt" (for-syntax "../trace/util.rkt")
         (rename-in "../cons-arithmetic/cons-arithmetic.rkt"
                    [cons-add +]
                    [cons-zero zero]))

(module+ test (require rackunit))

;; idea: exactly like trace, except instead of manipulating traces of
;; assignments, we directly manipulate syntax.

;; likely to need:
;; dfn-fresh
; dfn-get
; dfn-append
; dfn-add
; dfn-remove-duplicates
; dfn-filter-out (?)
; dfn-prune (?)
; dfn-last


(define-for-syntax (dfn-id dfn-form)
  (syntax-case dfn-form ()
    [(_ id rest ...) #'id]))

(define-for-syntax (dfn-expr dfn-form)
  (syntax-case dfn-form ()
    [(_ id expr) #'expr]))

(define-for-syntax (last-id dfn-forms)
  (dfn-id (last dfn-forms)))

(define-for-syntax (dfn-build op . dfn-lists)
  (with-syntax ([(arg-ids ...) (map last-id dfn-lists)]
                [fresh-id (generate-temporary)]
                [op op])
    (remove-duplicates
     (append (apply append dfn-lists)
             (list #'(define fresh-id (op arg-ids ...)))))))

(define-for-syntax (dfn-get id dfn-forms)
  (member id dfn-forms (λ (u v) (free-identifier=? u (dfn-id v)))))

(module+ test
  (define-syntax (s stx)
    (define a (dfn-build #'+
                         (list #'(define b '(1 0)))
                         (list #'(define c '(0 2)))))
    (define g (last-id a))
    #`(let () #,@a #,g))

  (check-equal? s '(1 2)))


(module+ test
  (define table (make-immutable-free-id-table))
  (check-equal? (dict-ref (dict-set table #'x 1) #'y 0)
                0)

  (dict-ref
   (dict-update
    table #'y
    (λ (current-vs) (append '(1) current-vs))
    (λ () (list)))
   #'y)
  )


;; adjoint-trace+terms : stx? (Listof stx?) (IdTable (Listof id?))
;;           -> (Values (Listof stx?) (IdTable (Listof id?)))
(define-for-syntax (adjoint-trace+terms w Aw adjoint-terms)
  (syntax-case w (define cons car cdr)
    [(define _ (cons x y))
     (let ([Ax (dfn-build #'car Aw)]
           [Ay (dfn-build #'cdr Aw)])
       {values (remove-duplicates (append Aw Ay Ax))
               (upd-adj adjoint-terms #:key last-id #'x Ax #'y Ay)})]

    [(define _ (car xs))
     (let* ([xs* (dfn-get #'xs Aw)]
            [tr  (dfn-build #'cons Aw
                   (dfn-build #'zero
                     (dfn-build #'cdr xs*)))])
       {values (remove-duplicates (append tr Aw))
               (upd-adj adjoint-terms #:key last-id #'xs tr)})]

    [(define _ (cdr xs))
     (let* ([xs* (dfn-get #'xs Aw)]
            [tr  (dfn-build #'cons Aw
                   (dfn-build #'zero
                     (dfn-build #'car xs*)))])
       {values (remove-duplicates (append tr Aw))
               (upd-adj adjoint-terms #:key last-id #'xs tr)})]

    ;; [(define _ (op xs ...))
    ;;  (let ([xs& (for/list ([x xs]) (trace-get x Aw))])
    ;;    (for/fold ([tr Aw]
    ;;               [adjoint-terms adjoint-terms])
    ;;              ([x xs]
    ;;               [i (in-naturals)])
    ;;      (let ([Ax (*& Aw (apply pderiv i op xs&))])
    ;;        {values (trace-append Ax tr)
    ;;                (upd-adj adjoint-terms x Ax)})))]

    ;; constant
    [(define _ c) {values Aw adjoint-terms}]
    ))
