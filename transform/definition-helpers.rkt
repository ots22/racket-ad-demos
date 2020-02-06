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

(define-for-syntax (dfn-remove-duplicates dfn-forms)
  (with-syntax ([tmp (generate-temporary)]
                [id (last-id dfn-forms)])
    (let ([rm (remove-duplicates dfn-forms)])
      (if (equal? (last-id rm) #'id)
          rm
          (append rm (list #'(define tmp id)))))))

(define-for-syntax (dfn-build op . dfn-lists)
  (with-syntax ([(arg-ids ...) (map last-id dfn-lists)]
                [fresh-id (generate-temporary)]
                [op op])
    (dfn-remove-duplicates
     (append (apply append dfn-lists)
             (list #'(define fresh-id (op arg-ids ...)))))))

(define-for-syntax (dfn-get id dfn-forms)
  (reverse (member id (reverse dfn-forms)
                   (λ (u v) (free-identifier=? u (dfn-id v))))))

(module+ test
  (define-syntax (s stx)
    (define a (dfn-build #'+
                         (list #'(define b '(1 0)))
                         (list #'(define c '(0 2)))))
    (define g (last-id a))
    #`(let () #,@a #,g))

  (check-equal? s '(1 2)))


;; (module+ test
;;   (define table (make-immutable-free-id-table))
;;   (check-equal? (dict-ref (dict-set table #'x 1) #'y 0)
;;                 0)

;;   (dict-ref
;;    (dict-update
;;     table #'y
;;     (λ (current-vs) (append '(1) current-vs))
;;     (λ () (list)))
;;    #'y)
;;   )

;; (define-for-syntax *backprop* (make-parameter (make-immutable-free-id-table)))

;; (define-for-syntax (backprop-set! op <-op)
;;   (*backprop* (dict-set (*backprop*) op <-op)))

;; (begin-for-syntax
;;   (backprop-set! #'+ (λ (s x-id y-id)
;;                        (make-immutable-free-id-table
;;                         (hash x-id s
;;                               y-id s)))))

;; (define-for-syntax (backprop op s . xs)
;;   (apply (dict-ref (*backprop*) op) s xs))

(define-for-syntax (backprop op s . xs)
  (println xs)
  (with-syntax ([s* (last-id s)])
    (let ([xs* (map last-id xs)])
      (syntax-case op (+ *)
        [+ (with-syntax ([g1 (generate-temporary)]
                         [g2 (generate-temporary)]
                         [g3 (generate-temporary)])
             (list #'(define g1 s*)
                   #'(define g2 s*)
                   #'(define g3 (cons g1 g2))))]
        
        [* (with-syntax ([g1 (generate-temporary)]
                         [g2 (generate-temporary)]
                         [g3 (generate-temporary)]
                         [x  (car xs*)]
                         [y  (cadr xs*)])
             (list #'(define g1 (* s* y))
                   #'(define g2 (* s* x))
                   #'(define g3 (cons g1 g2))))]
           ))))
         
;; (define-syntax (s stx)
;;   (syntax-case stx ()
;;     [(_ a b) (*backprop* (dict-set (*backprop*) #'a (syntax->datum #'b)))])
;;   (for ([(k v) (in-dict (*backprop*))])
;;     (display (format "~a : ~a~%" k v)))
;;   #''ok)

;; (s + 4)
;; (s * 4)


;; adjoint-trace+terms : stx? (Listof stx?) (IdTable (Listof id?))
;;           -> (Values (Listof stx?) (IdTable (Listof id?)))
(define-for-syntax (adjoint-trace+terms w Aw adjoint-terms)
  (syntax-case w (define cons car cdr)
    [(define _ (cons x y))
     (let ([Ax (dfn-build #'car Aw)]
           [Ay (dfn-build #'cdr Aw)])
       {values (dfn-remove-duplicates (append Aw Ay Ax))
               (upd-adj adjoint-terms #:key last-id #'x Ax #'y Ay)})]

    [(define _ (car xs))
     (let* ([xs* (dfn-get #'xs Aw)]
            [tr  (dfn-build #'cons Aw
                   (dfn-build #'zero
                     (dfn-build #'cdr xs*)))])
       {values (dfn-remove-duplicates (append tr Aw))
               (upd-adj adjoint-terms #:key last-id #'xs tr)})]

    [(define _ (cdr xs))
     (let* ([xs* (dfn-get #'xs Aw)]
            [tr  (dfn-build #'cons Aw
                   (dfn-build #'zero
                     (dfn-build #'car xs*)))])
       {values (dfn-remove-duplicates (append tr Aw))
               (upd-adj adjoint-terms #:key last-id #'xs tr)})]

    [(define _ (op xs ...))
     (let-values
         ([(new-dfns _ignore new-adjoint-terms)
           (let* ([xs* (for/list ([x (syntax-e #'(xs ...))])
                         (dfn-get x Aw))]
                  [b (apply backprop #'op Aw xs*)]
                  [tr (append Aw b)])
             (for/fold ([tr* tr]
                        [b-cdr b]
                        [adjoint-terms* adjoint-terms])
                       ([x (syntax-e #'(xs ...))])
               (println x)
               (let ([Ax (dfn-build #'car b-cdr)])
                 {values (dfn-remove-duplicates (append tr* Ax))
                         (dfn-build #'cdr b-cdr)
                         (upd-adj adjoint-terms* #:key last-id x Ax)
                         })))])
       {values new-dfns new-adjoint-terms})]
          
    ;; constant
    [(define _ c) {values Aw adjoint-terms}]
    ))
