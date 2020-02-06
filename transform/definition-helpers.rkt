#lang racket

;(provide dfn-build)
(provide (for-syntax dfn-build)
         (for-syntax adjoint-trace+terms)
         (for-syntax D/r)
         
         define/d
         define
         #%module-begin
         #%top
         #%app
         #%top-interaction
         #%datum
         +
         zero
         *
         cons
         car
         cdr
         null)

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
    [(_ id rest ...) #'id]
    [id #'id]))

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

(define-for-syntax (dfn-prune t)
  (define (rec t seen)
    (if (false? t)
        seen
        (syntax-case (last t) (define)
          [(define _ (f xs ...)) 
           (apply set-union
                  (map (λ (x) 
                         (let ([x-def (dfn-get x t)])
                           (rec x-def (set-add seen x))))
                       (syntax-e #'(xs ...))))]
          [_ seen])))
  (let ([seen (rec t (set (last-id t)))])
    (filter (λ (a) (set-member? seen (dfn-id a)))
            t)))

(define-for-syntax (dfn-datum d)
  (with-syntax ([g (generate-temporary)]
                [d #'d])
    (list #'(define g d))))

(module+ test
  (define-syntax (s stx)
    (define a (dfn-build #'+
                         (list #'(define b '(1 0)))
                         (list #'(define c '(0 2)))))
    (define g (last-id a))
    #`(let () #,@a #,g))
  
  (check-equal? s '(1 2))
  )

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
  (with-syntax ([s* (last-id s)])
    (syntax-case op (+ * null)
      [+ (with-syntax ([(g1 g2 g3 g4 g5) (generate-temporaries (range 5))])
           (list #'(define g1 s*)
                 #'(define g2 s*)
                 #'(define g3 null)
                 #'(define g4 (cons g2 g3))
                 #'(define g5 (cons g1 g4))))]
      
      [* (with-syntax ([(g1 g2 g3 g4 g5) (generate-temporaries (range 5))]
                       [x  (car xs)]
                       [y  (cadr xs)])
           (list #'(define g1 (* s* y))
                 #'(define g2 (* s* x))
                 #'(define g3 null)
                 #'(define g4 (cons g2 g3))
                 #'(define g5 (cons g1 g4))))]
      )))



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
           (let* ([xs* (syntax-e #'(xs ...))]
                  [b (apply backprop #'op Aw xs*)]
                  [tr (append Aw b)])
             (for/fold ([tr* tr]
                        [b-cdr b]
                        [adjoint-terms* adjoint-terms])
                       ([x xs*])
               (let ([Ax (dfn-build #'car b-cdr)])
                 {values (dfn-remove-duplicates (append tr* Ax))
                         (dfn-build #'cdr b-cdr)
                         (upd-adj adjoint-terms* #:key last-id x Ax)
                         })))])
       {values new-dfns new-adjoint-terms})]

    ;; constant
    [(define _ c) {values Aw adjoint-terms}]

    [id {values Aw adjoint-terms}]

    ))


;; (define-for-syntax (display-trace+terms tr tab)
;;   (for ([dfn tr])
;;     (displayln (syntax->datum dfn)))
  
;;   (for ([(k v) (in-dict tab)])
;;     (display (format "~a : ~a~%" (syntax->datum k) (if (pair? v)
;;                                                        (map syntax->datum v)
;;                                                        (syntax->datum v)))))
;;   )


(define-for-syntax (D/r result-tr indep-ids s)
  (define seed-id (last-id result-tr))
  (define seed-tr (append result-tr s))

  (define-values (tr _adjoint-terms adjoints)
    (for/fold (;; tr holds the current trace
               [tr seed-tr]
               ;; terms (Listof ids) contributing to the adjoint of the key
               [adjoint-terms (make-immutable-free-id-table 
                               (hash seed-id (list (last-id seed-tr))))]
               ;; the adjoints of each id seen
               [adjoints (make-immutable-free-id-table)])

              ;; iterate through each assignment, last to first
              ([w (append (reverse result-tr) indep-ids)])

      ;; Firstly, calculate the adjoint of the current assignment, w, and
      ;; put this at the head of the current trace, as Aw.
      (let*-values
          (;; list of traces of the terms that sum to Adj (id w)
           [(Aw-terms) (for/list ([k (dict-ref adjoint-terms (dfn-id w))])
                         (dfn-get k tr))]

           ;; adj-terms can't be empty
           [(Aw) (append tr (foldl (curry dfn-build #'+)
                                   (car Aw-terms) (cdr Aw-terms)))]

           [(adjoints*) (dict-set adjoints (dfn-id w) (last-id Aw))]

           ;; returns an updated trace, with the terms needed to
           ;; compute the adjoints of the variables in the rhs of the
           ;; assignment w, along with a map
           [(tr* adjoint-terms*) (adjoint-trace+terms w Aw adjoint-terms)])

        {values (dfn-remove-duplicates tr*)
                adjoint-terms*
                adjoints*})))

   (let* ([tr* (with-syntax ([g (generate-temporary)])
                 (append tr (list #'(define g 0.0))))]
          [zero-id (last-id tr*)])
     (foldl (curry dfn-build #'cons)
            (with-syntax ([g (generate-temporary)])
              (list #'(define g null)))
            ; reverse so foldl cons constructs the list in the correct order
            (for/list ([x (reverse indep-ids)])
              (dfn-get (dict-ref adjoints x zero-id) tr*))))
  )


(define-for-syntax (handle-assignments args body)
  ;(println args)
  ;(println body)
  (D/r (syntax-e body) (syntax-e args)
       (with-syntax ([s (generate-temporary "s")])
         (list #'(define s 1.0)))))


(define-syntax (define/d stx)
  (syntax-case stx ()
    [(_ (f args ...) body ...)
     (with-syntax ([(body* ...) (handle-assignments #'(args ...) #'(body ...))])
       #'(define (f args ...)
           body* ...
           ))]))

