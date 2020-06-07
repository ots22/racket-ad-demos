#lang racket

(provide anf-convention
         anf-val
         anf-expr
         anf?
         simplified-anf-expr
         simplified-anf?        
         anf-normalize
         simplify-anf)

(require syntax/parse
         racket/syntax
         "util.rkt"
         (only-in "../trace/util.rkt" all-equal?))

(module+ test
  (require rackunit))

;; ----------------------------------------
;; Syntax classes for ANF

(define-conventions anf-convention
  [#rx"^V" anf-val]
  [#rx"^B" simplified-anf-val]
  [#rx"^M" anf-expr]
  [#rx"^S" simplified-anf-expr]
  [x id]
  [x0 id]
  [xs id]
  [c anf-const])

(define-syntax-class anf-const
  (pattern a:boolean)
  (pattern a:number))

(define-syntax-class anf-val
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda (xs ...) M)))

(define-syntax-class simplified-anf-val
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda (xs ...) S)))

(define-syntax-class anf-expr
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern V0) ; return
  (pattern (#%plain-app V0 V ...)) ; tail call
  (pattern (let-values (((x) V0)) M)) ; bind
  (pattern (let-values (((x) (#%plain-app V0 V ...))) M)) ; call
  (pattern (if V0 M-true M-false))) ; branch

(define anf? (syntax-class->predicate anf-expr))

(define-syntax-class simplified-anf-expr
  #:conventions (anf-convention)
  #:literal-sets (kernel-literals)
  (pattern x)
  (pattern (let-values (((x) B)) S))
  (pattern (let-values (((x) (#%plain-app x0 xs ...))) S))
  (pattern (if B S1 S2)))

(define simplified-anf? (syntax-class->predicate simplified-anf-expr))

;; ----------------------------------------

;; walk stx, which must be a syntax list, calling f on each element: f
;; takes a syntax object and a function (the continuation); k is the
;; initial continuation.
;;
(define (walk-with f stx k)
  (syntax-parse stx
    [() (k #'())]
    [(v vs ...)
     (f #'v (pat-λ (t) (walk-with f #'(vs ...) (pat-λ (ts) (k #'(t . ts))))))]))

;; ----------------------------------------

;; From Flanagan, C., Sabry, A., Duba, B. F., & Felleisen, M., "The
;; essence of compiling with continuations." In proceedings, ACM
;; SIGPLAN 1993 (pp. 237-247).
;;
;; anf-normalize: syntax? (syntax? -> syntax?) -> anf?
(define (anf-normalize stx [k identity])
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [(#%plain-lambda (xs ...) u)
     #:with M (anf-normalize #'u)
     (k #'(#%plain-lambda (xs ...) M))]
    ;;
    [(let-values (((x) u1)) u2)
     #:with M2 (anf-normalize #'u2 k)
     (anf-normalize #'u1 (pat-λ (r) #'(let-values (((x) r)) M2)))]
    ;;
    [(if u1 u2 u3)
     #:with M2 (k (anf-normalize #'u2))
     #:with M3 (k (anf-normalize #'u3))
     (anf-normalize-name #'u1 (pat-λ (t) #'(if t M2 M3)))]
    ;;
    [(#%plain-app u us ...)
     (walk-with anf-normalize-name
                #'(u us ...)
                (pat-λ (r) (k #'(#%plain-app . r))))]
    ;;
    [V (k #'V)]))

(define (anf-normalize-name stx k)
  (anf-normalize
   stx
   (λ (u) (syntax-parse u
            #:conventions (anf-convention)
            #:literal-sets (kernel-literals)
            [V (k u)]
            [_ #:with x (generate-temporary)
               #`(let-values (((x) #,u))
                   #,(k #'x))]))))

;; ----------------------------------------

;; simplify-anf: anf? -> simplified-anf?
(define (simplify-anf stx)
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [(let-values (((x) V)) M)
     #:with sM (simplify-anf #'M)
     (simplify-anf-value #'V (pat-λ (r) #'(let-values (((x) r)) sM)))]
    ;;
    [(if V M1 M2)
     #:with sM1 (simplify-anf #'M1)
     #:with sM2 (simplify-anf #'M2)
     #'(if V sM1 sM2)]
    ;;
    [(let-values (((x) (#%plain-app V Vs ...))) M)
     #:with sM (simplify-anf #'M)
     (walk-with simplify-anf-lift-value
                #'(V Vs ...)
                (pat-λ (r) #'(let-values (((x) (#%plain-app . r)))
                               sM)))]
    ;;
    [(#%plain-app V Vs ...)
     #:with x (generate-temporary)
     (simplify-anf
      #'(let-values (((x) (#%plain-app V Vs ...)))
          x))]
    ;;
    [x #'x]
    ;;
    [V #:with x (generate-temporary)
       #'(let-values (((x) V)) x)]))

(define (simplify-anf-value v k)
  (syntax-parse v
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [(#%plain-lambda (xs ...) M)
     #:with sM (simplify-anf #'M)
     (k #'(#%plain-lambda (xs ...) sM))]
    [B (k #'B)]))

(define (simplify-anf-lift-value v k)
  (syntax-parse v
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    [x (k v)]
    [(#%plain-lambda (xs ...) M)
     #:with sM (simplify-anf #'M)
     #:with t (generate-temporary)
     #`(let-values (((t) (#%plain-lambda (xs ...) sM)))
         #,(k #'t))]
    [(quote c)
     #:with t (generate-temporary)
     #`(let-values (((t) (quote c)))
         #,(k #'t))]))

;; ----------------------------------------

(module+ test
  (parameterize ([current-namespace (make-base-namespace)])
    (test-case "anf/expand"
      (check-true  (anf? (expand #'(let ((a 1)) (+ 1 2)))))
      (check-false (anf? (expand #'(let ((a (let ((b 1)) b))) a)))))

    (test-case "anf/normalize"
      (check-true
       (with-syntax ([x #'(if '#t (#%plain-app + '1 '2) (#%plain-app + '2 '3))]
                     [y #'(if '#f (#%plain-app + '3 '4) (#%plain-app / '1 '2))])
         (anf? (anf-normalize #'(#%plain-app + x y))))))

    (test-case "anf/normalize 2"
      (define u1 (expand #'(#%plain-app + (let-values (((a) '1)) a) (let-values (((a) '2)) a))))
      (define M1 (anf-normalize u1))
      (check-true (anf? M1))
      (check-equal? (eval M1) 3))

    (test-case "anf/normalize 3"
      (define fib-stx
        (expand
         #'(let ((fib (lambda (n k)
                        (if (< n 2)
                            1
                            (+ (k (- n 1) k)
                               (k (- n 2) k))))))
             (fib 20 fib))))

      (check-false (anf? fib-stx))

      (define fib-stx-anf (anf-normalize fib-stx))
      (check-true (anf? fib-stx-anf))
      (check-false (simplified-anf? fib-stx-anf))


      (define fib-stx-anf-simplified (simplify-anf fib-stx-anf))
      
      (check-true (simplified-anf? fib-stx-anf-simplified))

      (check-true (all-equal? 10946
                              (eval fib-stx)
                              (eval fib-stx-anf)
                              (eval fib-stx-anf-simplified))))))
    


