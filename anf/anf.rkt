#lang racket

(provide anf-conv
         anf-val
         anf-expr
         anf?
         anf-normalize)

(require syntax/parse
         racket/syntax
         "util.rkt")

(module+ test
  (require rackunit))

;; ----------------------------------------
;; Syntax classes for ANF

(define-conventions anf-conv
  [#rx"^V" anf-val]
  [#rx"^M" anf-expr]
  [x id]
  [xs id]
  [c anf-const])

(define-syntax-class anf-const
  (pattern a:boolean)
  (pattern a:number))

(define-syntax-class anf-val
  #:conventions (anf-conv)
  #:literal-sets (kernel-literals)
  (pattern (quote c))
  (pattern x)
  (pattern (#%plain-lambda (xs ...) M)))

(define-syntax-class anf-expr
  #:conventions (anf-conv)
  #:literal-sets (kernel-literals)
  ;; return
  (pattern V0)
  ;; tail call
  (pattern (#%plain-app V0 V ...))
  ;; bind
  (pattern (let-values (((x) V0)) M))
  ;; call
  (pattern (let-values (((x) (#%plain-app V0 V ...))) M))
  ;; branch
  (pattern (if V0 M-true M-false)))

(define (anf? stx)
  (syntax-parse stx
    [_:anf-expr #t]
    [_ #f]))

(module+ test
  ;; namespace for expansion
  (parameterize ([current-namespace (make-base-namespace)])
    (check-true  (anf? (expand #'(let ((a 1)) (+ 1 2)))))
    (check-false (anf? (expand #'(let ((a (let ((b 1)) b))) a))))))

;; ----------------------------------------

;; From Flanagan, C., Sabry, A., Duba, B. F., & Felleisen, M., "The
;; essence of compiling with continuations." In proceedings, ACM
;; SIGPLAN 1993 (pp. 237-247).

(define (anf-normalize stx [k identity])
  (syntax-parse stx
    #:conventions (anf-conv)
    #:literal-sets (kernel-literals)
    [(#%plain-lambda (xs ...) u)
     #:with M (anf-normalize #'u)
     (k #'(#%plain-lambda (xs ...) M))]
    ;;
    [(let-values (((x) u1)) u2)
     #:with M2 (anf-normalize #'u2 k)
     (anf-normalize #'u1 (pat-λ (M1) #'(let-values (((x) M1)) M2)))]
    ;;
    [(if u1 u2 u3)
     #:with M2 (k (anf-normalize #'u2))
     #:with M3 (k (anf-normalize #'u3))
     (anf-normalize-name #'u1 (pat-λ (t) #'(if t M2 M3)))]
    ;;
    [(#%plain-app u . us)
     (anf-normalize-name
      #'u
      (pat-λ (t) (anf-normalize-name*
                  #'us
                  (pat-λ (t*) (k #'(#%plain-app t . t*))))))]
    ;;
    [V (k #'V)]))

(define (anf-normalize-name* stx k)
  (syntax-parse stx
    [() (k #'())]
    [(v . vs)
     (anf-normalize-name
      #'v
      (pat-λ (t) (anf-normalize-name*
                  #'vs
                  (pat-λ (t*) (k #'(t . t*))))))]))

(define (anf-normalize-name stx k)
  (anf-normalize
   stx
   (λ (M) (syntax-parse M
            #:conventions (anf-conv)
            #:literal-sets (kernel-literals)
            [V (k M)]
            [_
             #:with t (generate-temporary)
             #:with M* M
             #:with kt (k #'t)
             #'(let-values (((t) M*))
                 kt)]))))

(module+ test
  (check-true
   (with-syntax ([x #'(if '#t (#%plain-app + '1 '2) (#%plain-app + '2 '3))]
                 [y #'(if '#f (#%plain-app + '3 '4) (#%plain-app / '1 '2))])
     (anf? (anf-normalize #'(#%plain-app + x y)))))

  (parameterize ([current-namespace (make-base-namespace)])
    (define u1 (expand #'(#%plain-app + (let-values (((a) '1)) a) (let-values (((a) '2)) a))))
    (define M1 (anf-normalize u1))
    (check-true (anf? M1))
    (check-equal? (eval M1) 3)))


