#lang racket

(provide A/r-memo)

(require memoize
         "util.rkt"
         "assignment.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "trace-apply.rkt"
         "primitive-partial.rkt"
         "let-traced.rkt"
         "../cons-arithmetic/cons-arithmetic.rkt")

(define A/r-memo
  (val->trace
   (λ (f& Ay&)
     (val->trace
      (λ xs
        (define tr (apply (top-val f&) xs))
        (define/memo (A x)
          (define A& (val->trace A))
          (define x& (trace-get x tr))
          (if (eq? x (top-id tr))
              Ay&
              (for*/fold ([acc& (traced (cons-zero& x&))])
                         ([w& (depends-on x tr)]
                          [term (uses-in x (top-expr w&))])
                (define Aw& (A (top-id w&)))
                (match term
                  [(list 'cons (list a) b) (traced (car& Aw&))]
                  [(list 'cons a (list b)) (traced (cdr& Aw&))]
                  [(list 'car (list a)) (traced (cons& Aw& (cons-zero (cdr& x&))))]
                  [(list 'cdr (list a)) (traced (cons& (cons-zero (cdr& x&)) Aw&))]
                  [(list 'cons-zero (list a)) (traced (cons-zero& x&))]
                  [(list 'cons-add (list a) b) (traced Aw&)]
                  [(list 'cons-add a (list b)) (traced Aw&)]
                  [(list op a ... (list b) c ...)
                   (let ([d-op (apply (partial (length a) op) (map (curryr trace-get tr) (append a (list b) c)))])
                     (traced (+& acc& (*& Aw& d-op))))]
                  ;; we know there *is* a use, so an error if we get here
                  ))))
        (cons->trace (map (compose1 A top-id) xs)))))))
