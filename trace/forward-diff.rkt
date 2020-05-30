#lang racket

(provide D/f
         D/f-memo
         D/f*
         D/f*-memo)

(require memoize
         (for-syntax syntax/parse)
         syntax/parse
         syntax/id-table
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "primitive-partial.rkt"
         "../cons-arithmetic/cons-arithmetic.rkt")

;; Takes an 'expr', a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; D-primitive : expr? trace? (Dictionary symbol? trace?) -> trace?
(define (D-primitive z-expr tr D-map)
  ;; the trace of the derivative of identifier x
  (define-syntax-rule (d x) (dict-ref D-map x))
  (define-syntax-rule (trace-of a) (trace-get a tr))
  (syntax-parse z-expr
    #:literals (cons car cdr cons-add cons-zero null)
    [(cons x y)     (traced (cons& (d #'x) (d #'y)))]
    [(car ls)       (traced (car& (d #'ls)))]
    [(cdr ls)       (traced (cdr& (d #'ls)))]
    [(cons-add a b) (traced (cons-add& (d #'a) (d #'b)))]
    [(cons-zero x)  (traced (cons-zero& (trace-of #'x)))]
    [(op xs ...)
     (let* ([xs-ids (syntax-e #'(xs ...))]
            [xs-trs (for/list ([x xs-ids]) (trace-get x tr))])
       (for/fold ([acc& (traced 0.0)])
                 ([x xs-ids]
                  [i (in-naturals)])
         (let ([d-op (apply (partial i (syntax->datum #'op)) xs-trs)])
           (traced (+& acc& (*& d-op (d x)))))))]
    [null (traced null&)]
    [c (traced 0.0)]))

(define (((D/f* y&) . xs) . dxs)
  (define-values (dy& _)
    (for/fold ([result-trace y&]
               [derivatives (make-immutable-free-id-table
                             (make-hash (map cons (map top-id xs) dxs)))])
              ([z-assgn (reverse (trace-items y&))])
      (define dz&
        (dict-ref
         derivatives (assignment-id z-assgn)
         (λ () (D-primitive (assignment-expr z-assgn)
                            result-trace derivatives))))
      {values
       (trace-append dz& result-trace)
       (dict-set derivatives (assignment-id z-assgn) dz&)}))
  dy&)

;; ----------------------------------------

(define (((D/f*-memo y&) . xs) . dxs)
  (define dxs-map
    (make-immutable-free-id-table
     (make-hash (map cons (map top-id xs) dxs))))
  ;; d : symbol? -> trace?
  (define (d z)
    ;; so we can make a recursive call inside 'traced' conveniently
    (define d& (val->trace d))
    (define z& (trace-get z y&))
    (dict-ref
     dxs-map z 
     (λ () (syntax-parse (top-expr z&)
             #:datum-literals (cons car cdr cons-zero cons-add null)
             [(cons a b) (traced (cons& (d& #'a) (d& #'b)))]
             [(car as)   (traced (car& (d& #'as)))]
             [(cdr as)   (traced (cdr& (d& #'as)))]
             [(cons-add a b) (traced (cons-add& (d& #'a) (d& #'b)))]
             [(cons-zero a) z&]
             [(op as ...)
              (let ([as-trs (map (curryr trace-get y&) (syntax-e #'(as ...)))])
                (for/fold ([acc& (traced 0.0)])
                          ([a (syntax-e #'(as ...))]
                           [i (in-naturals)])
                  (let ([d-op (apply (partial i (syntax->datum #'op)) as-trs)])
                    (traced (+& acc& (*& d-op (d& a)))))))]
             [null  (traced null&)]
             [c     (traced 0.0)]))))
  (d (top-id y&)))

;; ----------------------------------------

(define& (D/f f&)
  (val->trace
   (λ xs
     (let ([y& (apply (top-val f&) xs)])
       (val->trace (apply (D/f* y&) xs))))))

(define& (D/f-memo f&)
  (val->trace
   (λ xs
     (let ([y& (apply (top-val f&) xs)])     
       (val->trace (apply (D/f*-memo y&) xs))))))
