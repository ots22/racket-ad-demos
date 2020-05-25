#lang racket

(provide D/f
         D/f-memo
         D/f*
         D/f*-memo)

(require memoize
         (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "primitive-partial.rkt")

;; Takes an 'expr', a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; D-primitive : expr? trace? (HashTable symbol? trace?) -> trace?
(define (D-primitive z-expr tr D-map)
  ;; the trace of the derivative of identifier x
  (define-syntax-rule (d x) (hash-ref D-map x))
  (define-syntax-rule (trace-of a) (trace-get a tr))
  (match z-expr
    [(list 'cons x y) (traced (cons& (d x) (d y)))]
    [(list 'car ls)   (traced (car& (d ls)))]
    [(list 'cdr ls)   (traced (cdr& (d ls)))]
    [(list 'cons-add a b) (traced (cons-add& (d a) (d b)))]
    [(list 'cons-zero x) (traced (cons-zero& (trace-of x)))]
    [(list op xs ...)
     (let ([xs-trs (for/list ([x xs]) (trace-get x tr))])
       (for/fold ([acc& (traced 0.0)])
                 ([x xs]
                  [i (in-naturals)])
         (let ([d-op (apply (partial i op) xs-trs)])
           (traced (+& acc& (*& d-op (d x)))))))]
    ['()  (traced null&)]
    [c    (traced 0.0)]))

(define D/f*
  (val->trace
   (λ (y& . dxs)
     (val->trace
      (λ xs
        (define-values (dy& _)
          (for/fold ([result-trace y&]
                     [derivatives (make-immutable-hash
                                   (map cons (map top-id xs) dxs))])
                    ([z-assgn (reverse (trace-items y&))])
            (define dz&
              (hash-ref
               derivatives (id z-assgn)
               (λ () (D-primitive (expr z-assgn) result-trace derivatives))))
            {values
             (trace-append dz& result-trace)
             (hash-set derivatives (id z-assgn) dz&)}))
        dy&)))))

;; ----------------------------------------

(define D/f*-memo
  (val->trace
   (λ (y& . dxs)
     (val->trace
      (λ xs
        (define dxs-map
          (make-immutable-hash (map cons (map top-id xs) dxs)))
        ;; d : symbol? -> trace?
        (define/memo (d z)
          ;; so we can make a recursive call inside 'traced' conveniently
          (define d& (val->trace d))
          (define z& (trace-get z y&))
          (hash-ref
           dxs-map z 
           (λ () (match (top-expr z&)
                   [(list 'cons a b) (traced (cons& (d& a) (d& b)))]
                   [(list 'car as)   (traced (car& (d& as)))]
                   [(list 'cdr as)   (traced (cdr& (d& as)))]
                   [(list 'cons-add a b) (traced (cons-add& (d& a) (d& b)))]
                   [(list 'cons-zero a) z&]
                   [(list op as ...)
                    (let ([as-trs (map (curryr trace-get y&) as)])
                      (for/fold ([acc& (traced 0.0)])
                                ([a as]
                                 [i (in-naturals)])
                        (let ([d-op (apply (partial i op) as-trs)])
                          (traced (+& acc& (*& d-op (d& a)))))))]
                   ['()  (traced null&)]
                   [c    (traced 0.0)]))))
        (d (top-id y&)))))))

;; ----------------------------------------

(define D/f
  (val->trace
   (λ (f& . dxs)
     (val->trace
      (λ xs
        (let ([y& (apply (top-val f&) xs)])
          (apply (top-val (apply (top-val D/f*) y& dxs)) xs)))))))

(define D/f-memo
  (val->trace
   (λ (f& . dxs)
     (val->trace
      (λ xs
        (let ([y& (apply (top-val f&) xs)])
          (apply (top-val (apply (top-val D/f*) y& dxs)) xs)))))))
