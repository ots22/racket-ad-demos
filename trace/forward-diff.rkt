#lang racket

(provide partial/f
         D/f)

(require (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "trace-apply.rkt"
         "let-traced.rkt"
         "primitive-partial.rkt")

;; takes an 'expr', a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; deriv/f : expr? trace? (HashTable symbol? symbol?) -> trace?
(define (d-primitive z-expr tr deriv-map)
  ;; the trace of the derivative of identifier x
  (define-syntax-rule (d x) (trace-get (hash-ref deriv-map x) tr))
  (match z-expr
    [(list 'constant '())  (traced null&)]
    [(list 'constant c)    (traced 0.0)]
    [(list 'app 'cons x y) (traced (cons& (d x) (d y)))]
    [(list 'app 'car ls)   (traced (car& (d ls)))]
    [(list 'app 'cdr ls)   (traced (cdr& (d ls)))]
    [(list 'app op xs ...)
     (let ([xs-trs (for/list ([x xs]) (trace-get x tr))])
       (for/fold ([acc& (traced 0.0)])
                 ([x xs]
                  [i (in-naturals)])
         (let ([d-op (apply (partial i op) xs-trs)])
           (traced (+& acc& (*& d-op (d x)))))))]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation
;;
;; partial/f : trace? (trace? ... -> trace?) -> trace? ... -> trace?
(define& (partial/f i& f&)
  (val->trace
   (lambda xs ;; not lambda&
     (let ([arg-ids (map top-id xs)]
           [x-id    (top-id (list-ref xs (top-val i&)))]
           [y&      (apply (top-val f&) xs)])
       (define-values (dy& _)
         (for/fold ([result-trace y&]
                    [derivatives (hash)])
                   ([z-assgn (reverse (trace-items y&))])
           (let ([dz& (cond
                        [(eq? (id z-assgn) x-id) (traced 1.0)]
                        [(memq (id z-assgn) arg-ids) (traced 0.0)]
                        [else (d-primitive (expr z-assgn) result-trace derivatives)])])
             {values
              (trace-append dz& result-trace)
              (hash-set derivatives (id z-assgn) (top-id dz&))})))
       dy&))))

(define (directional-derivative/f y& x-ids s&)
  (define-values (dy& _)
    (for/fold ([result-trace y&]
               [derivatives (make-immutable-hash
                             (map cons x-ids (trace-e s&)))])
              ([z-assgn (reverse (trace-items y&))])
      (define dz&
        (hash-ref
         derivatives (id z-assgn)
         (λ () (d-primitive (expr z-assgn) result-trace derivatives))))
      {values
       (trace-append dz& result-trace)
       (hash-set derivatives (id z-assgn) dz&)}))
  dy&)


;; The Jacobian of f at xs, computed by forward accumulation
;;
;; D/f : (trace? ... -> trace?) -> trace? ... -> trace?
(define& (D/f f&)
  (lambda& xs&
    (cons->trace
     (for/list ([i (range (length (top-val xs&)))])
       (let ([i& (val->trace i)])
         [traced (apply& (partial/f i& f&) xs&)])))))

(define& (D/s/f s& f&)
  (val->trace
   (lambda xs
     (let ([x-ids (map top-id xs)]
           [y& (apply (top-val f&) xs)])
       (directional-derivative/f y& x-ids s&)))))
