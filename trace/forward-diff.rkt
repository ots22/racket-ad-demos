#lang racket

(provide partial/f
         D/f)

(require (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "let-traced.rkt"
         "primitive-partial.rkt")

;; takes an assignment, a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; deriv/f : assignment? trace? (HashTable symbol? symbol?) -> trace?
(define (d-primitive assgn tr deriv-map)
  ;; the trace of the derivative of identifier x
  (define-syntax-rule (d x) (trace-get (hash-ref deriv-map x) tr))
  (match (expr assgn)
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
;; partial/f : trace? (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (partial/f i& f&)
  (lambda& xs ; currently rest args in lambda is a plain list
    (let ([arg-ids (map top-id xs)]
          [x-id    (top-id (list-ref xs (top-val i&)))]
          [y&      (apply (top-val f&) xs)]) ;; --> (traced (apply& f& xs&))
      (define-values (dy& _)
        (for/fold ([result-trace y&]
                   [derivatives (hash)])
                  ([z-assgn (reverse (trace-items y&))])
          (let ([dz& (cond
                       [(eq? (id z-assgn) x-id) (traced 1.0)]
                       [(memq (id z-assgn) arg-ids) (traced 0.0)]
                       [else (d-primitive z-assgn result-trace derivatives)])])
            {values
             (trace-append dz& result-trace)
             (hash-set derivatives (id z-assgn) (top-id dz&))})))
      dy&)))

;; The Jacobian of f at xs, computed by forward accumulation
;;
;; D/f : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (D/f f&)
  (lambda& xs ; currently rest args in lambda& is a plain list
    (cons->trace
     (for/list ([i (range (length xs))])
       (let ([i& (val->trace i)])
         (apply (top-val (traced (partial/f i& f&)))
                xs))))))
