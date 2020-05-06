#lang racket

(provide partial/f
         D/f)

(require (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "primitive-partial.rkt")

;; takes an assignment, a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; deriv/f : assignment? trace? (HashTable symbol? symbol?) -> trace?
(define (d-primitive assgn tr deriv-map)
  ;; the trace of the derivative of identifier x
  (define (d x) (trace-get (hash-ref deriv-map x) tr))
  (match (expr assgn)
    [(list 'constant '())  null&]
    [(list 'constant c)    (val->trace 0.0)]
    [(list 'app 'cons x y) (app& cons& (d x) (d y))]
    [(list 'app 'car ls)   (app& car& (d ls))]
    [(list 'app 'cdr ls)   (app& cdr& (d ls))]
    [(list 'app op xs ...)
     (let ([xs-tr (map (λ (x) (trace-get x tr)) xs)])
       (for/fold ([acc& (val->trace 0.0)])
                 ([x xs]
                  [i (in-naturals)])
         (app& +& acc&
               (app& *& (apply (partial i op) xs-tr) (d x)))))]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation
;;
;; partial/f : trace? (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (partial/f i& f&)
  (lambda& xs ; currently rest args in lambda is a plain list
    (let ([arg-ids (map top-id xs)]
          [x-id    (top-id (list-ref xs (top-val i&)))]
          [y&      (apply (top-val f&) xs)])
      (define-values (dy& _)
        (for/fold ([result-trace y&]
                   [derivatives (hash)])
                  ([z-assgn (reverse (trace-items y&))])
          (let ([dz& (cond
                       [(eq? (id z-assgn) x-id) (val->trace 1.0)]
                       [(memq (id z-assgn) arg-ids) (val->trace 0.0)]
                       [else (d-primitive z-assgn result-trace derivatives)])])
            {values
             (trace-append dz& result-trace)
             (hash-set derivatives (id z-assgn) (top-id dz&))})))
      dy&)))

;; The Jacobian of f at xs, computed by forward accumulation
;;
;; D/f : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (D/f f)
  (lambda& xs ; currently rest args in lambda is a plain list
    (cons->trace
     (for/list ([i (range (length xs))])
       (apply (top-val (app& partial/f (val->trace i) f))
              xs)))))
