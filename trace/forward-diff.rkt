#lang racket

(provide partial/f
         D/f)

(require (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "primitive-partial.rkt"
         "cons-arithmetic.rkt")

;; takes an assignment, a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; deriv/f : assignment? symbol? (Listof symbol?) trace?
;;     (HashTable symbol? symbol?) -> trace?
(define (deriv/f assgn var indep-ids tr deriv-map)
  ;; the value of the identifier x
  (define (I x) (trace-get x tr))
  ;; the value of the identifier which is the derivative of identifier x
  (define (D x) (I (hash-ref deriv-map x)))
  (cond
    [(eq? (id assgn) var) (val->trace 1.0)]
    [(memq (id assgn) indep-ids) (val->trace 0.0)]
    [else
     (match (expr assgn)
       [(list 'constant '())  null&]
       [(list 'constant c)    (val->trace 0.0)]
       [(list 'app 'cons x y) (app& cons& (D x) (D y))]
       [(list 'app 'car ls)   (app& car& (D ls))]
       [(list 'app 'cdr ls)   (app& cdr& (D ls))]
       [(list 'app op xs ...) (let ([xs& (map I xs)])
                                (for/fold ([acc (val->trace 0.0)])
                                          ([x xs]
                                           [i (in-naturals)])
                                  (define D_i_op (apply (partial i op) xs&))
                                  (app& +& (app& *& D_i_op (D x)) acc)))])]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation
;;
;; partial/f : trace? (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (partial/f i f)
  (lambda& xs
    (let* ([var       (top-id (list-ref xs (top-val i)))]
           [indep-ids (map top-id xs)]
           [result    (apply (top-val f) xs)])
      (define-values (Dresult _)
        (for/fold ([tr result]
                   [deriv-map (hash)])
                  ([a (reverse (trace-items result))])
          (let* ([Da (deriv/f a var indep-ids tr deriv-map)])
            {values
             (trace-append Da tr)
             (hash-set deriv-map (id a) (top-id Da))})))
      (trace-prune (trace-remove-duplicates Dresult)))))

;; The Jacobian of f at xs, computed by forward accumulation
;;
;; D/f : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (D/f f)
  (lambda& xs
    (let* ([n (length xs)]
           [Di (for/list ([i (range n)])
                 (apply (top-val (app& partial/f (val->trace i) f))
                        xs))])
      (apply (top-val list&) Di))))
