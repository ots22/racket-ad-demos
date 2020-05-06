#lang racket

(provide A/r
         D/r)

(require (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "primitive-partial.rkt"
         (suffix-in & "cons-arithmetic.rkt")
         (suffix-in & "trace-function.rkt"))

;; ----------------------------------------
;; Reverse mode AD

;; adjoint-trace+terms
;;
;; w: the current term (an assignment)
;;
;; Aw: the trace of the adjoint of the current term
;;
;; adjoint-terms: a map from a symbol s to the symbol representing Adj(s)
;;
;; returns:
;;   - additional trace items needed to compute (other) adjoints
;;   - an updated map of terms comprising these adjoints
;;
;; adjoint-trace+terms :
;;   assignment? (HashTable symbol? (Listof symbol?))
;;           -> (Values trace? (HashTable symbol? (Listof symbol?)))
(define (adjoint-trace+terms w Aw adjoint-terms)
  (match (expr w)
    [(list 'constant c) {values Aw adjoint-terms}]

    [(list 'app 'cons x y)
     (let ([Ax (app& car& Aw)]
           [Ay (app& cdr& Aw)])
       {values (trace-append Ay Ax Aw)
               (upd-adj adjoint-terms #:key top-id x Ax y Ay)})]

    [(list 'app c_r xs) #:when (or (eq? c_r 'car) (eq? c_r 'cdr))
     (let* ([xs& (trace-get xs Aw)]
            [tr  (case c_r
                   [(car) (app& cons& Aw (app& cons-zero& (app& cdr& xs&)))]
                   [(cdr) (app& cons& (app& cons-zero& (app& car& xs&)) Aw)])])
       {values (trace-append tr Aw)
               (upd-adj adjoint-terms #:key top-id xs tr)})]

    [(list 'app op xs ...)
     (let ([xs& (for/list ([x xs]) (trace-get x Aw))])
       (for/fold ([tr Aw]
                  [adjoint-terms adjoint-terms])
                 ([x xs]
                  [i (in-naturals)])
         (let ([Ax (app& *& Aw (apply (partial i op) xs&))])
           {values (trace-append Ax tr)
                   (upd-adj adjoint-terms #:key top-id x Ax)})))]
    ))

;; A helper for D/r. It is not provided by the module. It has a
;; different interface to partial/f.
;;
;; result-tr : the trace of the result
;; indep-ids : the variables with respect to which we are
;;             differentiating (symbols)
;;         s : the initial seed, which must be a trace of a pair with
;;             the same shape as the result.  This should almost
;;             certainly have a '1' in one position, and zeros
;;             elsewhere.
;;
;; A/r : trace? (Listof symbol?) trace? -> trace?
(define (A/r result-tr indep-ids s)
  (define seed-id (top-id result-tr))
  (define seed-tr (trace-append s result-tr))

  (define-values (tr _adjoint-terms adjoints)
    (for/fold (;; tr holds the current trace
               [tr seed-tr]
               ;; terms (Listof ids) contributing to the adjoint of the key
               [adjoint-terms (hash seed-id (list (top-id seed-tr)))]
               ;; the adjoints of each id seen
               [adjoints (hash)])

              ;; iterate through each assignment, last to first
              ([w (trace-items result-tr)])

      ;; Firstly, calculate the adjoint of the current assignment, w, and
      ;; put this at the head of the current trace, as Aw.
      (let*-values
          (;; list of traces of the terms that sum to Adj (id w)
           [(Aw-terms) (for/list ([k (hash-ref adjoint-terms (id w))])
                         (trace-get k tr))]

           ;; adj-terms can't be empty
           [(Aw) (trace-append
                  (foldl (top-val cons-add&) (car Aw-terms) (cdr Aw-terms))
                  tr)]

           [(adjoints*) (hash-set adjoints (id w) (top-id Aw))]

           ;; returns an updated trace, with the terms needed to
           ;; compute the adjoints of the variables in the rhs of the
           ;; assignment w, along with a map
           [(tr* adjoint-terms*) (adjoint-trace+terms w Aw adjoint-terms)])

        {values tr*
                adjoint-terms*
                adjoints*})))

  (let* ([tr* (trace-add tr (make-assignment #:val 0.0))]
         [zero-id (top-id tr*)])
    (trace-prune
     (apply (top-val list&)
            (for/list ([x indep-ids])
              (trace-get (hash-ref adjoints x zero-id) tr*))))))

;; The Jacobian of f at xs, computed by reverse accumulation
;;
;; D/r : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (D/r f)
  (lambda& xs
    (let* ([indep-ids (map top-id xs)]
           [result-tr (apply (top-val f) xs)]
           [result (top-val result-tr)]
           [result-flat (flatten result)]
           [n (length result-flat)])
      ;; flatten the result, seed each element in turn, reshape back to
      ;; have the same shape as the result, then call A/r.  Accumulate
      ;; into a list, then reshape back to have the shape of result.
      ;; Finally, convert cons of traces to trace of conses
      (cons->trace
       (reshape result
                (for/list ([i (in-range n)])
                  (let ([s (cons->trace
                            (reshape result
                                     (map exact->inexact
                                          (ind-list n i))))])
                    (A/r result-tr indep-ids s))))))))
