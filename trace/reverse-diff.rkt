#lang racket

(provide A/r
         D/r)

(require  "util.rkt"
          "trace.rkt"
          "trace-core.rkt"
          "trace-util.rkt"
          "trace-apply.rkt"
          "primitive-partial.rkt"
          "let-traced.rkt"
          (suffix-in & "cons-arithmetic.rkt")
          (suffix-in & "trace-function.rkt"))

;; update-tr+terms : trace? (HashTable symbol? (Listof symbol?))
;;     -> (Values trace? (HashTable symbol? (Listof symbol?)))
(define (update-tr+terms Aw& A-terms . id+As)
  {values
   (apply trace-append (append (map second (chunk 2 id+As))
                               (list Aw&)))
   (apply dict-list-update A-terms #:key top-id
          id+As)})

;; A-primitive
;;
;; w-expr: the current term's expression
;;
;; Aw&: the trace of the adjoint of the current term
;;
;; A-terms: a map from a symbol s to the symbol representing the
;;   adjoint of s
;;
;; returns:
;;   - additional trace items needed to compute (other) adjoints
;;   - an updated map of terms comprising these adjoints
;;
;; A-primitive :
;;   expr? trace? (HashTable symbol? (Listof symbol?))
;;           -> (Values trace? (HashTable symbol? (Listof symbol?)))
(define (A-primitive w-expr Aw& A-terms)
  (define-syntax-rule (trace-of a) (trace-get a Aw&))
  (match w-expr
    [(list 'cons x y)
     (update-tr+terms Aw& A-terms
                      x (traced (car& Aw&))
                      y (traced (cdr& Aw&)))]
    [(list 'car xs)
     (update-tr+terms Aw& A-terms
                      xs (traced
                          (cons& Aw& (cons-zero& (cdr& (trace-of xs))))))]
    [(list 'cdr xs)
     (update-tr+terms Aw& A-terms
                      xs (traced
                          (cons& (cons-zero& (car& (trace-of xs))) Aw&)))]
    [(list op xs ...)
     (for/fold ([tr Aw&]
                [A-terms A-terms])
               ([x xs]
                [i (in-naturals)])
       (define d-op (apply (partial i op)
                           (map (λ (x) (trace-of x)) xs)))
       (update-tr+terms tr A-terms
                        x (traced (*& Aw& d-op))))]
    [c {values Aw& A-terms}]))

;; A helper for D/r.  Note the different arguments to partial/f.
;;
;;      y& : the trace of the result of calling the function
;; arg-ids : the variables with respect to which we are
;;           differentiating (symbols)
;;      s& : the initial 'seed' (adjoint of y), which must be a trace
;;           of a pair with the same shape as y.  This should almost
;;           certainly have a '1' in one position, and '0's elsewhere.
;;
;; A/r : trace? (Listof symbol?) trace? -> trace?
(define (A/r y& arg-ids s&)
  (define-values (tr _ adjoints)
    (for/fold ([tr (trace-append s& y&)]
               ;; terms (Listof ids) contributing to the adjoint
               [adjoint-terms (hash (top-id y&) (list (top-id s&)))]
               ;; the adjoints of each id seen
               [adjoints (hash)])
              ;; iterate through each assignment, starting with the
              ;; most recent
              ([w-assgn (trace-items y&)])
      (define/contract Aw-terms (non-empty-listof trace?)
        (for/list ([k (hash-ref adjoint-terms (id w-assgn))])
          (trace-get k tr)))

      (define Aw& (trace-append
                   (foldl (top-val cons-add&) (car Aw-terms) (cdr Aw-terms))
                   tr))
      ;; tr* : the assignments needed for the adjoints of the ids in the RHS
      ;;       of w-assgn (appended to tr)
      ;; adjoint-terms* : the updated map of id -> adjoint id
      (define-values (tr* adjoint-terms*)
        (A-primitive (expr w-assgn) Aw& adjoint-terms))

      {values tr*
              adjoint-terms*
              (hash-set adjoints (id w-assgn) (top-id Aw&))}))

  (let* ([tr* (trace-append (val->trace 0.0) tr)]
         [zero-id (top-id tr*)])
    (trace-prune
     (cons->trace
      (for/list ([x arg-ids])
        (trace-get (hash-ref adjoints x zero-id) tr*))))))

;; The Jacobian of f at xs, computed by reverse accumulation
;;
;; D/r : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define& (D/r f&)
  (val->trace
   (lambda xs ;; not lambda&
     (let ([arg-ids (map top-id xs)]
           [y& (apply (top-val f&) xs)])
       (cons->trace
        (reshape-as (top-val y&)
                    (for/list ([ind (in-indicator (top-val y&))])
                      (define s& (cons->trace ind))
                      (A/r y& arg-ids s&))))))))
