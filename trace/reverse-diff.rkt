#lang racket

(provide A/r*
         A/r*-memo
         A/r
         A/r-memo)

(require memoize
         "util.rkt"
          "trace.rkt"
          "trace-core.rkt"
          "primitive-partial.rkt")

;; update-tr+terms :
;;              trace? (HashTable symbol? (Listof trace?)) [symbol? trace?] ...
;;   -> {Values trace? (HashTable symbol? (Listof trace?))}
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
    [(list 'cons-zero xs)
     (update-tr+terms Aw& A-terms
                      xs (traced (cons-zero& (trace-of xs))))]
    [(list 'cons-add x y)
     (update-tr+terms Aw& A-terms
                      x (traced Aw&)
                      y (traced Aw&))]
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

;; Ay& : the initial 'seed' (adjoint of y), which must be a trace of a pair with
;;       the same shape as y.  This should almost certainly have a '1' in one
;;       position, and '0's elsewhere.
;; y&  : the trace of the result of calling the function to be differentiated
;; xs  : the traces of the variables with respect to which we are
;;       differentiating
;;
;; A/r : trace? trace? -> (Listof trace?) -> trace?
(define& (A/r* y& Ay&)
  (val->trace
   (λ xs
     (define-values (tr _ adjoints)
       (for/fold ([tr (trace-append Ay& y&)]
                  ;; terms (Listof ids) contributing to the adjoint
                  [adjoint-terms (hash (top-id y&) (list (top-id Ay&)))]
                  ;; the adjoints of each id seen
                  [adjoints (hash)])
                 ;; iterate through each assignment, starting with the
                 ;; most recent
                 ([w-assgn (trace-items y&)])
         (define/contract Aw-terms (non-empty-listof trace?)
           (for/list ([k (hash-ref adjoint-terms (id w-assgn))])
             (trace-get k tr)))

         (define Aw&
           (match-let ([(cons a as) Aw-terms])
             (trace-append
              (foldl (top-val cons-add&) a as)
              tr)))
         ;; tr* : the assignments needed for the adjoints of the ids in the RHS
         ;;       of w-assgn (appended to tr)
         ;; adjoint-terms* : the updated map of id -> adjoint id
         (define-values (tr* adjoint-terms*)
           (A-primitive (expr w-assgn) Aw& adjoint-terms))

         {values tr*
                 adjoint-terms*
                 (hash-set adjoints (id w-assgn) (top-id Aw&))}))

     (let* ([tr* (trace-append (traced (cons-zero& Ay&)) tr)]
            [zero-id (top-id tr*)])
       (trace-prune
        (cons->trace
         (for/list ([x (map top-id xs)])
           (trace-get (hash-ref adjoints x zero-id) tr*))))))))

;; ----------------------------------------

(define& (A/r*-memo y& Ay&)
  (val->trace
   (λ xs
     ;; A : symbol? -> trace?
     (define/memo (A x)
       (define A& (val->trace A))
       (define x& (trace-get x y&))
       (if (eq? x (top-id y&))
           Ay&
           (for*/fold ([acc& (traced (cons-zero& x&))])
                      ([w& (depends-on x y&)]
                       [term (uses-in x (top-expr w&))])
             (define Aw& (A (top-id w&)))
             (match term
               [(list 'cons (list a) b) (traced (car& Aw&))]
               [(list 'cons a (list b)) (traced (cdr& Aw&))]
               [(list 'car (list a))
                (traced (cons& Aw& (cons-zero& (cdr& x&))))]
               [(list 'cdr (list a))
                (traced (cons& (cons-zero& (car& x&)) Aw&))]
               [(list 'cons-zero (list a)) (traced (cons-zero& x&))]
               [(list 'cons-add (list a) b) (traced Aw&)]
               [(list 'cons-add a (list b)) (traced Aw&)]
               [(list op a ... (list b) c ...)
                (let ([d-op (apply (partial (length a) op)
                                   (map (curryr trace-get y&)
                                        (append a (list b) c)))])
                  (traced (+& acc& (*& Aw& d-op))))]
               ;; we know there *is* a use, so an error if we get here
               ))))
     (cons->trace (map (compose1 A top-id) xs)))))

;; ----------------------------------------

(define& (A/r f& Ay&)
  (val->trace
   (λ xs
     (let ([y& (apply (top-val f&) xs)])
       (apply (top-val (traced (A/r* y& Ay&))) xs)))))

(define& (A/r-memo f& Ay&)
  (val->trace
   (λ xs
     (let ([y& (apply (top-val f&) xs)])
       (apply (top-val (traced (A/r*-memo y& Ay&))) xs)))))
