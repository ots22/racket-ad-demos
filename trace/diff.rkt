#lang racket

(provide partial/f
         A/r
         D/f
         D/r)

(require (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "cons-arithmetic.rkt")

(module+ test
  (require rackunit
           quickcheck
           rackunit/quickcheck
           "test-util.rkt"))

;; the i'th partial derivative of op at xs
;;
;; partial : integer? symbol? -> . (Listof trace?) -> trace?
(define/match ((partial i op) . xs)
  [(0 '+ _) (val->trace 1.0)]
  [(1 '+ _) (val->trace 1.0)]
  ;;
  [(0 '- _) (val->trace 1.0)]
  [(1 '- _) (val->trace -1.0)]
  ;;
  [(0 '* xs) (second xs)]
  [(1 '* xs) (first xs)]
  ;;
  [(0 'exp xs) (app exp& (first xs))]
  ;;
  [(0 'identity _) (val->trace 1.0)]
  ;;
  [(_ _ _) (raise-arguments-error
            'partial
            "Can't take the requested partial derivative"
            "i" i
            "op" op)])

(module+ test
  (test-case "partial"
    (check-property
     (property ([x (gen-trace (choose-real -1e10 1e10))]
                [y (gen-trace (choose-real -1e10 1e10))])
               (and
                (top-val (app =& ((partial 0 '*) x y) y))
                (top-val (app =& ((partial 1 '*) x y) x))
                (raises? exn:fail:contract? (λ () ((partial 2 '*) x y))))))

    (check-property
     (property ([x (gen-trace (choose-real -1e10 1e10))]
                [y (gen-trace (choose-real -1e10 1e10))])
               (and
                (top-val (app =& ((partial 0 '+) x y) (val->trace 1.0)))
                (top-val (app =& ((partial 1 '+) x y) (val->trace 1.0)))
                (raises? exn:fail:contract? (λ () ((partial 2 '+) x y))))))

    (check-exn exn:fail:contract? (λ () ((partial 0 'unknown) 0.0)))))


;; takes an assignment, a trace, and an environment (mapping of values
;; to derivatives in the trace), and returns additional trace items
;; for computing the derivative.
;;
;; TODO/idea: pass I and D instead of tr and deriv-map
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
       [(list 'app 'cons x y) (app cons& (D x) (D y))]
       [(list 'app 'car ls)   (app car& (D ls))]
       [(list 'app 'cdr ls)   (app cdr& (D ls))]
       [(list 'app op xs ...) (let ([xs& (map I xs)])
                                (for/fold ([acc (val->trace 0.0)])
                                          ([x xs]
                                           [i (in-naturals)])
                                  (define D_i_op (apply (partial i op) xs&))
                                  (app +& (app *& D_i_op (D x)) acc)))])]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation
;;
;; partial/f : integer? (trace? ... -> trace?) -> trace? ... -> trace?
(define partial/f
  (val->trace
   (procedure-rename
    (lambda (i f)
      (val->trace
       (lambda xs
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
           (trace-prune (trace-remove-duplicates Dresult))))))
    'partial/f)))

;; The Jacobian of f at xs, computed by forward accumulation
;;
;; D/f : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define D/f
  (val->trace
   (procedure-rename
    (lambda (f)
      (val->trace
       (lambda xs
         (let* ([n (length xs)]
                [Di (for/list ([i (range n)])
                      (apply (top-val (app partial/f (val->trace i) f))
                             xs))])
           (apply (top-val list&) Di)))))
      'D/f)))

;; ----------------------------------------
;; Reverse mode AD

#|

output is seeded with 1.  (Lists seeded one element at a time?)

  (assignment '%4 '(* %0 %3) 5)
  (assignment '%3 '(+ %1 %2) 5)
  (assignment '%2 3 3)
  (assignment '%1 2 2)
  (assignment '%0 1 1)

- Adj(%4) is 1.
- Start at the top.

- When an identifier is seen, emit an expression for its
adjoint (using the sum of what has been accumulated so far).  No need
to record it anywhere globally.

- (assignment '%a '(f %b %c) val):

  - Emit expressions for %d = Adj(%a), as the sum of what has been
    seen so far

  - Emit new assignments for: (* ((D i f) %b %c) %d)
    Record these (in a map of a -> terms of Adj(a))


- Adj(%3) += %0 * Adj(%4)
- Adj(%0) += %3 * Adj(%4)

- emit the adjoints of the independent variables

- lists/conses etc?

|#

(module+ test
  (test-case "Adjoint term update helper"
    (define adj-table (hash 'a (list 1 2 3) 'b (list 4)))
    (check-equal? (upd-adj adj-table #:key identity 'c 5)
                  (hash 'a (list 1 2 3) 'b (list 4) 'c (list 5)))
    (check-equal? (upd-adj adj-table #:key identity 'c 5 'b 1 'd 6 'c 7)
                  (hash 'a (list 1 2 3)
                        'b (list 1 4)
                        'c (list 7 5)
                        'd (list 6)))))

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
     (let ([Ax (app car& Aw)]
           [Ay (app cdr& Aw)])
       {values (trace-append Ay Ax Aw)
               (upd-adj adjoint-terms #:key top-id x Ax y Ay)})]

    [(list 'app c_r xs) #:when (or (eq? c_r 'car) (eq? c_r 'cdr))
     (let* ([xs& (trace-get xs Aw)]
            [tr  (case c_r
                   [(car) (app cons& Aw (app cons-zero (app cdr& xs&)))]
                   [(cdr) (app cons& (app cons-zero (app car& xs&)) Aw)])])
       {values (trace-append tr Aw)
               (upd-adj adjoint-terms #:key top-id xs tr)})]

    [(list 'app op xs ...)
     (let ([xs& (for/list ([x xs]) (trace-get x Aw))])
       (for/fold ([tr Aw]
                  [adjoint-terms adjoint-terms])
                 ([x xs]
                  [i (in-naturals)])
         (let ([Ax (app *& Aw (apply (partial i op) xs&))])
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
                  (foldl (top-val cons-add) (car Aw-terms) (cdr Aw-terms))
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

(define (cons->trace x)
  (cond
    [(null? x)  null&]
    [(pair? x)  (app cons& (cons->trace (car x)) (cons->trace (cdr x)))]
    [(trace? x) x]
    [else       (val->trace x)]))


;; The Jacobian of f at xs, computed by reverse accumulation
;;
;;
;; D/r : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define D/r
  (val->trace
   (procedure-rename
    (lambda (f)
      (val->trace
       (lambda xs
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
                         (A/r result-tr indep-ids s)))))))))
    'D/r)))
