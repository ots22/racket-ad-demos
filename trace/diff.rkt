#lang racket

(provide (rename-out (D/f& D/f))
         grad/f)

(require "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "cons-arithmetic.rkt")

(module+ test
  (require rackunit))

;; the i'th partial derivative of f at xs
;; pderiv : integer? symbol? . (Listof trace?) -> trace?
(define (pderiv i op . xs)
  (define (err) (raise-arguments-error
                 'pderiv
                 "can't take requested partial derivative"
                 "i" i
                 "op" op))
  (case op
    [(+)        (case i
                  [(0 1) (datum . 1.0)]
                  [else  (err)])]
    [(-)        (case i
                  [(0)   (datum . 1.0)]
                  [(1)   (datum . -1.0)]
                  [else (err)])]
    [(*)        (case i
                  [(0)   (cadr xs)]
                  [(1)   (car  xs)]
                  [else  (err)])]
    [(exp)      (case i
                  [(0)   (exp& (car xs))]
                  [else  (err)])]

    ;; add more cases here...

    [else (err)]

    ))

(module+ test
  (test-case "pderiv"
    (check-true (top-val (=& (pderiv 0 '* (datum . 1) (datum . 2))
                             (datum . 2))))))

#|

have lines like

(assignment '%2 '(+ %0 %1) 3)

what to do with them?

Forward mode:
  (assignment '%4 '(* %0 %3) 5)
  (assignment '%3 '(+ %1 %2) 5)
  (assignment '%2 3 3)
  (assignment '%1 2 2)
  (assignment '%0 1 1)

- know %0 and %1 (say) are inputs (to the program)

- %3 is not an input (it is a constant): can't tell this from the
above, but know it from the function signature, which we have access
to when taking gradient.

Start, D[%0] = 1 (then D[%1] = 1, etc for each of the inputs in turn)

Now, work "up" the list (in the order it's in as above, anyway), and
apply the rules (to the 'expression' column, %nthe
expression)

a?b means a if it is known, or b:

D[%0] = 1
D[%n] =
  match on(E[%n]):
   c             : 0
   %a  (a =/= 0) : D[%a]
   (+ %a %b)     : (+ D[%a] D[%b])
   (* %a %b)     : (+ (* D[%a]*V[%b]) (* V[%a]*D[%b]))
etc.

Notice that we can make this reduction immediately, since if %a
appears in E[%b], then an assignment for %a must have been seen.  If
it hasn't, it is in part of the chain that lead to the computation of
an argument.  Could discard this on entry (only keep the tops
of the arg stacks) - would this always work (nested gradients)?

Can apply this in order:

;; make a trace a sequence, with

(for/fold ([D (hash )])
          ([a (in-trace t)])
  (hash-set (id a) (cons (val a) (deriv (expr a) D))))

where deriv is a function like the one above, taking an expression and
environment and returning derivatives.

;; note the for/fold construct in Racket

;; (for best way to connect to later part - compile time code
transform - probably want to actually keep an ordered list of
assignments, or some opaque type)

;; (can we do this without an explicit hash table, using the environment?)

;; as e.g.

- read these bottom to top as "let %n in (let %(n+1) in ...)"

- 'eval' to do this, e.g.
  - "let n = expr in ..." becomes "let n = expr in (let Dn = D[expr] in ...)"

- could rewrite as let terms and use (eval expr ns) to evaluate in a
new empty namespace.

- all of this is a clue => do it at compile time instead.

- simplest language only lets us do this. Next simplest has
jumps/calls, and is Turing complete.

|#

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
    [(eq? (id assgn) var) (datum . 1.0)]
    [(memq (id assgn) indep-ids) (datum . 0.0)]
    [else
     (match (expr assgn)
       [(list 'constant c)    (datum . 0.0)]
       [(list 'app 'cons x y) (cons& (D x) (D y))]
       [(list 'app 'car ls)   (car& (D ls))]
       [(list 'app 'cdr ls)   (cdr& (D ls))]
       [(list 'app op xs ...) (let ([xs& (map I xs)])
                                (for/fold ([acc (datum . 0.0)])
                                          ([i (in-range (length xs))]
                                           [x xs])
                                  (define D_i_op (apply pderiv i op xs&))
                                  (+& (*& D_i_op (D x)) acc)))])]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation 
;;
;; D/f : integer? (trace? ... -> trace?) -> trace? ... -> trace?
(define ((D/f i f) . xs)
  (let* ([var       (top-id (list-ref xs i))]
         [indep-ids (map top-id xs)]
         [result    (apply f xs)])
    (define-values (Dresult _)
      (for/fold ([tr result]
                 [deriv-map (hash)])
                ([a (reverse (trace-items result))])
        (let* ([Da (deriv/f a var indep-ids tr deriv-map)])
          {values
           (trace-append Da tr)
           (hash-set deriv-map (id a) (top-id Da))})))
    (trace-prune (trace-remove-duplicates Dresult))))

;; The operator D/f, for providing to the tracing lang
;;
;; D& : trace? (trace? ... -> trace?) -> trace? ... -> trace?
(define (D/f& i f) (D/f (top-val i) f))

;; The gradient of f at xs, computed by forward accumulation
;;
;; grad : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define ((grad/f f) . xs)
  (let* ([n (length xs)]
         [Di (for/list ([i (range n)]) (apply (D/f i f) xs))])
    (apply list& Di)))

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

(define (upd-adj adj-table #:key [key-fn top-id] . keys-and-traces)
  (for/fold ([adj-table* adj-table])
            ([kt (chunk2 keys-and-traces)])
    (let ([k (car kt)] [t (cadr kt)])
      (hash-list-append adj-table* k (list (key-fn t))))))

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

;; compute Adj[(id assgn)]
;; returns:
;;   - additional trace items needed to compute the adjoint 
;;   - an updated map of terms comprising adjoints
;;
;;
;; adjoint/r : assignment? symbol? trace? (HashTable symbol? (Listof symbol?))
;;             (HashTable symbol? symbol?)
;;           -> (Values trace? (HashTable symbol? (Listof symbol?)))
(define (adjoint/r assgn var tr adjoint-terms adjoint-map)
  (cond
    [(eq? (id assgn) var) (datum . 1.0)] ;; is this needed?
    [else
     (match (expr assgn)
       [(list 'constant c)    {values tr adjoint-terms}]
       ;[(list 'app 'cons x y) {values ? (upd-adj (upd-adj ))}]
       ;[(list 'app 'cdr ls)   {values ? ?}]
       ;; [(list 'app op xs ...) 
       ;;  {values
         
       ;;   }]
    
    
    )]))

(define ((D/r i f) . xs)
  (let* ([indep-ids (map top-id xs)]
         [result    (apply f xs)] ;; i'th element of result list
         [seed      (top-id result)]
         [result*   (trace-add result (make-assignment #:val 1.0))])

    (define-values (D_j _ adjoint-map)
      (for/fold ([tr result*]
                 ;; terms (Listof ids) contributing to the adjoint of the key
                 [adjoint-terms (hash seed (list (top-id result*)))]
                 ;; the adjoints of each id seen
                 [adjoint-map (hash)])
                ([a (trace-items result)])
        
        ;; helper: get the current trace of x
        (define (I x) (trace-get x tr))

        ;; firstly, calculate the adjoint of the current term, a, and
        ;; put this at the head of the trace tr, as tr*.
        (let* (;; list of traces of the terms that sum to Adj (id a)
               [adj-terms (map I (hash-ref adjoint-terms (id a)))]
               ;; the trace of adj-a (summed) - adj-terms can't be empty
               [tr* (trace-append
                     (foldl +& (car adj-terms) (cdr adj-terms)) tr)]
               [adjoint-map* (hash-set adjoint-map (id a) (top-id tr*))])

          (match (expr a)
            [(list 'constant c)
               {values tr*
                       adjoint-terms
                       adjoint-map*}]

            [(list 'app '+ x y)
             {values tr*
                     (upd-adj (upd-adj adjoint-terms x tr*) y tr*)
                     adjoint-map*}]

            [(list 'app '* x y) 
             (let ([Ax (*& (I y) tr*)]
                   [Ay (*& (I x) tr*)])
               {values (trace-append Ay Ax)
                       (upd-adj (upd-adj adjoint-terms x Ax) y Ay)
                       adjoint-map*})]

            [(list 'app 'exp x) 
             (let ([Ax (*& (exp& (I x)) tr*)])
               {values Ax
                       (upd-adj adjoint-terms x Ax)
                       adjoint-map*})]))))

    (map (λ (k) (hash-ref adjoint-map k 0.0)) indep-ids)

    (let* ([D_j* (trace-add D_j (make-assignment #:val 0.0))]
           [zero_id (top-id D_j*)])
      (apply list& (for/list ([k indep-ids])
                     (trace-get (hash-ref adjoint-map k zero_id) D_j*))))))
