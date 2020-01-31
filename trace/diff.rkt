#lang racket

(provide (rename-out (D& D))
         grad)

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
    [(identity) (case i
                  [(0)   (datum . 1.0)]
                  [else  (err)]
                  )]
    [(+)        (case i
                  [(0 1) (datum . 1.0)]
                  [else  (err)])]
    [(*)        (case i
                  [(0)   (cadr xs)]
                  [(1)   (car  xs)]
                  [else  (err)])]
    [(exp)      (case i
                  [(0)   (exp& (car xs))]
                  [else  (err)])]))

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
;; deriv : assignment? symbol? (Listof symbol?) trace?
;;     (HashTable symbol? symbol?) -> trace?
(define (deriv assgn var indep-ids tr deriv-map)
  ;; the value of the identifier x
  (define (I x) (trace-get x tr))
  ;; the value of the identifier which is the derivative of identifier x
  (define (D x) (I (hash-ref deriv-map x)))
  (cond
    [(eq? (id assgn) var) (datum . 1.0)]
    [(memq (id assgn) indep-ids) (datum . 0.0)]
    [else
     (match (expr assgn)
       [(list 'constant c)      (datum . 0.0)]
       [(list 'app 'identity x) (D x)]
       [(list 'app '+ x y)      (+& (D x) (D y))]
       [(list 'app '- x y)      (-& (D x) (D y))]
       [(list 'app '* x y)      (+& (*& (D x) (I y)) (*& (I x) (D y)))]
       [(list 'app 'exp x)      (*& (D x) (exp& (I x)))]
       [(list 'app 'cons x y)   (cons& (D x) (D y))]
       ;; add more cases here
       ;; ...
       )]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation 
;;
;; D : integer? (trace? ... -> trace?) -> trace? ... -> trace?
(define ((D i f) . xs)
  (let* ([var       (top-id (list-ref xs i))]
         [indep-ids (map top-id xs)]
         [result    (apply f xs)])
    (define-values (Dresult _)
      (for/fold ([tr result]
                 [deriv-map (hash)])
                ([a (reverse (trace-items result))])
        (let* ([Da (deriv a var indep-ids tr deriv-map)])
          {values
           (trace-append Da tr)
           (hash-set deriv-map (id a) (top-id Da))})))
    (trace-prune (trace-remove-duplicates Dresult))))

;; The operator D, for providing to the tracing lang
;;
;; D& : trace? (trace? ... -> trace?) -> trace? ... -> trace?
(define (D& i f) (D (top-val i) f))

;; The gradient of f at xs
;;
;; grad : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define ((grad f) . xs)
  (let* ([n (length xs)]
         [Di (for/list ([i (range n)]) (apply (D i f) xs))])
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

(define (upd-adj adj-table k t)
  (hash-list-append adj-table k (list (top-id t))))

(define ((D_r i f) . xs)
  (let* ([indep-ids (map top-id xs)]
         [result    (apply f xs)] ;; i'th element of result list
         [seed      (top-id result)]
         [result*   (trace-add result (make-assignment #:val 1.0))])

    (define-values (D_j _ adjoints)
      (for/fold ([tr result*]
                 ;; terms (Listof ids) contributing to the adjoint of the key
                 [adjoint-map (hash seed (list (top-id result*)))]
                 ;; the adjoints of each id seen
                 [adjoints (hash)])
                ([a (trace-items result)])
        
        ;; helper: get the current trace of x
        (define (I x) (trace-get x tr))

        ;; firstly, calculate the adjoint of the current term, a, and
        ;; put this at the head of the trace tr, as tr*.
        (let* (;; list of traces of the terms that sum to Adj (id a)
               [adj-terms (map I (hash-ref adjoint-map (id a)))]
               ;; the trace of adj-a (summed) - adj-terms can't be empty
               [tr* (trace-append
                     (foldl +& (car adj-terms) (cdr adj-terms)) tr)]
               [adjoints* (hash-set adjoints (id a) (top-id tr*))])

          (match (expr a)
            [(list 'constant c)
               {values tr*
                       adjoint-map
                       adjoints*}]

            [(list 'app 'identity x)
               {values tr*
                       (upd-adj adjoint-map x tr*)
                       adjoints*}]

            [(list 'app '+ x y)
             {values tr*
                     (upd-adj (upd-adj adjoint-map x tr*) y tr*)
                     adjoints*}]

            [(list 'app '* x y) 
             (let ([Ax (*& (I y) tr*)]
                   [Ay (*& (I x) tr*)])
               {values (trace-append Ay Ax)
                       (upd-adj (upd-adj adjoint-map x Ax) y Ay)
                       adjoints*})]

            [(list 'app 'exp x) 
             (let ([Ax (*& (exp& (I x)) tr*)])
               {values Ax
                       (upd-adj adjoint-map x Ax)
                       adjoints*})]))))

    (map (λ (k) (hash-ref adjoints k 0.0)) indep-ids)

    (let* ([D_j* (trace-add D_j (make-assignment #:val 0.0))]
           [zero_id (top-id D_j*)])
      (apply list& (for/list ([k indep-ids])
                     (trace-get (hash-ref adjoints k zero_id) D_j*))))))
