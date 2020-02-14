#lang racket

(provide (rename-out (partial/f& partial/f))
         A/r
         D/f
         D/r)

(require "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "cons-arithmetic.rkt")

(module+ test
  (require rackunit))

;; the i'th partial derivative of f at xs
;;
;; pderiv : integer? symbol? . (Listof trace?) -> trace?
(define (pderiv i op . xs)
  (define (err) (raise-arguments-error
                 'pderiv
                 "Can't take the requested partial derivative"
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
    [(identity) (case i
                  [(0)   (datum . 1.0)]
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
    [(eq? (id assgn) var) (datum . 1.0)]
    [(memq (id assgn) indep-ids) (datum . 0.0)]
    [else
     (match (expr assgn)
       [(list 'constant '())  (datum . ())]
       [(list 'constant c)    (datum . 0.0)]
       [(list 'app 'cons x y) (cons& (D x) (D y))]
       [(list 'app 'car ls)   (car& (D ls))]
       [(list 'app 'cdr ls)   (cdr& (D ls))]
       [(list 'app op xs ...) (let ([xs& (map I xs)])
                                (for/fold ([acc (datum . 0.0)])
                                          ([x xs]
                                           [i (in-naturals)])
                                  (define D_i_op (apply pderiv i op xs&))
                                  (+& (*& D_i_op (D x)) acc)))])]))

;; The i'th partial derivative of f, evaluated as xs, computed by
;; forward accumulation
;;
;; partial/f : integer? (trace? ... -> trace?) -> trace? ... -> trace?
(define ((partial/f i f) . xs)
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

;; The operator partial/f, for providing to the tracing lang
;;
;; D& : trace? (trace? ... -> trace?) -> trace? ... -> trace?
(define (partial/f& i f) (partial/f (top-val i) f))

;; The Jacobian of f at xs, computed by forward accumulation
;;
;; D/f : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define ((D/f f) . xs)
  (let* ([n (length xs)]
         [Di (for/list ([i (range n)]) (apply (partial/f i f) xs))])
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
     (let ([Ax (car& Aw)]
           [Ay (cdr& Aw)])
       {values (trace-append Ay Ax Aw)
               (upd-adj adjoint-terms #:key top-id x Ax y Ay)})]

    [(list 'app c_r xs) #:when (or (eq? c_r 'car) (eq? c_r 'cdr))
     (let* ([xs& (trace-get xs Aw)]
            [tr  (case c_r
                   [(car) (cons& Aw (cons-zero (cdr& xs&)))]
                   [(cdr) (cons& (cons-zero (car& xs&)) Aw)])])
       {values (trace-append tr Aw)
               (upd-adj adjoint-terms #:key top-id xs tr)})]

    [(list 'app op xs ...)
     (let ([xs& (for/list ([x xs]) (trace-get x Aw))])
       (for/fold ([tr Aw]
                  [adjoint-terms adjoint-terms])
                 ([x xs]
                  [i (in-naturals)])
         (let ([Ax (*& Aw (apply pderiv i op xs&))])
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
                  (foldl cons-add (car Aw-terms) (cdr Aw-terms))
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
     (apply list&
            (for/list ([x indep-ids])
              (trace-get (hash-ref adjoints x zero-id) tr*))))))


(define (datum->trace x) (make-trace (make-assignment #:val x)))

(define (cons->trace x)
  (cond
    [(null? x)  null&]
    [(pair? x)  (cons& (cons->trace (car x))
                       (cons->trace (cdr x)))]
    [(trace? x) x]
    [else       (datum->trace x)]))


;; The Jacobian of f at xs, computed by reverse accumulation
;;
;;
;; D/r : (trace? ... -> trace?) -> (Listof trace?) -> trace?
(define ((D/r f) . xs)
  (let* ([indep-ids (map top-id xs)]
         [result-tr (apply f xs)]
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
                  (A/r result-tr indep-ids s)))))))
