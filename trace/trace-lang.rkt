#lang racket

(provide #%app
         #%module-begin
         #%top
         #%top-interaction
         (rename-out (datum #%datum))
         (rename-out (=& =))
         (rename-out (<&  <))
         (rename-out (<=& <=))
         (rename-out (>&  >))
         (rename-out (>=& >=))
         (rename-out (+& +))
         (rename-out (-& -))
         (rename-out (*& *))
         (rename-out (/& /))
         (rename-out (expt& expt))
         (rename-out (exp& exp))
         (rename-out (log& log))

         (rename-out (list& list))
         (rename-out (cons& cons))

         (rename-out (car& car))
         (rename-out (cdr& cdr))
         
         (rename-out (null?& null?))
         (rename-out (pair?& pair?))
         (rename-out (null& null))

         (rename-out (range& range))

         (rename-out (define& define))
         (rename-out (if& if))

         (rename-out (not& not))

         (rename-out (D& D))
         grad
         
         and

         trace-display

         lambda
         λ

         provide
         all-defined-out
         require
         )

(require racket/syntax
         (for-syntax racket/syntax)
         "trace.rkt"
         "util.rkt")

(module+ test
  (require rackunit))

;; ----------------------------------------
;; datum

;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     #'(make-trace (make-assignment #:val (#%datum . d)))]))


;; ----------------------------------------
;; arithmetic

;; note the branching in the patterns, for the cases where rest-args
;; is a symbol, or null (dotted argument list)
(define-syntax (define-traced-primitive stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : trace? ...
    ;; body ... : expression? ...
    [(_ (f args ... . rest-args) f-name body ...)
     (with-syntax* ([(arg-vals ...) #'((top-val args) ...)]
                    [rest-arg-vals #'(map top-val rest-args)]
                    [(arg-ids ...) #'((top-id args) ...)]
                    [rest-arg-ids  #'(map top-id rest-args)]
                    [(all-arg-ids-pat ...)
                     (if (null? (syntax->datum #'rest-args))
                         #'(arg-ids ... '())
                         #'(arg-ids ... rest-arg-ids))]
                    [arg-let (if (null? (syntax->datum #'rest-args))
                                 #'([args arg-vals] ...)
                                 #'([args arg-vals]
                                    ... [rest-args rest-arg-vals]))]
                    [(rev-args ...) (syntax-reverse #'(args ... ))]
                    [arg-traces-pat #'(append (trace-items rev-args) ...)]
                    [all-arg-traces-pat
                     (if (null? (syntax->datum #'rest-args))
                         #'(apply make-trace
                                  (remove-duplicates-before arg-traces-pat))
                         #'(apply make-trace
                                  (remove-duplicates-before
                                   (append
                                    (apply append (map trace-items
                                                       (reverse rest-args)))
                                    arg-traces-pat))))]
                    [f-val #'f])
       #'(define (f args ... . rest-args)
           (let (;; shadow the actual args (which have trace annotations)
                 [result     (let arg-let body ...)]
                 ;; trace the arguments in reverse order
                 [all-arg-traces all-arg-traces-pat]
                 [result-name (next-name)])
             (trace-add
              all-arg-traces
              ;; add rest-args here
              (make-assignment #:id   result-name
                               #:expr (list* 'app f-name all-arg-ids-pat ...)
                               #:val  result)))))]))

(define-syntax (define-traced stx)
  (syntax-case stx ()
    [(_ (f args ... . rest-args) body ...)
     (with-syntax* ([(rev-args ...) (syntax-reverse #'(args ...))]
                    [rev-args-trace
                     (if (null? (syntax->datum #'rest-args))
                         #'(trace-append rev-args ...)
                         #'(trace-append
                            (apply trace-append (reverse rest-args))
                            rev-args ...))]
                    [(rest-arg-let-binding ...)
                     (if (null? (syntax->datum #'rest-args))
                         #'()
                         #'((rest-args (foldl cons& null& (reverse rest-args)))))])
       #'(define (f args ... . rest-args)
           (let* ([arg-traces rev-args-trace]
                  rest-arg-let-binding ...
                  [result-trace (let () body ...)])
             (trace-prune
              (trace-remove-duplicates
               (trace-append result-trace arg-traces))))))]))

;; Provided define form
(define-syntax (define& stx)
  (syntax-case stx ()
    [(_ (id args ... . rest-args) body ...)
     #'(define-traced (id args ... . rest-args) body ...)]
    [(_ id expr) #'(define id expr)]))

(define-syntax (if& stx)
  (syntax-case stx ()
    [(_ test-expr then-expr else-expr)
     #'(if (top-val test-expr)
           then-expr
           else-expr)]))

(define-traced-primitive (not& a)    'not  (not a))

(define-traced-primitive (+& a b)    '+    (+ a b))
(define-traced-primitive (-& a b)    '-    (- a b))
(define-traced-primitive (*& a b)    '*    (* a b))
(define-traced-primitive (/& a b)    '/    (/ a b))
(define-traced-primitive (=& a b)    '=    (= a b))
(define-traced-primitive (<& a b)    '<    (< a b))
(define-traced-primitive (>& a b)    '>    (> a b))
(define-traced-primitive (<=& a b)   '<=   (<= a b))
(define-traced-primitive (>=& a b)   '>=   (>= a b))
(define-traced-primitive (expt& a b) 'expt (expt a b))
(define-traced-primitive (exp& a)    'exp  (exp a))
(define-traced-primitive (log& a)    'log  (log a))

(define-traced-primitive (cons& a b)   'cons  (cons a b))
(define-traced (list& . xs) xs)

(define-syntax null& (lambda (stx) #'(datum . ())))

(define-traced-primitive (car& a) 'car (car a))
(define-traced-primitive (cdr& a) 'cdr (cdr a))

(define-traced-primitive (null?& a) 'null? (null? a))
(define-traced-primitive (pair?& a) 'pair? (pair? a))

(define-traced-primitive (range& n) 'range (range n))

;; map, fold etc ...

;; ...

(module+ test
  (for* ([i (in-range -10.0 10.0 1.2)]
         [j (in-range -10.0 10.0 1.2)]
         [op (list (cons + +&)
                   (cons - -&)
                   (cons * *&)
                   (cons / /&)
                   (cons = =&)
                   (cons < <&)
                   (cons > >&)
                   (cons <= <=&)
                   (cons >= >=&))
                   ])
    (check-equal? (top-val
                   ((cdr op) (make-trace (make-assignment #:val i))
                             (make-trace (make-assignment #:val j))))
                  ((car op) i j))))


;; ----------------------------------------
;; gradients

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
       [(list 'constant c)    (datum . 0.0)]
       [(list 'ref x)         (D x)]
       [(list 'app '+ x y)    (+& (D x) (D y))]
       [(list 'app '- x y)    (-& (D x) (D y))]
       [(list 'app '* x y)    (+& (*& (D x) (I y)) (*& (I x) (D y)))]
       [(list 'app 'exp x)    (*& (D x) (exp& (I x)))]
       [(list 'app 'cons x y) (cons& (D x) (D y))]
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

            [(list 'ref x)
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


