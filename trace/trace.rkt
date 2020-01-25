#lang racket

(require racket/syntax
         (for-syntax racket/syntax)
         syntax/id-table)

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
         
         (rename-out (range& range))

         (rename-out (define& define))
         (rename-out (if& if))
         
         (rename-out (D& D))
         grad

         display-trace

         lambda
         λ
         )

;; ----------------------------------------
;; utils (uses set! for the counter)

(define ((flip f) a b) (f b a))

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))


(define (make-name base n)
  (format-symbol "%~a~a" base n))

(define (name-generator name [name-counter 0])
  (lambda () (make-name name (post-inc! name-counter))))

(define next-name (name-generator ""))

;; Like remove-duplicates, but the *last* occurrence of any duplicate
;; is kept instead of the first occurrence
(define (remove-duplicates-before xs)
  (reverse (remove-duplicates (reverse xs) #:key assignment-id)))

;; syntax-reverse : syntax? -> syntax?
;;
;; Giving a syntax object comprising a list, return a new syntax
;; object otherwise identical but with this list reversed.
(define-for-syntax (syntax-reverse stx)
  (datum->syntax stx (reverse (syntax->list stx))))


;; ----------------------------------------
;; assignments

(struct assignment (id expr val #|context|#) #:transparent)

(define (make-assignment #:id [id (next-name)] #:val val #:expr [expr val])
  (assignment id expr val))

(define (id      v) (assignment-id      v))
(define (expr    v) (assignment-expr    v))
(define (val     v) (assignment-val     v))
;;(define (context v) (assignment-context v))

(struct trace (items) #:transparent
  ;; #:methods gen:custom-write
  ;; [(define write-proc 
  ;;    (lambda (x port mode)
  ;;      (write (val (top x)) port)))]
  )

;; extract the trace corresponding to an id i from the trace tr, if it
;; is present, or false
(define (trace-get s tr)
  (let ([maybe-a (member s (trace-items tr) (λ (u v) (eq? u (id v))))])
    (if maybe-a
        (trace maybe-a)
        #f)))

(define (trace-add t . vs)
  (struct-copy trace t [items (append vs (trace-items t))]))

(define (trace-append . ts)
  (trace (apply append (map trace-items ts))))

(define (trace-remove-duplicates t)
  (trace (remove-duplicates-before (trace-items t))))

(define (trace-prune t)
  (define (rec t seen)
    (match (expr (top t))
      [x #:when (symbol? x) (rec (trace-get x t) (set-add seen x))]
      [(list f xs ...)
       (apply set-union 
              (map (λ (x) (rec (trace-get x t) (set-add seen x))) xs))]
      [_ seen]))
  (let ([seen (rec t (set (id (top t))))])
    (trace (filter (λ (a) (set-member? seen (id a))) (trace-items t)))))

(define (top t) (car (trace-items t)))

;; pretty print the trace
(define (display-trace t)
  (let* ([id-fmt        (map (compose ~a id) (trace-items t))]
         [expr-fmt      (map (compose ~a expr) (trace-items t))]
         [val-fmt       (map (compose ~a val) (trace-items t))]
         ;;[context-fmt   (map (compose ~a id) t)]
         [id-width      (apply max (map string-length id-fmt))]
         [expr-width    (apply max (map string-length expr-fmt))]
         [val-width     (apply max (map string-length val-fmt))]
         ;;[context-width (apply max (map string-length context-fmt))]
         )

    (display (foldl (lambda (i e v #|c|# acc)
                      (string-append
                       (format "~a | ~a | ~a~%"
                               (~a i #:min-width id-width)
                               (~a e #:min-width expr-width)
                               (~a v #:min-width val-width)
                               ;(~a c #:min-width context-width)
                               )
                       acc))
                    "" id-fmt expr-fmt val-fmt #|context-fmt|#))))

;; ----------------------------------------
;; datum

;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     #'(trace (list (make-assignment #:val (#%datum . d))))]))


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
     (with-syntax* ([(arg-vals ...) #'((val (top args)) ...)]
                    [rest-arg-vals #'(map (compose val top) rest-args)]
                    [(arg-ids ...) #'((id (top args)) ...)]
                    [rest-arg-ids  #'(map (compose id top) rest-args)]
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
                         #'(trace (remove-duplicates-before arg-traces-pat))
                         #'(trace (remove-duplicates-before
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
                               #:expr (list* f-name all-arg-ids-pat ...)
                               #:val  result)))))]))

(define-syntax (define-traced stx)
  (syntax-case stx ()
    [(_ (f args ...) body ...)
     (with-syntax ([(rev-args ...) (syntax-reverse #'(args ...))])
       #'(define (f args ...)
           (let ([arg-traces (trace (append (trace-items rev-args) ...))]
                 [result-trace (let () body ...)])
             (trace-remove-duplicates 
              (trace-append result-trace arg-traces)))))]))

(define-syntax (define& stx)
  (syntax-case stx ()
    [(_ (id args ...) body ...)
     #'(define-traced (id args ...) body ...)]
    [(_ id expr) #'(define id expr)]))

(define-syntax (if& stx)
  (syntax-case stx ()
    [(_ test-expr then-expr else-expr)
     #'(if (val (top test-expr))
           then-expr
           else-expr)]))

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

(define-traced-primitive (list& . items) 'list  (apply list items))
(define-traced-primitive (cons& a b)   'cons  (cons a b))

(define-traced-primitive (range& n) 'range (range n))

;; map, fold etc ...

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
       [c #:when (number? c) (datum . 0.0)]
       [x #:when (symbol? x) (D x)]
       [(list '+ x y) (+& (D x) (D y))]
       [(list '- x y) (-& (D x) (D y))]
       [(list '* x y) (+& (*& (D x) (I y)) (*& (I x) (D y)))]
       [(list 'exp x) (*& (D x) (exp& (I x)))]
       [(list 'list xs ...) (apply list& (map D xs))]
       ;; add more cases here
       ;; ...
       )]))

(define ((D i f) . xs)
  (let* ([var       (id (top (list-ref xs i)))]
         [indep-ids (map (compose id top) xs)]
         [result    (apply f xs)]
         [result-id (id (top result))])
    (define-values (Dresult _)
      (for/fold ([tr result]
                 [deriv-map (hash)])
                ([a (reverse (trace-items result))])
        (let* ([Da (deriv a var indep-ids tr deriv-map)])
          {values
           (trace-append Da tr)
           (hash-set deriv-map (id a) (id (top Da)))})))
    (trace-prune (trace-remove-duplicates Dresult))))

(define ((grad f) . xs)
  (let* ([n (length xs)]
         [Di (for/list ([i (range n)]) (apply (D i f) xs))])
    (apply list& Di)))

(define (D& i f) (D (val (top i)) f))
  
