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
         
         (rename-out (define& define))
         (rename-out (if& if))
         
         display-trace
         grad
         )


;; based on nocell

;; ----------------------------------------
;; utils (uses set! for the counter)

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (name-generator name [name-counter 0])
  (lambda () (make-name name (post-inc! name-counter))))

(define next-name (name-generator ""))

(define (make-name base n)
  (format-symbol "%~a~a" base n))

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

(define (make-assignment #:id id #:expr expr #:val val)
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

(define (trace-add t . vs)
  (struct-copy trace t
               [items (append vs (trace-items t))]))

(define (trace-append . ts)
  (trace (apply append (map trace-items ts))))

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
     #'(trace (list (assignment (next-name) (#%datum . d) (#%datum . d))))]))


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
                         #'(trace (remove-duplicates-before
                                   arg-traces-pat))
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
             (trace
              (remove-duplicates-before
               (trace-items
                (trace-add
                 (trace-append result-trace arg-traces)
                 (make-assignment #:id (next-name)
                                  #:expr (id (top result-trace))
                                  #:val (val (top result-trace))))
                ))))))]))

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

;; map, fold etc ...

;; ----------------------------------------
;; gradients

;; extract the value from the trace tr if present
(define (trace-get i tr)
  (let ([maybe-a (member i (trace-items tr) (λ (u v) (eq? u (id v))))])
    (and maybe-a (car maybe-a))))

(provide trace-get)



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

;; takes an assignment, and an 'environment' (pairs of values and
;; derivatives), and takes this to a derivative.

;; deriv? : assignment? (Hash symbol? (number? . number?)) -> number?
(define (deriv a env)
  (define (get x) (hash-ref env x 0.0))
  (define (val x) (car x))
  (define (D x) (cdr x))
  (match a
    [c #:when number? 0.0]
    [x #:when symbol? (D (get x))
    [(+ x y) (+ (D (get x)) (D (get y)))]
    [(* x y) (* ()]))

;; really want to build up the gradient calculation on a stack too,
;; making second derivatives possible
(define ((grad f) . xs)
  (let* ([indep-ids (map (compose id top) xs)]
         [result (apply f xs)]
         [result-id (id (top (apply f xs)))]
         )
    (map (λ (i) (trace-get i result)) indep-ids)
    ))
