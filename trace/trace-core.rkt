#lang racket

(provide (all-defined-out))

(require racket/syntax
         quickcheck
         (for-syntax racket/syntax)
         "trace.rkt"
         "util.rkt")

(module+ test
  (require rackunit
           rackunit/quickcheck))

;; ----------------------------------------
;; datum

;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     #'(make-trace (make-assignment #:val (#%datum . d)))]))

(module+ test
  (test-case "datum"
    (define d (datum . 1.0))
    (check-equal? (top-val d) 1.0)
    (check-equal? (top-expr d) '(constant 1.0))
    (check-equal? (length (trace-items d)) 1)))

(define-syntax-rule (app fn args ...)
  (#%app (top-val fn) args ...))

;; ----------------------------------------

(define (val->trace v)
  (make-trace (make-assignment #:val v)))

;; take a quickcheck generator and produce another that generates the
;; same values, but as traces
(define (gen-trace gen)
  (bind-generators ([v gen])
                   (val->trace v)))

(define trace-display& (val->trace trace-display))

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
                    [arg-traces-pat #'(trace-append rev-args ...)]
                    [all-arg-traces-pat
                     (if (null? (syntax->datum #'rest-args))
                         #'arg-traces-pat
                         #'(trace-append
                            (apply trace-append (reverse rest-args))
                            arg-traces-pat))])
       #'(define f
           (val->trace
            (procedure-rename
             (lambda (args ... . rest-args)
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
                                   #:val  result))))
             f-name))))]))

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
                         #'((rest-args (foldl (top-val cons&)
                                              null&
                                              (reverse rest-args)))))])
       #'(define f
           (val->trace
            (procedure-rename
             (lambda (args ... . rest-args)
               (let* ([arg-traces rev-args-trace]
                      rest-arg-let-binding ...
                      [result-trace (let () body ...)])
                 (trace-prune
                  (trace-append result-trace arg-traces))))
             'f))))]))

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

(define-syntax null& (λ (stx) #'(datum . ())))

(define-traced-primitive (car& a) 'car (car a))
(define-traced-primitive (cdr& a) 'cdr (cdr a))

(define-traced-primitive (null?& a) 'null? (null? a))
(define-traced-primitive (pair?& a) 'pair? (pair? a))

(define-traced-primitive (range& n) 'range (range n))

(define-syntax-rule (lambda& forms ...)
  (val->trace (lambda forms ...)))

;; map, fold etc ...

;; ...

(module+ test
  ;; property-based tests to check that the traced operators produce
  ;; the same result as their counterparts
  (test-case "traced ops"
    (for ([bin-op (list (cons + +&)
                        (cons - -&)
                        (cons * *&)
                        (cons / /&)
                        (cons expt expt&)
                        (cons = =&)
                        (cons < <&)
                        (cons > >&)
                        (cons <= <=&)
                        (cons >= >=&)
                        (cons cons cons&))])
      (with-check-info (['operation-plain (car bin-op)]
                        ['operation-trace (cdr bin-op)])
        (check-property
         (property ([x arbitrary-real]
                    [y arbitrary-real])
                   (equal?
                    (top-val ((top-val (cdr bin-op))
                              (val->trace x) (val->trace y)))
                    ((car bin-op) x y))))))

    (check-property
     (property ([x arbitrary-real])
               (= (top-val (app exp& (val->trace x)))
                  (exp x))))

    (check-property
     (property ([x (choose-real 0 1e+8)])
               (= (top-val (app log& (val->trace x)))
                  (log x))))

    (check-property
     (property ([xs (arbitrary-pair arbitrary-real
                                    (arbitrary-list arbitrary-real))])
               (let ([xs& (apply (top-val list&) (map val->trace xs))])
                 (and (equal? (top-val (app car& xs&)) (car xs))
                      (equal? (top-val (app cdr& xs&)) (cdr xs))))))

    (check-equal? (top-val (app not& (val->trace #f))) #t)
    (check-equal? (top-val (app not& (val->trace #t))) #f)

    (check-equal? (top-val (app null?& (val->trace null))) #t)
    (check-equal? (top-val (app pair?& (app list&
                                            (val->trace 'a) (val->trace 'b))))
                  #t)
    (check-equal? (top-val (app pair?& null&)) #f)
    (check-equal? (top-val (app pair?& (val->trace 1.0))) #f)

    )) ; test-case, module+
