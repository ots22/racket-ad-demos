#lang racket

;; Implementation of the core functions and special forms in the
;; rackpropagator/traced lang.

;; Traced counterparts of several of these are provided with names
;; ending in '&', and reprovided without the '&' from trace-lang.rkt.
;; The custom #%datum and #%app for the lang are defined here as datum
;; and app.

(provide datum
         app
         define&
         if&
         lambda&
         null&
         not&
         +&
         -&
         *&
         /&
         =&
         <&
         >&
         <=&
         >=&
         expt&
         exp&
         log&
         cons&
         car&
         cdr&
         null?&
         pair?&
         range&
         list&
         trace-display&)

(require racket/syntax
         quickcheck
         (for-syntax racket/syntax
                     syntax/parse
                     (only-in racket/function const))
         "trace.rkt"
         "util.rkt")

(module+ test
  (require rackunit
           rackunit/quickcheck))

;; define-traced and define-traced-primitive are helper macros that
;; emit definition of functions that consume and emit traces.

;; ...


;; Helper for define-traced-primitive and define-traced.
;;
;; Given a dotted argument list, returns a single trace which is a
;; concatenation of the traces of each argument id in the correct
;; (reverse) order
;;
(define-for-syntax (get-all-arg-traces lambda-list)
  (syntax-case lambda-list ()
    [(args ...)
     (with-syntax ([(rev-args ...) (syntax-reverse #'(args ...))])
       #'(trace-append rev-args ...))]

    [(args ... . rest-args)
     (with-syntax ([(rev-args ...) (syntax-reverse #'(args ...))])
       #'(trace-append (apply trace-append (reverse rest-args))
                       rev-args ...))]))

;; (define-for-syntax (get-all-arg-ids stx)
;;   (syntax-case stx ()
;;     [(args ...) #'(arg-ids ... '())]
;;     [(args ... . rest-args) #'(ar)

(define-syntax (define-traced-primitive stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : trace? ...
    ;; body ... : expression? ...
    [(_ (f args ... . rest-args) f-name body ...)
     (with-syntax*
       ([(arg-vals ...) #'((top-val args) ...)]
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
        [all-arg-traces (get-all-arg-traces #'(args ... . rest-args))])
       #'(define f
           (val->trace
            (procedure-rename
             (lambda (args ... . rest-args)
               (let (;; shadow the actual args (which have trace annotations)
                     [result     (let arg-let body ...)]
                     ;; trace the arguments in reverse order
                     [arg-traces all-arg-traces]
                     [result-name (next-name)])
                 (trace-add
                  arg-traces
                  ;; add rest-args here
                  (make-assignment #:id   result-name
                                   #:expr (list* 'app f-name all-arg-ids-pat ...)
                                   #:val  result))))
             f-name))))]))


;; The rest args to define-traced are received as a plain list of
;; traces. This function returns a let binding form for a traced
;; version of the list construction itself (used to shadow the
;; rest-args parameter).
;;
;; Because rest-args is optional, a list of zero or one such bindings
;; is returned, in fact.
;;
(define-for-syntax (get-rest-args-binding rest-args)
  (syntax-case rest-args ()
    [() #'()]
    [rest-args #'((rest-args (foldl (top-val cons&) null& (reverse rest-args))))]))

(define-for-syntax (get-args-contract lambda-list)
  (syntax-case lambda-list ()
    [(args ...)
     (with-syntax ([(non-empty-traces ...)
                    (map (const #'non-empty-trace?)
                         (syntax-e #'(args ...)))])
       #'(-> non-empty-traces ... non-empty-trace?))]

    [(args ... . rest-args)
     (with-syntax ([(non-empty-traces ...)
                    (map (const #'non-empty-trace?)
                         (syntax-e #'(args ...)))])
       #'(->* (non-empty-traces ...)
              ()
              #:rest (listof non-empty-trace?)
              non-empty-trace?))]))

(define-syntax (define-traced stx)
  (syntax-case stx ()
    [(_ (f args ... . rest-args) body ...)
     (with-syntax
       ([all-arg-traces (get-all-arg-traces #'(args ... . rest-args))]
        [(rest-args-binding ...) (get-rest-args-binding #'rest-args)]
        [args-contract (get-args-contract #'(args ... . rest-args))]
        ;; a new symbol with the same name as f (so that recursive
        ;; definitions work correctly)
        [f* (syntax->datum #'f)])
       #'(define f (let ()
                     (define/contract (f* args ... . rest-args)
                       args-contract
                       (let* ([arg-traces all-arg-traces]
                              rest-args-binding ...
                              [result-trace (let () body ...)])
                         (trace-prune
                          (trace-append result-trace arg-traces))))
                     (val->trace f*))))]))

;; ----------------------------------------

(define-syntax-rule (datum . d)
  (val->trace (#%datum . d)))

(define-syntax-rule (app fn args ...)
  (#%app (top-val fn) args ...))

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

(define-syntax-rule (lambda& forms ...)
  (val->trace (lambda forms ...)))

(define-syntax null& (λ (stx) #'(datum . ())))

(define-traced-primitive (not& a)    'not   (not a))
(define-traced-primitive (+& a b)    '+     (+ a b))
(define-traced-primitive (-& a b)    '-     (- a b))
(define-traced-primitive (*& a b)    '*     (* a b))
(define-traced-primitive (/& a b)    '/     (/ a b))
(define-traced-primitive (=& a b)    '=     (= a b))
(define-traced-primitive (<& a b)    '<     (< a b))
(define-traced-primitive (>& a b)    '>     (> a b))
(define-traced-primitive (<=& a b)   '<=    (<= a b))
(define-traced-primitive (>=& a b)   '>=    (>= a b))
(define-traced-primitive (expt& a b) 'expt  (expt a b))
(define-traced-primitive (exp& a)    'exp   (exp a))
(define-traced-primitive (log& a)    'log   (log a))
(define-traced-primitive (cons& a b) 'cons  (cons a b))
(define-traced-primitive (car& a)    'car   (car a))
(define-traced-primitive (cdr& a)    'cdr   (cdr a))
(define-traced-primitive (null?& a)  'null? (null? a))
(define-traced-primitive (pair?& a)  'pair? (pair? a))
(define-traced-primitive (range& n)  'range (range n))

(define-traced (list& . xs) xs)

(define trace-display& (val->trace trace-display))

;; ----------------------------------------

(module+ test
  (test-case "datum"
    (define d (datum . 1.0))
    (check-equal? (top-val d) 1.0)
    (check-equal? (top-expr d) '(constant 1.0))
    (check-equal? (length (trace-items d)) 1))

  (test-case "app"
    (check-equal? (top-val (app not& (val->trace #t))) #f))

  (test-case "definitions"
    (define-traced (f) (val->trace 1))
    (check-equal? (top-val (app f)) 1)

    (define-traced (g x . xs) (val->trace 1))
    (check-equal? (top-val (app g (val->trace 1))) 1)

    (define-traced (h x) x)
    ;; wrong number of arguments
    (check-exn exn:fail? (λ () (app h)))
    ;; invalid empty trace
    (check-exn exn:fail? (λ () (app h (trace-append)))))


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
