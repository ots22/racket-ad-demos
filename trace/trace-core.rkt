#lang racket

;; Implementation of the core functions and special forms in the
;; rackpropagator/traced lang.

;; Traced counterparts of several of these are provided with names
;; ending in '&', and reprovided without the '&' from trace-lang.rkt.
;; The custom #%datum and #%app for the lang are defined here as datum&
;; and app&.

(provide datum& app& define& if& and& lambda& null& not& +& -& *& /& =&
<& >& <=& >=& expt& exp& log& cons& car& cdr& null?& pair?& range&
list& trace-display&)

(require (for-syntax racket/syntax
                     syntax/parse
                     (only-in racket/function const)
                     "syntax-classes.rkt"
                     (only-in "util.rkt" syntax-reverse))
         "trace.rkt"
         "util.rkt")

;; ----------------------------------------
;; * Macros for defining functions and primitive operations
;;
;; ** Helpers

;; Given a dotted argument list, returns a single trace which is a
;; concatenation of the traces of each argument id in the correct
;; (reverse) order
(define-for-syntax (get-all-arg-traces stx)
  (syntax-parse stx
    [args:lambda-list
     #:with (rev-args ...) (syntax-reverse #'(args.args ...))
     #'(trace-append (apply trace-append (reverse args.maybe-rest-args)) ...
                     rev-args ...)]))

;; Syntax for the contract on a traced function, given its argument list
(define-for-syntax (get-args-contract stx)
  (syntax-parse stx
    [args:lambda-list
     #:with (non-empty-traces ...) (map (const #'non-empty-trace?)
                                        (syntax-e #'(args.args ...)))
     (if (null? (attribute args.maybe-rest-args))
         #'(-> non-empty-traces ... non-empty-trace?)
         #'(->* (non-empty-traces ...) ()
                #:rest (listof non-empty-trace?)
                non-empty-trace?))]))

;;
;; ** define-traced-primitive and define-traced

;; define-traced-primitive defines a traced primitive operation
;;
;; - The function defined takes traces as arguments, and returns a
;;   trace
;; - The body is expressed in terms of arguments which are plain
;;   values (not traces), and returning a plain value
;; - The operation is recorded on the trace with the name passed as
;;   the symbol f-name
;; - Dotted argument lists are not supported
;;
(define-syntax (define-traced-primitive stx)
  (syntax-parse stx
    [(_ (f:id args:id ...) f-name:quoted-symbol body:expr ...)
     (with-syntax (;; build up the trace of the arguments in reverse order
                   [all-arg-traces (get-all-arg-traces #'(args ...))]
                   ;;
                   [args-contract (get-args-contract #'(args ...))]
                   ;; a new symbol with the same name as f (so that
                   ;; the function has the expected procedure-name
                   ;; while keeping recursive definitions working as
                   ;; expected)
                   [f* (syntax->datum #'f-name.id)])
       #'(define f
           (let ()
             (define/contract (f* args ...)
               args-contract
               (let (;; shadow the actual args (which have trace annotations)
                     [result     (let ([args (top-val args)] ...) body ...)]
                     [arg-traces all-arg-traces]
                     [result-name (next-name)])
                 (trace-add
                  arg-traces
                  (make-assignment #:id   result-name
                                   #:expr (list 'app f-name (top-id args) ...)
                                   #:val  result))))
             (val->trace f*))))]))

;; define-traced defines a traced function
;;
;; - The function defined takes traces as arguments, and returns a
;;   trace
;; - The body is expressed in terms of arguments which are traces, and
;;   returning a trace
;; - The trace returned will be in terms of primitive operations (it
;;   will not show up on the trace itself
;; - This form implements the define form of trace-lang (for the most
;;   part - along with variable definition)
;; - Dotted argument lists are supported
;;
(define-syntax (define-traced stx)
  (syntax-parse stx
    [(_ (f:id args:id ... . rest-args:id*) body:expr ...)
     (with-syntax
       ([all-arg-traces (get-all-arg-traces #'(args ... . rest-args))]
        ;;
        ;; The rest args to define-traced are received as a plain list
        ;; of traces.  Convert this to a let binding form for a traced
        ;; version of the list construction itself (used to shadow the
        ;; rest-args parameter).
        ;;
        ;; Because rest-args may be null, use the 'maybe-id'
        ;; attribute, which is a list of at most one id.
        [(rest-args-binding ...)
         #'((rest-args.maybe-id
             (foldl (top-val cons&) null& (reverse rest-args.maybe-id))) ...)]
        ;;
        [args-contract (get-args-contract #'(args ... . rest-args))]
        ;;
        ;; A new symbol with the same name as f (so that the function
        ;; has the expected procedure-name while keeping recursive
        ;; definitions working as expected)
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
;; Definitions for trace-lang

(define-syntax-rule (datum& . d)
  (val->trace (#%datum . d)))

(define-syntax-rule (app& fn args ...)
  (#%app (top-val fn) args ...))

(define-syntax (define& stx)
  (syntax-parse stx
    ;; Procedure definition
    ;;
    ;; Match forms like (define (f args ... . rest-args) body ...)
    [(_ ((~var f id #:role "procedure name") . args:lambda-list)
        (~describe "procedure body" (~seq body:expr ...+)))
     #'(define-traced (f args.args ... . args.rest-args) body ...)]

    ;; Variable definition
    [(_ id:id form:expr) #'(define id form)]))

(define-syntax-rule (if& test-expr then-expr else-expr)
  (if (top-val test-expr) then-expr else-expr))

(define-syntax-rule (and& a b)
  (if& a b (val->trace #f)))

(define-syntax-rule (lambda& forms ...)
  (val->trace (lambda forms ...)))

(define null& (val->trace null))

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
