#lang racket
(require (for-syntax racket)
         (for-syntax racket/syntax)
         (for-syntax syntax/parse)
         (except-in "../trace/trace.rkt" id expr)
         ;(prefix-in assignment- (only-in "../trace/trace.rkt" id expr))
         "../trace/trace-core.rkt"
         (for-syntax
          (except-in "../trace/trace.rkt" id expr)
          ;(prefix-in assignment- (only-in "../trace/trace.rkt" id expr))
          )
         (for-syntax "../trace/trace-core.rkt")
         (for-syntax "../trace/diff.rkt"))
        ;; (for-syntax "trace-syntax.rkt"))

(provide (rename-out (define/d define))
         #%module-begin
         #%top
         #%app
         #%top-interaction
         #%datum
         list
         cons
         +
         *)

;; Take a symbol and a syntax (comprising a list of symbols).  If the
;; symbol symb is eq? to any of the ids in the syntax args, the one in
;; args is used in the define statement.
;;
;; maybe-arg->id : symbol? syntax? -> syntax?
(define-for-syntax (maybe-arg->id symb args)
  (let ([match-args (filter (λ (arg) (eq? symb (syntax->datum arg)))
                            (syntax-e args))])
    (if (null? match-args)
        (datum->syntax args symb)
        (car match-args))))


;; Take an assignment form (from trace), and produce an equivalent
;; "define" statement.  The context for the syntax is taken from ctx.
;;
;; assignment->def : assignment? syntax? syntax? -> syntax?
(define-for-syntax (assignment->def assgn args ctx)
  (with-syntax ([var (datum->syntax ctx (assignment-id assgn))]
                [e   (datum->syntax
                      ctx
                      (match (assignment-expr assgn)
                        [(list 'constant '()) ''()]
                        [(list 'constant x) x]
                        [(list 'app op xs ...)
                         (cons op (map (λ (x) (maybe-arg->id x args)) xs))]))])
    #'(define var e)))

;; Given a trace, return syntax corresponding to a sequence of define
;; statements, and a final expression (for the last assignment in the
;; trace).  The context for the syntax is taken from ctx
;;
;; trace->defs : trace? syntax? -> syntax?
(define-for-syntax (trace->defs tr args ctx)
  ;; apply reverse, since head of the list is the most recent assignment

  ;; return the final assignment (without wrapping in a define)
  (datum->syntax
   ctx
   (append (for/list ([assgn (reverse (cdr (trace-items tr)))])
             (assignment->def assgn args ctx))
           (with-syntax ([(define var e)
                          (assignment->def (top tr) args ctx)])
             (list #'e)))))

(define-for-syntax (def-expr->expr e)
  (syntax-case e ()
    [(op xs ...) (list* 'app (syntax->datum #'op) (syntax->datum #'(xs ...)))]
    [c           (list 'constant (syntax->datum #'c))]))

;; note identical patterns: consolidate into a single pattern
(define-for-syntax (def->assignment stx)
  (syntax-parse stx #:datum-literals (define)
    [(define var e) (make-assignment
                     #:id   (syntax->datum #'var)
                     #:expr (def-expr->expr #'e)
                     #:val  +nan.0)]
    [x:id           (make-assignment
                     #:expr (def-expr->expr #'(identity x))
                     #:val  +nan.0)]
    [(f xs ...)     (make-assignment
                     #:expr (def-expr->expr #'(f xs ...))
                     #:val  +nan.0)]
    [e              (make-assignment
                     #:expr (def-expr->expr #'e)
                     #:val  +nan.0)]))

;; given syntax for a sequence of "define" statements, followed by
;; a final use, produce an equivalent "trace".
;;
;; assignments->trace : syntax? -> trace?
(define-for-syntax (defs->trace body)
  (apply make-trace (reverse (map def->assignment (syntax-e body)))))


(define-for-syntax (handle-assignments args body)
  ;; turn the program into a trace - we can then use the previous
  ;; machinery we built
  (let* ([arg-ids (syntax->datum args)]

         [arg-tr
          (apply
           make-trace
           (map (λ (s) (make-assignment #:id s #:val +nan.0)) arg-ids))]

         [body-tr (defs->trace body)]

         [tr (trace-append body-tr arg-tr)]

         [seed-tr (make-trace (make-assignment #:val 1))]

         [D-tr (trace-filter-out arg-ids
                                 (D/r (trace-prune tr) arg-ids seed-tr))])

    (trace->defs D-tr args body)))


(define-syntax (define/d stx)
  (syntax-case stx ()
    [(_ (f args ...) body ...)
     (with-syntax ([(body* ...) (handle-assignments #'(args ...)
                                                    #'(body ...))])
       #'(define (f args ...)
           body* ...
           ))]))




;; should be an error if used outside a def form
;; (define-syntax (assign stx)
;;   (syntax-case stx ()
;;     ;; check at this point that expr has the correct form
;;     [(_ var expr) #'(define var expr)]))
