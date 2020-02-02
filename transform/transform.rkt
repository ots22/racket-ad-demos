#lang racket
(require (for-syntax racket)
         (for-syntax racket/syntax)
         "../trace/trace.rkt"
         "../trace/trace-core.rkt"
         (for-syntax "../trace/trace.rkt")
         (for-syntax "../trace/trace-core.rkt"))
        ;; (for-syntax "trace-syntax.rkt"))

(provide def
         #%module-begin
         #%top
         #%app
         #%top-interaction
         #%datum
         +
         *)


;; Take an assignment form (from trace), and produce an equivalent
;; "define" statement.  The context for the syntax is taken from ctx.
;;
;; assignment->syntax : assignment? -> syntax?
(define-for-syntax (assignment->syntax assgn ctx)
  (with-syntax ([var (datum->syntax ctx (id assgn))]
                [e   (datum->syntax
                      ctx
                      (match (expr assgn)
                        [(list 'constant x) x]
                        [(list 'app op xs ...) (cons op xs)])
                      )])
    #'(define var e)))

;; Given a trace, return syntax corresponding to a sequence of define
;; statements, and a final expression (for the last assignment in the
;; trace).  The context for the syntax is taken from ctx
;;
;; trace->assignments : trace? syntax? -> syntax?
(define-for-syntax (trace->assignments tr ctx)
  ;; apply reverse, since head of the list is the most recent assignment

  ;; return the final assignment (without wrapping in a define)
  (datum->syntax
   ctx
   (append (for/list ([assgn (reverse (cdr (trace-items tr)))])
             (assignment->syntax assgn ctx))
           (with-syntax ([(define var e) (assignment->syntax (top tr) ctx)])
             (list #'e)))))

(define-for-syntax (syntax->expression e)
  (syntax-case e ()
    [(op xs ...) (list* 'app (syntax->datum #'op) (syntax->datum #'(xs ...)))]
    [c           (list 'constant (syntax->datum #'c))]))

;; note identical patterns: consolidate into a single pattern
(define-for-syntax (syntax->assignment stx)
  (syntax-case stx (assign)
    [(assign var e) (make-assignment
                     #:id   (syntax->datum #'var)
                     #:expr (syntax->expression #'e)
                     #:val  '??)]
    [e              (make-assignment
                     #:id   (syntax->datum #'var)
                     #:expr (syntax->expression #'e)
                     #:val  '??)]))

;; given syntax for a sequence of "assignment" statements, followed by
;; a final use, produce an equivalent "trace".
;;
;; assignments->trace : syntax? -> trace?
(define-for-syntax (assignments->trace body)
  (apply make-trace (reverse (map syntax->assignment (syntax-e body)))))


(define-for-syntax (handle-assignments stx)
  ;; turn the program into a trace - we can then use the previous
  ;; machinery we built
  ;(assignments->trace (syntax-e stx) stx)
  (trace->assignments (assignments->trace stx)
                      stx))


  ;; (syntax-case stx (assign)
  ;;   [(assign a e) #'(define a e)]
  ;;   [a #'a]))

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (f args ...) body ...)
     (with-syntax ([(body* ...) (handle-assignments #'(body ...))])
       #'(define (f args ...)
           body* ...
           ))]))




;; should be an error if used outside a def form
;; (define-syntax (assign stx)
;;   (syntax-case stx ()
;;     ;; check at this point that expr has the correct form
;;     [(_ var expr) #'(define var expr)]))
