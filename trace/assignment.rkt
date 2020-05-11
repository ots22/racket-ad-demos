#lang racket

(provide make-assignment
         assignment?
         expr?
         (rename-out [id* id]
                     [expr* expr]
                     [val* val]))

;; (require "util.rkt")

;; (define (expr? e)
;;   (match e
;;     [(list 'constant _) #t]
;;     [(list 'app (? symbol? _) ..1) #t]
;;     [_ #f]))

;; (module+ test
;;   (require rackunit)
;;   (test-case "valid expressions"
;;     [check-true  (expr? '(app + a b c))]
;;     [check-true  (expr? '(constant '(1 2 3)))]
;;     [check-false (expr? '(app + 1 2 3))]
;;     [check-false (expr? '(constant 1 2 3))]
;;     [check-false (expr? '(app))]
;;     [check-false (expr? '(wrong))]
;;     [check-false (expr? 'wrong)]
;;     [check-false (expr? 1)]))

;; ;; represents a single assignment of value val, to an id (: symbol?),
;; ;; as computed by an expression expr; expr must be:
;; ;; - a value
;; ;; - a reference to another variable
;; ;;
;; (struct assignment (id expr val)
;;   #:transparent
;;   #:guard (struct-guard/c symbol? expr? any/c))

;; (define (make-assignment #:id [id (next-name)]
;;                          #:val val
;;                          #:expr [expr (list 'constant val)])
;;   (assignment id expr val))

;; ;; shorter names for assignment accessors
;; (define (id   v) (assignment-id   v))
;; (define (expr v) (assignment-expr v))
;; (define (val  v) (assignment-val  v))

;; ----------------------------------------

(require syntax/parse
         "util.rkt")
(provide all-defined-out)

(define (make-assignment #:id [id (next-name)]
                         #:val val
                         #:expr [expr val])
  (syntax-property
   (datum->syntax #f `(def ,id ,expr))
   'val
   (datum->syntax #f val)
   #t))

;; (define-syntax-class a-constant
;;   [pattern c:boolean]
;;   [pattern c:char]
;;   [pattern c:number]
;;   [pattern c:string]
;;   [pattern ()])

(define-syntax-class a-expr
  [pattern (id:id ...+)]
  [pattern c])

(define expr?
  (syntax-parser
    [e:a-expr #t]
    [_ #f]))

(define-syntax-class assignment
  #:description "an assignment"
  [pattern ((~datum def) id:id expr:a-expr)])

(define assignment?
  (syntax-parser
    [a:assignment #t]
    [_ #f]))

(define id*
  (syntax-parser
    [a:assignment (syntax->datum #'a.id)]))

(define expr*
  (syntax-parser
    [a:assignment (syntax->datum #'a.expr)]))

(define/contract (val* a)
  [-> assignment? any/c]
  (cond
    [(syntax-property a 'val) => syntax->datum]
    [else (raise-arguments-error
           'val
           "This assignment has no result (no 'val' syntax property set)"
           "id" (id* a)
           "expr" (expr* a))]))
