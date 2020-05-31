#lang racket

(require (for-syntax "../trace/reverse-diff.rkt"
                     "../trace/syntax-classes.rkt"
                     syntax/parse
                     racket/list
                     "../trace/trace.rkt"
                     "../trace/trace-core.rkt")
         "../cons-arithmetic/cons-arithmetic.rkt")

(module+ test
  (require rackunit))

(define-for-syntax (defs->trace body)
  (apply make-trace (reverse (syntax-e body))))

(define-syntax (return stx)
  (raise-syntax-error #f "permitted only within define/d" stx))

(define-syntax (call stx)
  (raise-syntax-error #f "permitted only within define/d" stx))

(define-syntax (call-if stx)
  (raise-syntax-error #f "permitted only within define/d" stx))

(begin-for-syntax
  (define-syntax-class final-expr
    #:literals (return)
    (pattern (return)
             #:attr final-fn #'(lambda (x) (list (identity x) identity)))
    (pattern (call f)
             #:attr final-fn #'(curry apply f))
    (pattern (call-if test then-call else-call)
             #:attr final-fn #'(if test (curry apply then-call) (curry apply else-call)))
    ))

;; artificially add definitions (to 'constants', with 'nothing'
;; vals), which will are needed for A/r* to work properly, and
;; will be bound by the enclosing function
;;
(define-syntax define/d
  (syntax-parser
    [(_ (f args ...) body ... final:final-expr)
     #:with (args* ...) (generate-temporaries #'(args ...))
     #:with (Ay Ay*) (generate-temporaries #'(Ay Ay*))
     #:with (arg-defs ...) #'((define args args*) ...)
     #:do [(define body-tr (defs->trace #'(arg-defs ... body ...)))
           (define body*-tr ((apply (A/r* body-tr) (syntax-e #'(args ...)))
                             (make-trace (make-assignment #:expr #'Ay*))))]
     #:with (body* ...)
     (reverse
      (trace-items
       (trace-filter-out (map assignment-id (trace-items body-tr))
                         body*-tr)))
     #:with last-id (assignment-id (last (syntax-e #'(body ...))))
     #:with last-id* (assignment-id (last (syntax-e #'(body* ...))))
     #'(define (f args ...)
         (let ((args* args) ...)
           arg-defs ...
           body ...
           (define fin (final.final-fn last-id))
           (list (car fin) (lambda (Ay)
                             (define Ay* ((cadr fin) Ay))
                             body* ...
                             last-id*))))]))

(module+ test
  (define/d (f a b)
    (define c (* a b))
    (return))

  (define/d (plus a b)
    (define c (+ a b))
    (return))

  (define/d (g ls1 ls2)
    (define car1 (car ls1))
    (define cdr2 (cdr ls2))
    (define cadr2 (car cdr2))
    (define p (* car1 cadr2))
    (return))

  (define/d (h a b unused)
    (define c (cons-add a b))
    (return))

  (define/d (mult x y)
    (define x2 (* x y))
    (return))

  (define/d (plus-1-mult x y)
    (define one 1)
    (define z (+ x one))
    (define w (+ y one))
    (define ls1 (cons z null))
    (define ls (cons w ls1))
    (call mult))

  (define/d (sq x)
    (define x2 (* x x))
    (return))

  (define/d (const0 x)
    (define result 0.0)
    (return))

  (define/d (get-result x n res)
    (define one 1)
    (define result (* res one))
    (return))

  (define/d (pow x n res)
    (define res* (* x res))
    (define one 1)
    (define n* (- n one))
    (define next1 (cons res* null))
    (define next2 (cons n* next1))
    (define next (cons x next2))
    (call-if (<= n* 0) get-result pow))

  ;;
  )
