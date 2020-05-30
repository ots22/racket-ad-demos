#lang racket

(require (for-syntax "../trace/reverse-diff.rkt"
                     "../trace/syntax-classes.rkt"
                     syntax/parse
                     racket/list
                     "../trace/trace.rkt"
                     "../trace/trace-core.rkt"))

(module+ test
  (require rackunit))

(define-for-syntax (defs->trace body)
  (apply make-trace (reverse (syntax-e body))))

;; artificially add definitions (to 'constants', with 'nothing'
;; vals), which will are needed for A/r* to work properly, and
;; will be bound by the enclosing function
;;
(define-syntax define/d
  (syntax-parser
    [(_ (f args ...) body ...)
     #:with (args* ...) (generate-temporaries #'(args ...))
     #:with (arg-defs ...) #'((define args args*) ...)
     #:do [(define body-tr (defs->trace #'(arg-defs ... body ...)))
           (define body*-tr (apply (A/r* body-tr (val->trace 1.0))
                                  (syntax-e #'(args ...))))] 
     #:with (body* ...) (reverse (trace-items (trace-append body*-tr body-tr)))
     #:with last-id (assignment-id (last (syntax-e #'(body ...))))
     #:with last-id* (assignment-id (last (syntax-e #'(body* ...))))
     #'(define (f args ...)
         (let ((args* args) ...)
           body* ...
           last-id*))]))

(module+ test
  (define/d (f a b)
    (define c (* a b)))

  (define/d (g ls1 ls2)
    (define car1 (car ls1))
    (define cdr2 (cdr ls2))
    (define cadr2 (car cdr2))
    (define p (* car1 cadr2))))
    
