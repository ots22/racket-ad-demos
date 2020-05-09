#lang racket

(provide cons->trace
         trace->cons
         trace-e)

(require "trace.rkt"
         "trace-core.rkt"
         "let-traced.rkt")

(define (cons->trace x)
  (cond
    [(null? x)  null&]
    [(pair? x)  (app& cons& (cons->trace (car x)) (cons->trace (cdr x)))]
    [(trace? x) x]
    [else       (val->trace x)]))


(define (trace->cons tr)
  (cond
    [(null? (top-val tr)) null]
    [(pair? (top-val tr)) (cons (trace->cons (app& car& tr))
                                (trace->cons (app& cdr& tr)))]
    [else tr]))

(define (trace-e tr)
  (cond
    [(null? (top-val tr)) null]
    [(pair? (top-val tr)) (cons (traced (car& tr))
                                (trace-e (traced (cdr& tr))))]
    [else tr]))
                                          
