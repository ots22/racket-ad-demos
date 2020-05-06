#lang racket

(provide cons->trace
         trace->cons)

(require "trace.rkt"
         "trace-core.rkt")

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
