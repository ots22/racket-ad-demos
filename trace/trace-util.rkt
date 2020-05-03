#lang racket

(provide cons->trace)

(require "trace.rkt"
         "trace-core.rkt")

(define (cons->trace x)
  (cond
    [(null? x)  null&]
    [(pair? x)  (app cons& (cons->trace (car x)) (cons->trace (cdr x)))]
    [(trace? x) x]
    [else       (val->trace x)]))
