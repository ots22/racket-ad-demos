#lang s-exp "trace-lang.rkt"

(provide cons-zero
         cons-add)

(define (cons-zero a)
  (if (null? a) 
      null
      (if (pair? a)
          (cons (cons-zero (car a))
                (cons-zero (cdr a)))
          0.0)))

(define (cons-add a b)
  (if (and (null? a) (null? b))
      null 
      (if (not (pair? a))
          (+ a b)
          (cons (cons-add (car a) (car b))
                (cons-add (cdr a) (cdr b))))))

