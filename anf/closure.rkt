#lang racket

(provide closure?
         make-closure
         apply-closure
         closure-fn
         closure-zero
         zero)

(require "../cons-arithmetic/cons-arithmetic.rkt")

(struct closure (fn zero))

(define (make-closure fn zero) (closure fn zero))

(define (apply-closure f . xs)
  (if (closure? f)
      (apply (closure-fn f) xs)
      (apply f xs)))

(define (zero v)
  (cond
    [(closure? v) (closure-zero v)]
    [(procedure? v) null]
    [else (cons-zero v)]))
