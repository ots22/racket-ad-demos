#lang racket

(provide (struct-out dual-number)
         number-or-dual?
         primal
         dual)

(struct dual-number (p d) #:transparent #:mutable)

(define (number-or-dual? x) (or (dual-number? x) (number? x)))

;; zero, with the same type as x
(define (zero x) (- x x))

(define (primal x)
  (cond 
    [(dual-number? x) (dual-number-p x)]
    [(number? x) x]
    [else (raise-argument-error 'primal "number? or dual-number?" x)]))

(define (dual x)
  (cond 
    [(dual-number? x) (dual-number-d x)]
    [(number? x) (zero x)]
    [else (raise-argument-error 'dual "number? or dual-number?" x)]))
