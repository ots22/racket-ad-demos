#lang s-exp "transform.rkt"

;; like ANF, but no λ

(define (f x y)
  (define a (+ x y))
  (define b (+ a a))
  (define c (* a y))
  (define d 1)
  (+ c d)) ;; last expression doesn't need a name

(define (g x y)
  (define a (* x x))
  (define b (* x y))
  (+ a a))

(define (h x a h)
  (define z (* a x))
  (* z z))

;; (define (k x y)
;;   (define a (* x y))
;;   (define h (+ a y))
;;   (define b 1)
;;   (define y (g b a h)))

(define (i x y)
  (define z (* x y))
  (* z z))
