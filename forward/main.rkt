#lang racket

(require "dual-number.rkt")

(provide
 (rename-out (dual-= =))
 (rename-out (dual-< <))
 (rename-out (dual-> >))
 (rename-out (dual-<= <=))
 (rename-out (dual->= >=))
 (rename-out (dual-+ +))
 (rename-out (dual-- -))
 (rename-out (dual-* *))
 (rename-out (dual-/ /))
 (rename-out (dual-exp exp))
 (rename-out (dual-log log))
 (rename-out (number-or-dual? number?))
 get-dual-part
 D
 D0
 D1
 D2
 grad)

(define (dual-= x y)
  (= (primal x) (primal y)))

(define (dual-< x y)
  (< (primal x) (primal y)))

(define (dual-> x y)
  (> (primal x) (primal y)))

(define (dual-<= x y)
  (<= (primal x) (primal y)))

(define (dual->= x y)
  (>= (primal x) (primal y)))

;; ----------------------------------------
;; add

(define (+D x y)
  (if (or (dual-number? x) (dual-number? y))
      (dual-number (+D (primal x) (primal y))
                   (+D (dual x) (dual y)))
      (+ x y)))

(define (dual-+ . xs)
  (foldl +D 0 xs))

;; ----------------------------------------
;; subtract/negate

(define (dual-negate x)
  (if (dual-number? x)
      (dual-number (- (primal x)) (- (dual x)))
      (- x)))

(define (dual-subtract x0 x1 . xs)
  (let ([s (apply dual-+ x1 xs)])
    (if (or (dual-number? x0) (dual-number? s))
        (dual-number (- (primal x0) (primal s))
                     (- (dual x0) (dual s)))
        (- x0 s))))

(define (dual-- x . xs)
  (if (null? xs)
      (dual-negate x)
      (apply dual-subtract x xs)))

;; ----------------------------------------
;; multiply

(define (*D x y)
  (if (or (dual-number? x) (dual-number? y))
      (dual-number (*D (primal x) (primal y))
                   (+D (*D (dual x) (primal y))
                       (*D (primal x) (dual y))))
      (* x y)))

(define (dual-* . xs)
  (foldl *D 1 xs))

;; ----------------------------------------
;; divide/reciprocal

(define (dual-reciprocal x)
  (if (dual-number? x)
      (dual-number (dual-/ (primal x))
                   (dual-/ (dual-- (dual x))
                           (dual-* (primal x) (primal x))))
      (/ x)))

(define (dual-divide n d0 . ds)
  (let ([d (apply dual-* d0 ds)])
    (if (or (dual-number? n) (dual-number? d))
        (let ([N  (primal n)]
              [dN (dual n)]
              [D  (primal d)]
              [dD (dual d)])
          (dual-number (dual-/ N D)
                       (dual-- (dual-/ dN D)
                               (dual-/ (dual-* N dD) (dual-* D D)))))
        (/ n d))))

(define (dual-/ x . xs)
  (if (null? xs)
      (dual-reciprocal x)
      (apply dual-divide x xs)))

;; ----------------------------------------
;; exp, log

(define (dual-exp x)
  (if (dual-number? x)
      (dual-number (primal x) (dual-* (primal x) (dual x)))
      (exp x)))

(define (dual-log x)
  (if (dual-number? x)      
      (dual-number (dual-log (primal x))
                   (dual-/ (dual x) (primal x)))
      (log x)))

;; ----------------------------------------

(define (get-dual-part s)
  (cond [(null? s) null]
        [(cons? s) 
         (cons (get-dual-part (car s))
               (get-dual-part (cdr s)))]
        [(vector? s)
         (vector-map get-dual-part s)]
        [else (dual s)]))
        
;; n'th partial derivative of a function f
(define (((D n) f) . args)
  (let ([args* (for/list [(i (in-naturals))
                          (a args)]
                 (if (= i n)
                     ;;(dual-+ a (dual-number 0 1))
                     (dual-number a 1)
                     (dual-number a 0)))])
    (get-dual-part (apply f args*))))

(define D0 (D 0))
(define D1 (D 1))
(define D2 (D 2))

(define ((grad f) . args)
  (build-vector (length args) (λ (i) (apply ((D i) f) args))))


