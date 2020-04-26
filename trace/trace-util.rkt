#lang s-exp "trace-lang.rkt"

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (foldl f x0 xs) 
  (if (null? xs)
      x0
      (foldl f 
             (f (car xs) x0)
             (cdr xs))))
