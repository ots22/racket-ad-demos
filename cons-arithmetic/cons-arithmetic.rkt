#lang racket

(provide cons-conforming?
         cons-zero
         cons-add)

(module+ test
  (require rackunit))

(define (cons-conforming? a b)
  (cond
    [(or (null? a) (null? b))
     (and (null? a) (null? b))]
    [(or (not (pair? a)) (not (pair? b)))
     (and (not (pair? a)) (not (pair? b)))]
    [else (and (cons-conforming? (car a) (car b))
               (cons-conforming? (cdr a) (cdr b)))]))

(module+ test
  (check-true  (cons-conforming? null null))
  (check-true  (cons-conforming? '(() ()) '(() ())))
  (check-true  (cons-conforming? '(() ()) '(() ())))
  (check-true  (cons-conforming? 'a 5))

  (check-false (cons-conforming? '((1 2 3)) '(a b c)))
  (check-false (cons-conforming? 'a '(5)))
  (check-false (cons-conforming? '() '(())))
  (check-false (cons-conforming? '(a . b) '(a b))))
                               

;; the value that is cons-conforming to a, but filled with zeros
(define (cons-zero a)
  (cond
    [(null? a) null]
    [(pair? a) (cons (cons-zero (car a)) (cons-zero (cdr a)))]
    [else 0.0]))

(module+ test
  (check-equal? (cons-zero '(a (b . c) d)) '(0.0 (0.0 . 0.0) 0.0))
  (check-equal? (cons-zero 1.0) 0.0)
  (check-equal? (cons-zero '(1 2 3)) '(0.0 0.0 0.0))
  (check-equal? (cons-zero '()) '()))

(define (cons-add a b)
  (if (not (cons-conforming? a b))
      (raise-arguments-error 'cons-add
                             "arguments are non-conforming"
                             "a" a
                             "b" b)
      (if (and (null? a) (null? b))
          null
          (if (not (pair? a))
              (+ a b)
              (cons (cons-add (car a) (car b))
                    (cons-add (cdr a) (cdr b)))))))

(module+ test
  (check-equal? (cons-add 1 2) 3)
  (check-equal? (cons-add '(1) '(2)) '(3))
  (check-equal? (cons-add '(1 0) '(0 1)) '(1 1))
  (check-equal? (cons-add '(1 . 0) '(0 . 1)) '(1 . 1)))
  
