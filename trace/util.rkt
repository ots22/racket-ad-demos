#lang racket

(provide flip
         chunk
         chunk2
         next-name
         remove-duplicates-before
         hash-list-append
         (for-syntax syntax-reverse))

(require racket/syntax)
(module+ test
  (require rackunit))

(define ((flip f) a b) (f b a))

(define (chunk n xs) (for/list ([x (in-slice n xs)]) x))
(define chunk2 (curry chunk 2))
(module+ test
  (check-equal? (chunk 2 '(1 2 3 4 5 6)) '((1 2) (3 4) (5 6)))
  (check-equal? (chunk 3 '(1 2 3 4 5)) '((1 2 3) (4 5))))

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (make-name base n)
  (format-symbol "%~a~a" base n))

(define (name-generator name [name-counter 0])
  (lambda () (make-name name (post-inc! name-counter))))

(define next-name (name-generator ""))

;; syntax-reverse : syntax? -> syntax?
;;
;; Giving a syntax object comprising a list, return a new syntax
;; object otherwise identical but with this list reversed.
(define-for-syntax (syntax-reverse stx)
  (datum->syntax stx (reverse (syntax->list stx))))

;; Like remove-duplicates, but the *last* occurrence of any duplicate
;; is kept instead of the first occurrence
;;
;; remove-duplicates-before : (Listof assignment?) -> (Listof assignment?)
(define (remove-duplicates-before xs #:key [key identity])
  (reverse (remove-duplicates (reverse xs) #:key key)))

(module+ test
  (check-equal? (remove-duplicates-before (list 1 2 1 3))
                (list 2 1 3)))

;; Append the list of items vs to a list contained in a hash table ht
;; under key k.  If the key does not exist, first create it with an
;; empty list as its value.
;;
;; hash-list-append : (Hashof any/c list?) any/c list? -> (Hashof any/c list?)
(define (hash-list-append ht k vs)
  (hash-update ht k 
               (λ (current-vs) (append vs current-vs))
               (λ () (list))))

(module+ test
  (test-case "hash-list-append"
    (check-equal? (hash-list-append (hash 1 '(2 3)) 1 '(0 5))
                  (hash 1 '(0 5 2 3)))
    (check-equal? (hash-list-append (hash 1 '(2 3)) 2 '(0 5))
                  (hash 1 '(2 3) 2 '(0 5)))))
