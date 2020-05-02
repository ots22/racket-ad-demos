#lang racket

(provide reshape
         ind-list
         flip
         raises?
         within-rel
         checks->preds
         chunk
         chunk2
         next-name
         remove-duplicates-before
         dict-list-append
         upd-adj
         syntax-reverse)

(require racket/syntax
         rackunit
         quickcheck)

(module+ test
  (require rackunit))

(define (reshape shape data)
  (define-values (result _)
    (let loop ([shape shape]
               [data data])
      (cond
        [(and (null? data) (not (null? shape)))
         (raise-user-error 'reshape "out of data when reshaping")]

        [(pair? shape)
         (let*-values ([(left data*) (loop (car shape) data)]
                       [(right data**) (loop (cdr shape) data*)])
           {values (cons left right) data**})]

        [(null? shape) {values null data}]

        [else          {values (car data) (cdr data)}])))
  result)

(module+ test
  (check-equal? (reshape '() '()) '())

  (check-equal? (reshape '((1 (2) . 3) . 4) '(a b c d))
                '((a (b) . c) . d))

  (check-equal? (reshape '(((a))) '(1 2))
                '(((1))))

  (check-equal? (reshape 'a '(1 2))
                1)

  (check-exn exn:fail? (λ () (reshape '(1) '()))))


(define (ind-list n i)
  (if (or (< i 0) (>= i n))
      (raise-arguments-error 'ind-list
                             "i must be positive and less than n"
                             "i" i
                             "n" n)
      (build-list n (λ (j) (if (= i j) 1 0)))))

(module+ test
  (check-equal? (ind-list 5 3) '(0 0 0 1 0))
  (check-exn exn:fail? (λ () (ind-list 5 6))))


(define ((flip f) a b) (f b a))

(define (raises? pred? t)
  (with-handlers ([pred?
                   (λ (e) #t)]
                  [exn:fail?
                   (λ (e) #f)])
    (begin (t) #f)))

(module+ test
  (test-case "raises?"
    (check-true  (raises? exn:fail:contract:divide-by-zero?
                          (λ () (/ 1 0))))
    (check-false (raises? exn:fail:contract:divide-by-zero?
                          (λ () (/ 1 2))))
    (check-false (raises? exn:fail:contract:divide-by-zero?
                          (λ () (error "some other error"))))))

(define (within-rel tol x y)
  (or (= x y)
      (< (/ (abs (- x y)) (min (abs x) (abs y))) tol)))

;; evaluate forms with any checks converted to predicates in their
;; test condition
(define-syntax-rule (checks->preds forms ...)
  (parameterize
      ([current-check-handler (const #f)] ;; check failed => #f
       [current-check-around
        (λ (chk-thunk)
          (with-handlers ([(const #t) (current-check-handler)])
            (chk-thunk)
            #t))]) ;; check passed => #t
    forms ...))

(define (chunk n xs) (for/list ([x (in-slice n xs)]) x))
(define chunk2 (curry chunk 2))
(module+ test
  (check-equal? (chunk 2 '(1 2 3 4 5 6)) '((1 2) (3 4) (5 6)))
  (check-equal? (chunk 3 '(1 2 3 4 5)) '((1 2 3) (4 5))))

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (next-name) (syntax->datum (generate-temporary "t")))

;; syntax-reverse : syntax? -> syntax?
;;
;; Giving a syntax object comprising a list, return a new syntax
;; object otherwise identical but with this list reversed.
(define (syntax-reverse stx)
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

;; Append the list of items vs to a list contained in a dictionary ht
;; under key k.  If the key does not exist, first create it with an
;; empty list as its value.
;;
;; dict-list-append : (Dict any/c list?) any/c list? -> (Dict any/c list?)
(define (dict-list-append ht k vs)
  (dict-update ht k
               (λ (current-vs) (append vs current-vs))
               (λ () (list))))

(module+ test
  (test-case "dict-list-append"
    (check-equal? (dict-list-append (hash 1 '(2 3)) 1 '(0 5))
                  (hash 1 '(0 5 2 3)))
    (check-equal? (dict-list-append (hash 1 '(2 3)) 2 '(0 5))
                  (hash 1 '(2 3) 2 '(0 5)))))

(define (upd-adj adj-table #:key key-fn . keys-and-traces)
  (for/fold ([adj-table* adj-table])
            ([kt (chunk2 keys-and-traces)])
    (let ([k (car kt)] [t (cadr kt)])
      (dict-list-append adj-table* k (list (key-fn t))))))
