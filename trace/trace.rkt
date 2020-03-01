#lang racket

;; assignments and traces

(provide (struct-out assignment)
         assignment
         make-assignment
         id
         expr
         val

         (except-out (struct-out trace) trace)
         make-trace
         trace-get
         trace-add
         trace-append
         trace-remove-duplicates
         trace-filter-out
         trace-prune
         trace-display

         top
         top-id
         top-expr
         top-val)

(require "util.rkt"
         racket/syntax
         (for-syntax racket/syntax))

(module+ test
  (require rackunit))

(define (expr? e)
  (match e
    [(list 'constant _) #t]
    [(list 'app (? symbol? _) ..1) #t]
    [_ #f]))

(module+ test
  (test-case "valid expressions"
    [check-true  (expr? '(app + a b c))]
    [check-true  (expr? '(constant '(1 2 3)))]
    [check-false (expr? '(app + 1 2 3))]
    [check-false (expr? '(constant 1 2 3))]
    [check-false (expr? '(app))]
    [check-false (expr? '(wrong))]
    [check-false (expr? 'wrong)]
    [check-false (expr? 1)]))

;; ----------------------------------------

;; represents a single assignment of value val, to an id (: symbol?),
;; as computed by an expression expr; expr must be:
;; - a value
;; - a reference to another variable
;;
(struct assignment (id expr val)
  #:transparent
  #:guard (struct-guard/c symbol? expr? any/c))

(define (make-assignment #:id [id (next-name)]
                         #:val val
                         #:expr [expr (list 'constant val)])
  (assignment id expr val))

;; shorter names for assignment accessors
(define (id   v) (assignment-id   v))
(define (expr v) (assignment-expr v))
(define (val  v) (assignment-val  v))

;; ----------------------------------------

(struct trace (items) #:transparent
  #:guard (struct-guard/c (listof assignment?))
  #:methods gen:custom-write
  [(define write-proc
     (λ (x port mode)
       (write (val (top x)) port)))])

;; alternative constructor
(define (make-trace . items)
  (trace items))

;; Extract the trace corresponding to an id i from the trace tr, if it
;; is present, or false
;;
;; trace-get : symbol? trace? -> (U trace? boolean?)
(define (trace-get s tr)
  (let ([maybe-a (member s (trace-items tr) (λ (u v) (eq? u (id v))))])
    (if maybe-a
        (trace maybe-a)
        #f)))

(module+ test
  (test-case "trace-get"
    (define tr (make-trace
                (make-assignment #:id 'a #:val 1)
                (make-assignment #:id 'b #:val 2)
                (make-assignment #:id 'c #:val 3)))

    (check-equal? (trace-get 'b tr)
                  (make-trace
                   (make-assignment #:id 'b #:val 2)
                   (make-assignment #:id 'c #:val 3)))

    (check-equal? (trace-get 'd tr)
                  #f)))


;; Add vs to the head of trace t, in order ((car vs) will be the new
;; head)
;;
;; trace-add : trace? assignment? ... -> trace?
(define (trace-add t . vs)
  (struct-copy trace t [items (append vs (trace-items t))]))

(module+ test
  (test-case "trace-add"
    (define actual
      (trace-add (make-trace (make-assignment #:id 'a #:val 1)
                             (make-assignment #:id 'b #:val 2))
                 (make-assignment #:id 'c #:val 3)
                 (make-assignment #:id 'd #:val 4)))

    (define expected
      (make-trace (make-assignment #:id 'c #:val 3)
                  (make-assignment #:id 'd #:val 4)
                  (make-assignment #:id 'a #:val 1)
                  (make-assignment #:id 'b #:val 2)))

    (check-equal? actual expected)))


;; Append traces ts
;;
;; trace-append : trace? ... -> trace?
(define (trace-append . ts)
  (trace-remove-duplicates (trace (apply append (map trace-items ts)))))

(module+ test
  (test-case "trace-append"
    (define a
      (trace-append (make-trace (make-assignment #:val 1))
                    (make-trace (make-assignment #:val 2))))
    (define b
      (apply trace-append (list (make-trace (make-assignment #:val 1))
                                (make-trace (make-assignment #:val 2)))))

    (check-equal? (map val (trace-items a)) '(1 2))
    (check-equal? (map val (trace-items a)) (map val (trace-items b)))))


;; Remove duplicate items from trace.  Will not remove the head item,
;; even if it is a duplicate.
;;
;; trace-remove-duplicates : trace? -> trace?
(define (trace-remove-duplicates t)
  (define head (top t))
  (trace (cons head (remove-duplicates-before
                     (cdr (trace-items t)) #:key assignment-id))))

(module+ test
  (define a (make-assignment #:id 'a #:val 0))
  (define b (make-assignment #:id 'b #:val 1))
  (define c (make-assignment #:id 'c #:val 2))
  (define tr (trace (list a b a a c a)))
  ; keep the head item, even if it occurs earlier
  (define expected (make-trace a b c a))

  (check-equal? (trace-remove-duplicates tr)
                expected))


;; trace-filter-out : (Listof symbol?) trace? -> trace?
(define (trace-filter-out ids t)
  (trace (remove* ids (trace-items t)
                  (λ (s assgn) (eq? s (id assgn))))))

(module+ test
  (test-case "trace-filter-out"
    (define tr (make-trace (make-assignment #:id 'a #:val 1)
                           (make-assignment #:id 'b #:val 2)
                           (make-assignment #:id 'c #:val 3)
                           (make-assignment #:id 'b #:val 2)
                           (make-assignment #:id 'd #:val 4)))
    (define expected (make-trace (make-assignment #:id 'a #:val 1)
                                 (make-assignment #:id 'c #:val 3)))

    (define actual (trace-filter-out '(b d) tr))

    (check-equal? actual expected)
    (check-equal? (trace-filter-out '(b d) actual) expected)))


;; The head assignment in trace t
;;
;; top : trace? -> assignment?
(define (top t) (car (trace-items t)))

;; helpers: id, expr and val of head assignment
(define (top-id t)   (id (top t)))
(define (top-expr t) (expr (top t)))
(define (top-val t)  (val (top t)))

(module+ test
  (test-case "top-val"
    (define tr (make-trace (make-assignment #:id 'b #:val 2)
                           (make-assignment #:id 'a #:val 1)))
    (check-equal? (top-val tr) 2)))


;; Remove orphaned assignments from the trace
;;
;; trace-prune : trace? -> trace?
(define (trace-prune t)
  (define (rec t seen)
    (match (top-expr t)
      [(list 'app f xs ...) (apply set-union
                                   (map (λ (x) (rec (trace-get x t)
                                                    (set-add seen x)))
                                        xs))]
      [_ seen]))
  (let ([seen (rec t (set (top-id t)))])
    (trace (filter (λ (a) (set-member? seen (id a)))
                   (trace-items t)))))

(module+ test
  (test-case "trace-prune"
    (define tr-1 (make-trace (make-assignment #:id 'b #:val 2)
                             (make-assignment #:id 'a #:val 1)))

    (define expected-1 (make-trace (make-assignment #:id 'b #:val 2)))

    (check-equal? (trace-prune tr-1) expected-1)

    (define tr-2
      (make-trace (make-assignment #:id 'b #:expr '(app exp a) #:val 1.0)
                  (make-assignment #:id 'a #:val 0.0)))

    (check-equal? (trace-prune tr-2) tr-2)))


;; Pretty print a trace
;;
;; trace-display : trace? -> void?
(define (trace-display t)
  (let* ([id-fmt        (map (compose ~a id) (trace-items t))]
         [expr-fmt      (map (compose ~a expr) (trace-items t))]
         [val-fmt       (map (compose ~a val) (trace-items t))]
         [id-width      (apply max (map string-length id-fmt))]
         [expr-width    (apply max (map string-length expr-fmt))]
         [val-width     (apply max (map string-length val-fmt))])
    (display (foldl (λ (i e v acc)
                      (string-append
                       (format "~a | ~a | ~a~%"
                               (~a i #:min-width id-width)
                               (~a e #:min-width expr-width)
                               (~a v #:min-width val-width))
                       acc))
                    "" id-fmt expr-fmt val-fmt))))

(module+ test
  (test-case "write trace"
    (define tr
      (make-trace (make-assignment #:id 'b #:expr '(app exp a) #:val 1.0)
                  (make-assignment #:id 'a #:val 0.0)))

    (check-equal? (~a tr) "1.0")))
