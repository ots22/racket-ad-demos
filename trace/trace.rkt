#lang racket

(provide make-assignment
         id
         expr
         val

         trace?
         trace-items
         make-trace
         trace-empty?
         non-empty-trace?

         val->trace

         top
         top-id
         top-expr
         top-val

         trace-get
         trace-add
         trace-append
         trace-remove-duplicates
         trace-filter-out
         trace-prune
         trace-display)

(require "util.rkt"
         "assignment.rkt"
         racket/syntax
         (for-syntax racket/syntax))

(module+ test (require rackunit))

(struct trace (items) #:transparent
  #:guard (struct-guard/c (listof assignment?))
  #:methods gen:custom-write
  [(define write-proc
     (λ (x port mode)
       (when (non-empty-trace? x)
         (write (val (top x)) port))))]
  #:methods gen:equal+hash
  [(define (equal-proc t1 t2 equal?-recur)
     (and
      (equal?-recur (length (trace-items t1))
                    (length (trace-items t2)))
      (for/and ([a1 (trace-items t1)]
                [a2 (trace-items t2)])
        (and (equal?-recur (id a1) (id a2))
             (equal?-recur (expr a1) (expr a2))
             (equal?-recur (val a1) (val a2))))))
   (define (hash-proc t1 hash-recur)
     (for/sum ([a1 (trace-items t1)])
       (+ (hash-recur (id a1))
          (hash-recur (expr a1))
          (hash-recur (val a1)))))
   (define (hash2-proc t1 hash2-recur)
     (for/sum ([a1 (trace-items t1)])
       (+ (hash2-recur (id a1))
          (hash2-recur (expr a1))
          (hash2-recur (val a1)))))]
  )

;; provided constructor
(define (make-trace . items)
  (trace items))

(define (trace-empty? t)
  (null? (trace-items t)))

(define (non-empty-trace? t)
  (and (trace? t)
       (not (trace-empty? t))))

;; val->trace: make a single-assignment trace containing the given
;; value (with default id and expr)
;;
;; val->trace : any/c -> trace?
(define (val->trace v)
  (make-trace (make-assignment #:val v)))


;; The head assignment in trace t (which must be non-empty)
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
  (trace-remove-duplicates (trace (append* (map trace-items ts)))))

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
  (if (trace-empty? t)
      t
      (let ([head (top t)])
        (trace (cons head (remove-duplicates-before
                           (cdr (trace-items t)) #:key id))))))

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

;; Remove orphaned assignments from the trace
;;
;; trace-prune : trace? -> trace?
(define (trace-prune t)
  (define (rec t seen)
    (match (top-expr t)
      [(list f xs ...) (apply set-union
                              (map (λ (x) (rec (trace-get x t)
                                               (set-add seen x)))
                                   xs))]
      [_ seen]))
  (if (trace-empty? t)
      t
      (let ([seen (rec t (set (top-id t)))])
        (trace (filter (λ (a) (set-member? seen (id a)))
                       (trace-items t))))))

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
