#lang racket

(provide make-assignment
         assignment?
         expr?
         (rename-out [assignment-id id]
                     [assignment-expr expr]
                     [assignment-val val])
         uses-in)

(require syntax/parse
         "util.rkt")

(define (make-assignment #:id [id (next-name)]
                         #:val val
                         #:expr [expr val])
  (syntax-property
   (datum->syntax #f `(def ,id ,expr))
   'val
   (datum->syntax #f val)
   #t))

(define-syntax-class a-expr
  [pattern (id:id ...+)]
  [pattern c])

(define expr?
  (syntax-parser
    [e:a-expr #t]
    [_ #f]))

(define-syntax-class assignment
  #:description "an assignment"
  [pattern ((~datum def) id:id expr:a-expr)])

(define assignment?
  (syntax-parser
    [a:assignment #t]
    [_ #f]))

(define assignment-id
  (syntax-parser
    [a:assignment (syntax->datum #'a.id)]))

(define assignment-expr
  (syntax-parser
    [a:assignment (syntax->datum #'a.expr)]))

(define/contract (assignment-val a)
  [-> assignment? any/c]
  (cond
    [(syntax-property a 'val) => syntax->datum]
    [else (raise-arguments-error
           'val
           "This assignment has no result (no 'val' syntax property set)"
           "id" (assignment-id a)
           "expr" (assignment-expr a))]))

;; The uses of x in an expr e.  A list of each "use" is returned,
;; where this is highlighted by putting the match in a list.
;;
;; uses-in : symbol? expr? -> (listof modified-expr)
(define (uses-in x e)
  (match e
    [(list op as ...)
     (for/list ([i (indexes-of as x)])
       (let-values ([(a b) (split-at as i)])
         (list* op (append a (list (list (car b))) (cdr b)))))]
    [_ null]))

(module+ test
  (require rackunit)
  (check-equal? (uses-in 'x '(+ x y)) '((+ (x) y)))
  (check-equal? (uses-in 'x '(+ x x)) '((+ (x) x) (+ x (x))))
  (check-equal? (uses-in 'a '(* x y)) '())
  (check-equal? (uses-in 'a 5) '()))
