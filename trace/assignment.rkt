#lang racket

(provide unknown
         unknown?
         make-assignment
         assignment?
         expr?
         assignment-id
         assignment-expr
         assignment-val
         uses-in)

(require syntax/parse
         "util.rkt"
         (for-template racket/base))

(struct unknown ())

(define (make-assignment #:id [id (next-name)]
                         #:val val
                         ;; #'null instead of #'()
                         #:expr [expr (if (null? val) #'null val)])
  (syntax-property
   #`(define #,id #,expr)
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
  [pattern ((~literal define) id:id expr:a-expr)])

(define assignment?
  (syntax-parser
    [a:assignment #t]
    [_ #f]))

(define assignment-id
  (syntax-parser [a:assignment #'a.id]))

(define assignment-expr
  (syntax-parser [a:assignment #'a.expr]))

(define/contract (assignment-val a)
  [-> assignment? any/c]
  (cond
    [(syntax-property a 'val) => syntax->datum]
    [else (unknown)]))

;; The uses of x in an expr e.  A list of each "use" is returned,
;; where this is highlighted by putting the match in a list.
;;
;; uses-in : symbol? expr? -> (listof modified-expr)
(define (uses-in x e)
  (syntax-parse e
    [(op as ...)
     (let ([as-ids (syntax-e #'(as ...))])
       (for/list ([i (indexes-of as-ids x free-identifier=?)])
         (let-values ([(fst rst) (split-at as-ids i)])
           (with-syntax ([(a ...) fst]
                         [b (car rst)]
                         [(c ...) (cdr rst)])
             #'(op a ... (b) c ...)))))]
    [_ '()]))

(module+ test
  (require rackunit)
  (check-equal? (map syntax->datum (uses-in #'x #'(+ x y))) '((+ (x) y)))
  (check-equal? (map syntax->datum (uses-in #'x #'(+ x x))) '((+ (x) x) (+ x (x))))
  (check-equal? (map syntax->datum (uses-in #'a #'(* x y))) '())
  (check-equal? (map syntax->datum (uses-in #'a #'5)) '()))
