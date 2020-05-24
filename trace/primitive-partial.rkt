#lang racket

(provide partial)

(require "trace.rkt"
         "trace-core.rkt"
         "let-traced.rkt")

;; the i'th partial derivative of op at xs
;;
;; partial : integer? symbol? -> . (Listof trace?) -> trace?
(define/match ((partial i op) . xs)
  [(0 '+ _) (traced 1.0)]
  [(1 '+ _) (traced 1.0)]
  ;;
  [(0 '- _) (traced 1.0)]
  [(1 '- _) (traced -1.0)]
  ;;
  [(0 '* (list _ y&)) (traced y&)]
  [(1 '* (list x& _)) (traced x&)]
  ;;
  [(0 'exp (list x&)) (traced (exp& x&))]
  ;;
  [(_ _ _) (raise-arguments-error
            'partial
            "Can't take the requested partial derivative"
            "i" i
            "op" op)])

(module+ test
  (require rackunit
           quickcheck
           rackunit/quickcheck
           "util.rkt"
           "test-util.rkt")

  (test-case "partial"
    (check-property
     (property ([x (gen-trace (choose-real -1e10 1e10))]
                [y (gen-trace (choose-real -1e10 1e10))])
               (and
                (top-val (app& =& ((partial 0 '*) x y) y))
                (top-val (app& =& ((partial 1 '*) x y) x))
                (raises? exn:fail:contract? (λ () ((partial 2 '*) x y))))))

    (check-property
     (property ([x (gen-trace (choose-real -1e10 1e10))]
                [y (gen-trace (choose-real -1e10 1e10))])
               (and
                (top-val (app& =& ((partial 0 '+) x y) (val->trace 1.0)))
                (top-val (app& =& ((partial 1 '+) x y) (val->trace 1.0)))
                (raises? exn:fail:contract? (λ () ((partial 2 '+) x y))))))

    (check-exn exn:fail:contract? (λ () ((partial 0 'unknown) 0.0)))))
