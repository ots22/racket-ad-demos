#lang racket

;; require with a phase-shift of -1, so we can test the
;; 'define-for-syntax' functions at runtime
(require (for-meta -1 "definition-helpers.rkt")
         syntax/id-table)

(module+ test
  ;; (dfn-build #'+
  ;;            (list #'(define b 2) #'(define a 1)) 
  ;;            (list #'(define c 2)))

  (define (display-trace+terms tr tab)
    (for ([dfn tr])
      (displayln (syntax->datum dfn)))
    
    (for ([(k v) (in-dict tab)])
      (display (format "~a : ~a~%" (syntax->datum k) (if (pair? v)
                                                         (map syntax->datum v)
                                                         (syntax->datum v)))))
    )
  

  ;; (let-values ([(tr tab)
  ;;               (adjoint-trace+terms #'(define x (cons a b))
  ;;                                    (list #'(define a 1)
  ;;                                          #'(define b 2)
  ;;                                          #'(define Aw (1 2)))
  ;;                                    (make-immutable-free-id-table))])
  ;;   (display-trace+terms tr tab))

  ;; (let-values ([(tr tab)
  ;;               (adjoint-trace+terms #'(define x (car ls))
  ;;                                    (list #'(define ls '(1 2))
  ;;                                          #'(define Aw 0.1))
  ;;                                    (make-immutable-free-id-table))])
  ;;   (display-trace+terms tr tab))

  ;; (let-values ([(tr tab)
  ;;               (adjoint-trace+terms #'(define z (* x y))
  ;;                                    (list #'(define x 1)
  ;;                                          #'(define y 2)
  ;;                                          #'(define Az 0.1))
  ;;                                    (make-immutable-free-id-table))])
  ;;   (display-trace+terms tr tab))

  (for ([x 
         (A/r (list #'(define a 1)
                    #'(define b 2)
                    #'(define c (* a b))
                    #'(define x (* c c)))
              (list #'a #'b)
              (list #'(define n0 0)
                    #'(define n1 1)
                    #'(define s (cons n0 n1))))])
    (println (syntax->datum x)))



)
