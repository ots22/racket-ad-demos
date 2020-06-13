#lang racket

(require (for-template (only-in "../trace/util.rkt"
                                identifier-append))
         racket/syntax
         syntax/id-table
         syntax/parse
         "anf.rkt"
         "closure.rkt"
         "util.rkt")

;;----------------------------------------
;; backpropagators for primitive operations

(define k<-+
  (pat-λ (t)
    #'(let-values (((<-+) (#%plain-lambda (x y)
                            (cons (+ x y)
                                  (#%plain-lambda (Aw)
                                    (cons Aw (cons Aw null)))))))
        t)))

(define k<-*
  (pat-λ (t)
    #'(let-values (((<-*) (#%plain-lambda (x y)
                            (cons (* x y)
                                  (#%plain-lambda (Aw)
                                    (cons (* Aw y) (cons (* Aw x) null)))))))
        t)))

(define k<-cons
  (pat-λ (t)
    #'(let-values (((<-cons) (#%plain-lambda a b)
                             (cons (cons a b)
                                   (#%plain-lambda (Aw)
                                     (cons (car Aw) (cdr Aw))))))
        t)))

(define k<-car
  (pat-λ (t)
    #'(let-values (((<-car) (#%plain-lambda xs)
                            (cons (car xs)
                                  (#%plain-lambda (Aw)
                                    (cons Aw (cons-zero (cdr xs)))))))
        t)))

(define (primitive-backprops) (make-immutable-free-id-table (hash #'* #'<-*
                                                                  #'+ #'<-+
                                                                  #'cons #'<-cons
                                                                  #'car #'<-car)))

;; ----------------------------------------

(define (forward-transform stx
                           [backprops (primitive-backprops)]
                           [free-vars null]
                           [k identity])
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    #:literals (make-closure apply-closure)
    ;; -------
    [(let-values (((x) (quote c))) S)
     (forward-transform #'S
                        backprops
                        free-vars
                        (pat-λ (t) (k #'(let-values (((x) (quote c)))
                                          t))))]
    ;; -------
    [(let-values (((x) x0)) S)
     #:with <-x0 (dict-ref backprops #'x0 #'x0)
     (forward-transform #'S
                        backprops
                        free-vars
                        (pat-λ (t) (k #'(let-values (((x) <-x0)) S))))]
    ;; -------
    [(let-values (((x) (#%plain-lambda (xs ...) S1))) S2)
     #:with x-defn
     (forward-transform #'S1
                        backprops
                        (append (syntax-e #'(xs ...)) free-vars)
                        (pat-λ (t) #'(#%plain-lambda (xs ...)
                                       t)))     
     (forward-transform #'S2
                        backprops
                        free-vars
                        (pat-λ (t) (k #'(let-values (((x) x-defn))
                                          t))))]
    ;; -------
    [(let-values (((x) (#%plain-app make-closure f
                                    (~optional z #:defaults ((z #f))))))
       S)
     #:with (free-vars* ...) free-vars
     #:with free-var-zeros
     #'(#%plain-app apply-closure list
                    (#%plain-app apply-closure zero free-vars*) ...)
     #:with z* (if (attribute z) #'z #'free-var-zeros)
     (forward-transform #'S
                        backprops
                        free-vars
                        (pat-λ (t)
                          (k
                           #'(let-values (((x) (#%plain-app make-closure f z*)))
                               t))))]
    ;; -------
    [(let-values (((x) (#%plain-app apply-closure x0 xs ...))) S)
     #:with <-x0 (dict-ref backprops #'x0 #'x0)
     #:with (<-xs ...) (map (λ (x) (dict-ref backprops x x))
                            (syntax-e #'(xs ...)))
     #:with x* (generate-temporary (identifier-append #'x #'*))
     #:with x-bar (generate-temporary (identifier-append #'x #'-bar))
     (forward-transform #'S
                        backprops
                        free-vars
                        (pat-λ (t)
                          (k
                           #'(let-values
                                 (((x*) (#%plain-app apply-closure <-x0
                                                     <-xs ...)))
                               (let-values
                                   (((x) (#%plain-app apply-closure car x*)))
                                 (let-values
                                     (((x-bar) (#%plain-app apply-closure cdr
                                                            x*)))
                                   t))))))]
    ;; -------
    [(if B S1 S2)
     (forward-transform #'S1
                        backprops
                        free-vars
                        (pat-λ (t1)
                          (forward-transform #'S2
                                             backprops
                                             free-vars
                                             (pat-λ (t2)
                                               #'(if B t1 t2)))))]
    ;; -------
    [x (k #'x)]
    ;; -------
    [other (raise-syntax-error 'forward-transform "unknown form" #'other)]
    ))

;; ----------------------------------------

(module+ test
  (syntax->datum
   (forward-transform
    #'(let-values
          (((f) (#%plain-lambda (x)
                  (let-values (((result) (#%plain-app apply-closure * x x)))
                    result))))
        f)
    (primitive-backprops)))


  (syntax->datum
   (forward-transform
    #'(let-values (((f) (#%plain-lambda (x)
                          (let-values (((result) x))
                            x))))
        f)
    (primitive-backprops)))

  (syntax->datum
   (forward-transform
    #'(let-values
          (((f) (#%plain-lambda (x)
                  (let-values (((fn) (#%plain-lambda (y) x)))
                    (let-values (((res) (#%plain-app make-closure fn)))
                      res)))))
        (let-values (((two) '2))
          (let-values (((z) (#%plain-app apply-closure f two)))
            z)))
    (primitive-backprops)))
  ;;
  )
