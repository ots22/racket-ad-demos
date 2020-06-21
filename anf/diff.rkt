#lang racket

(require (for-template (only-in "../trace/util.rkt"
                                identifier-append))
         "../trace/util.rkt"
         racket/syntax
         syntax/id-table
         syntax/parse
         "anf.rkt"
         "closure.rkt"
         "util.rkt"
         "../cons-arithmetic/cons-arithmetic.rkt")

;;----------------------------------------
;; backpropagators for primitive operations

(define k<-+
  (pat-λ (t)
    #'(let-values (((<-+) (#%plain-lambda (x y)
                            (cons (+ x y)
                                  (#%plain-lambda (Aw)
                                    (cons null
                                          (cons Aw (cons Aw null))))))))
        t)))

(define k<-*
  (pat-λ (t)
    #'(let-values (((<-*) (#%plain-lambda (x y)
                            (cons (* x y)
                                  (#%plain-lambda (Aw)
                                    (cons null
                                          (cons (* Aw y) (cons (* Aw x) null))))))))
        t)))

(define k<-cons
  (pat-λ (t)
    #'(let-values (((<-cons) (#%plain-lambda a b)
                             (cons (cons a b)
                                   (#%plain-lambda (Aw)
                                     (cons null
                                           (cons (car Aw) (cdr Aw)))))))
        t)))

(define k<-car
  (pat-λ (t)
    #'(let-values (((<-car) (#%plain-lambda xs)
                            (cons (car xs)
                                  (#%plain-lambda (Aw)
                                    (cons null
                                          (cons Aw (cons-zero (cdr xs))))))))
        t)))

(define backprops (make-immutable-free-id-table (hash #'* #'<-*
                                                      #'+ #'<-+
                                                      #'cons #'<-cons
                                                      #'car #'<-car)))

;; ----------------------------------------

(struct term (lhs))
(struct bind term (rhs))
(struct call term (fn args))
(struct defn term (free-vars))
(struct fvar term ())

;; ----------------------------------------

(define (forward-transform stx
                           [args null]
                           [free-vars null]
                           [rev-terms null]
                           [k identity])
  (syntax-parse stx
    #:conventions (anf-convention)
    #:literal-sets (kernel-literals)
    #:literals (make-closure apply-closure)
    ;; -------
    [(let-values (((x) (quote c))) S)
     (forward-transform #'S
                        args free-vars rev-terms
                        (pat-λ (t) (k #'(let-values (((x) (quote c)))
                                          t))))]

    ;; -------
    [(let-values (((x) x0)) S)
     #:with <-x0 (dict-ref backprops #'x0 #'x0)
     #:do [(display ">> ") (displayln #'S)]
     (forward-transform #'S
                        args free-vars (cons (bind #'x #'x0) rev-terms)
                        (pat-λ (t) (k #'(let-values (((x) <-x0)) t))))]

    ;; -------
    [(let-values (((x) (#%plain-lambda (xs ...) S1))) S2)
     #:with x-defn
     (forward-transform #'S1
                        (syntax-e #'(xs ...)) (append args free-vars) null
                        (pat-λ (t) #'(#%plain-lambda (xs ...)
                                       t)))
     #:do [(displayln #'x-defn)]
     #:do [(displayln #'S1)]

     (forward-transform #'S2
                        args free-vars rev-terms
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
                        args free-vars (cons (defn #'x (append args free-vars)) rev-terms)
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
                        args free-vars (cons (call #'x #'x0 (syntax-e #'(xs ...))) rev-terms)
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
                        args free-vars rev-terms
                        (pat-λ (t1)
                          (forward-transform #'S2
                                             args free-vars rev-terms
                                             (pat-λ (t2)
                                               #'(if B t1 t2)))))]

    ;; -------
    [x
     #:with r (reverse-transform #'x args free-vars rev-terms)
     #:do [(display "**") (displayln #'x)]
     (k #'(cons x r))]

    ;; -------
    [other (raise-syntax-error 'forward-transform "unknown form" #'other)]
    ))

;; ----------------------------------------

(define (reverse-transform result-var args free-vars rev-terms)
  (define-values (introductions adjoint-terms adjoints)
    (with-syntax ([Ay (generate-temporary 'Ay)])
      (for/fold ([introductions (pat-λ (t) #'(#%plain-lambda (Ay) t))]
                 [adjoint-terms (make-immutable-free-id-table (hash result-var (list #'Ay)))]
                 [adjoints (make-immutable-free-id-table)])
                ([t (append rev-terms (map (λ (a) (fvar a))
                                           (append args free-vars)))])
        ;; 1. Look up `term` in adjoint-terms
        ;; 2. Calculate (syntax of!) A(term) = (foldl cons-add (zero term) adjoint-terms)
        ;; 3. Include A(term) in introductions
        ;; 4. Include term -> A(term) in adjoints
        ;; 5. Match on the definition of `term`:
        ;;   - Update introductions appropriately with new terms (intermediate working + 'result')
        ;;   - Update adjoint-terms with the 'result' of the update
        ;;>after the loop:
        ;;   Return: ((A(free-vars) ...) A(args) ...) -- so need to pass args!

        (with-syntax* ([lhs (term-lhs t)]
                       [result (foldl (λ (x xs) #`(#%plain-app cons-add #,x #,xs))
                                      #'(#%plain-app zero lhs)
                                      (dict-ref adjoint-terms #'lhs '()))]
                       [adj (generate-temporary (identifier-append #'A #'lhs))])
          (match t
            [(fvar x)
             (values (compose1 introductions
                               (pat-λ (z) #'(let-values (((adj) result)) z)))
                     adjoint-terms
                     (dict-set adjoints #'lhs #'adj))]
            ;;
            [(bind y x)
             (values (compose1 introductions
                               (pat-λ (z) #'(let-values (((adj) result)) z)))
                     (dict-list-update adjoint-terms x #'adj)
                     (dict-set adjoints #'lhs #'adj))]
            ;;
            [(call y f xs)
             (with-syntax ([(Axs ...) (generate-temporaries (map (λ (g) (identifier-append #'A g)) xs))]
                           [Af (generate-temporary (identifier-append #'A #'f))])
               (values (compose1 introductions
                                 (pat-λ (z) #'(let-values (((adj) result)) z))
                                 (bind-vars (syntax-e #'(Af Axs ...)) #'((cdr f) adj)))
                       (dict-list-update* adjoint-terms
                                          (cons f xs)
                                          (syntax-e #'(Af Axs ...)))
                       (dict-set adjoints #'lhs #'adj)))]
            ;;
            [(defn f fvs)
             (with-syntax ([(Afree-vars ...) (generate-temporaries (map (λ (g) (identifier-append #'A g)) free-vars))])
               (values (compose1 introductions
                                 (pat-λ (z) #'(let-values (((adj) result)) z))
                                 (bind-vars (syntax-e #'(Afree-vars ...)) #'f))
                       (dict-list-update* adjoint-terms
                                          free-vars
                                          (syntax-e #'(Afree-vars ...)))
                       (dict-set adjoints #'lhs #'adj)))])))))

  ;;
  (with-syntax ([args-result
                 (foldl (pat-λ (t ts) #'(#%plain-app cons t ts))
                        #'null
                        (map (λ (key) (dict-ref adjoints key)) args))]
                [fv-result
                 (foldl (pat-λ (t ts) #'(#%plain-app cons t ts))
                        #'null
                        (map (λ (key) (dict-ref adjoints key)) free-vars))])
    (introductions
     #'(#%plain-app cons fv-result args-result))))


;; ----------------------------------------

(module+ test
  (syntax->datum
   (forward-transform
    #'(let-values
          (((f) (#%plain-lambda (x)
                  (let-values (((result) (#%plain-app apply-closure * x x)))
                    result))))
        f)
    ))


  (syntax->datum
   (forward-transform
    #'(let-values (((f*) (#%plain-lambda (x)
                           (let-values (((result) x))
                             result))))
        (let-values (((f) (#%plain-app make-closure f*)))
          f))
    ))

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
    ))

  )
