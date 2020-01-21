#lang racket

(require racket/syntax
         syntax/id-table)

(provide #%app
         #%module-begin
         #%top
         #%top-interaction
         (rename-out (datum #%datum))
         (rename-out (=& =))
         (rename-out (<&  <))
         (rename-out (<=& <=))
         (rename-out (>&  >))
         (rename-out (>=& >=))
         (rename-out (+& +))
         (rename-out (-& -))
         (rename-out (*& *))
         (rename-out (/& /))
         (rename-out (expt& expt))
         (rename-out (exp& exp))
         (rename-out (log& log))

         (rename-out (define& define))
         (rename-out (if& if))
         
         display-trace
         grad
         )


;; based on nocell

;; ----------------------------------------
;; utils (uses set! for the counter)

(define-syntax-rule (post-inc! x)
  (begin0
      x
    (set! x (+ x 1))))

(define (name-generator name [name-counter 0])
  (lambda () (make-name name (post-inc! name-counter))))

(define next-name (name-generator ""))

(define (make-name base n)
  (format-symbol "%~a~a" base n))

;; Like remove-duplicates, but the *last* occurrence of any duplicate
;; is kept instead of the first occurrence
(define (remove-duplicates-before xs)
  (reverse (remove-duplicates (reverse xs) #:key assignment-id)))

;; syntax-reverse : syntax? -> syntax?
;;
;; Giving a syntax object comprising a list, return a new syntax
;; object otherwise identical but with this list reversed.
(define-for-syntax (syntax-reverse stx)
  (datum->syntax stx (reverse (syntax->list stx))))


;; ----------------------------------------
;; assignments

(struct assignment (id expr val #|context|#) #:transparent)

(define (make-assignment #:id id #:expr expr #:val val)
  (assignment id expr val))

(define (id      v) (assignment-id      v))
(define (expr    v) (assignment-expr    v))
(define (val     v) (assignment-val     v))
;;(define (context v) (assignment-context v))

(struct trace (items) #:transparent
  ;; #:methods gen:custom-write
  ;; [(define write-proc 
  ;;    (lambda (x port mode)
  ;;      (write (val (top x)) port)))]
  )

(define (trace-add t . vs)
  (struct-copy trace t
               [items (append vs (trace-items t))]))

(define (trace-append . ts)
  (trace (apply append (map trace-items ts))))

(define (top t) (car (trace-items t)))

;; pretty print the trace
(define (display-trace t)
  (let* ([id-fmt        (map (compose ~a id) (trace-items t))]
         [expr-fmt      (map (compose ~a expr) (trace-items t))]
         [val-fmt       (map (compose ~a val) (trace-items t))]
         ;;[context-fmt   (map (compose ~a id) t)]
         [id-width      (apply max (map string-length id-fmt))]
         [expr-width    (apply max (map string-length expr-fmt))]
         [val-width     (apply max (map string-length val-fmt))]
         ;;[context-width (apply max (map string-length context-fmt))]
         )

    (display (foldl (lambda (i e v #|c|# acc)
                      (string-append
                       (format "~a | ~a | ~a~%"
                               (~a i #:min-width id-width)
                               (~a e #:min-width expr-width)
                               (~a v #:min-width val-width)
                               ;(~a c #:min-width context-width)
                               )
                       acc))
                    "" id-fmt expr-fmt val-fmt #|context-fmt|#))))

;; ----------------------------------------
;; datum

;; A datum expands to a stack with a single element
(define-syntax (datum stx)
  (syntax-case stx ()
    [(_ . d)
     #'(trace (list (assignment (next-name) (#%datum . d) (#%datum . d))))]))


;; ----------------------------------------
;; arithmetic

(define-syntax (define-traced-primitive stx)
  (syntax-case stx ()
    ;; f        : id?
    ;; args ... : trace? ...
    ;; body ... : expression? ...
    [(_ (f args ...) f-name body ...)
     (with-syntax ([(arg-vals ...) #'((val (top args)) ...)]
                   [(arg-ids ...) #'((id (top args)) ...)]
                   [(rev-args ...) (syntax-reverse #'(args ...))]
                   [f-val #'f])
       #'(define (f args ...)
           (let* (;; shadow the actual args (which have trace annotations)
                  [result     (let ([args arg-vals] ...) body ...)]
                  ;; trace the arguments in reverse order
                  [arg-traces (trace (remove-duplicates-before
                                      (append (trace-items rev-args) ...)))]
                  [result-name (next-name)])
             (trace-add
              arg-traces
              (make-assignment #:id   result-name
                               #:expr (list f-name arg-ids ...)
                               #:val  result)))))]))

(define-syntax (define-traced stx)
  (syntax-case stx ()
    [(_ (f args ...) body ...)
     (with-syntax ([(rev-args ...) (syntax-reverse #'(args ...))])
       #'(define (f args ...)
           (let ([arg-traces (trace (append (trace-items rev-args) ...))]
                 [result-trace (let () body ...)])
             (trace
              (remove-duplicates-before
               (trace-items
                (trace-add
                 (trace-append result-trace arg-traces)
                 (make-assignment #:id (next-name)
                                  #:expr (id (top result-trace))
                                  #:val (val (top result-trace))))
                ))))))]))

(define-syntax (define& stx)
  (syntax-case stx ()
    [(_ (id args ...) body ...)
     #'(define-traced (id args ...) body ...)]
    [(_ id expr) #'(define id expr)]))

(define-syntax (if& stx)
  (syntax-case stx ()
    [(_ test-expr then-expr else-expr)
     #'(if (val (top test-expr))
           then-expr
           else-expr)]))

(define-traced-primitive (+& a b)    '+    (+ a b))
(define-traced-primitive (-& a b)    '-    (- a b))
(define-traced-primitive (*& a b)    '*    (* a b))
(define-traced-primitive (/& a b)    '/    (/ a b))
(define-traced-primitive (=& a b)    '=    (= a b))
(define-traced-primitive (<& a b)    '<    (< a b))
(define-traced-primitive (>& a b)    '>    (> a b))
(define-traced-primitive (<=& a b)   '<=   (<= a b))
(define-traced-primitive (>=& a b)   '>=   (>= a b))
(define-traced-primitive (expt& a b) 'expt (expt a b))
(define-traced-primitive (exp& a)    'exp  (exp a))
(define-traced-primitive (log& a)    'log  (log a))

;; ----------------------------------------
;; think about defining 'list'
;; (define (list& ))

;; ----------------------------------------
;; gradients

;; extract the value from tr if present
(define (trace-get i tr)
  (let ([maybe-a (member i (trace-items tr) (λ (u v) (eq? u (id v))))])
    (and maybe-a (car maybe-a))))

(provide trace-get)

;; really want to build up the gradient calculation on a stack too,
;; making second derivatives possible
(define ((grad f) . xs)
  (let* ([indep-ids (map (compose id top) xs)]
         [result (apply f xs)]
         [result-id (id (top (apply f xs)))]
         )
    (map (λ (i) (trace-get i result)) indep-ids)
    ))


