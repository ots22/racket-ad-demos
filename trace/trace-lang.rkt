#lang racket

(require "trace.rkt"
         "trace-core.rkt")

(define one 1)

(provide #%module-begin
         #%top
         #%top-interaction
         (rename-out (datum #%datum))
         (rename-out (app #%app))
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

         (rename-out (list& list))
         (rename-out (cons& cons))

         (rename-out (car& car))
         (rename-out (cdr& cdr))
         
         (rename-out (null?& null?))
         (rename-out (pair?& pair?))
         (rename-out (null& null))

         (rename-out (range& range))

         (rename-out (define& define))
         (rename-out (if& if))

         (rename-out (not& not))
         
         and
         
         (rename-out (trace-display& trace-display))

         (rename-out (lambda& lambda))
         (rename-out (lambda& λ))

         provide
         all-defined-out
         require)
