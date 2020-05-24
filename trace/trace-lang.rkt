#lang racket

(require "trace.rkt"
         "trace-core.rkt"
         "trace-apply.rkt")

(provide #%module-begin
         #%top
         #%top-interaction

         provide
         all-defined-out
         require

         (rename-out [datum& #%datum]
                     [app& #%app]
                     [=& =]
                     [<&  <]
                     [<=& <=]
                     [>&  >]
                     [>=& >=]
                     [+& +]
                     [-& -]
                     [*& *]
                     [/& /]
                     [expt& expt]
                     [exp& exp]
                     [log& log]

                     [list& list]
                     [cons& cons]
                     [car& car]
                     [cdr& cdr]
                     [null?& null?]
                     [pair?& pair?]
                     [null& null]

                     [range& range]

                     [define& define]
                     [if& if]
                     [not& not]
                     [and& and]

                     [cons-add& cons-add]
                     [cons-zero& cons-zero]

                     [trace-display& trace-display]

                     [lambda& lambda]
                     [lambda& λ]

                     [apply& apply]))
