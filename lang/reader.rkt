#lang s-exp syntax/module-reader
clojure

#:read clj:read
#:read-syntax clj:read-syntax
#:wrapper1 (lambda (t)
             (parameterize ([read-case-sensitive #f]
                            [read-accept-infix-dot #f]
                            [read-curly-brace-as-paren #f]
                            [read-square-bracket-as-paren #f])
               (t)))
(require (prefix-in clj: "vector-reader.rkt"))
