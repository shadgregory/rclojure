#lang reader "clojure.rkt"

(println "hello")

(eq? (= 1 1) true)

(if true "yes")

(display (vector-ref 1 [1 2 3]))