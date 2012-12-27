#lang reader "clojure.rkt"

(println "hello\n")

(eq? (= 1 1) true)

(if true "yes")

(println [1 2 3])
(newline)

(println "hello again\n")

[x y]
(pop [1 2 3]) ;[1 2]
(pop '(1 2 3)) ;(2 3)

(defn factorial [n]
  (cond
   (<= n 1) 1
   :else (* n (factorial (dec n)))))

(factorial 5) ;120
                 