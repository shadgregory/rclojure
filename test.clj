#lang reader "clojure.rkt"
(require rackunit)

(defn factorial [n]
  (cond
   (<= n 1) 1
   :else (* n (factorial (dec n)))))

(check-equal? #t (eq? (= 1 1) true))
(check-equal? 120 (factorial 5))
(check-equal? [1 2] (pop [1 2 3]))
(check-equal? '(2 3) (pop '(1 2 3)))
(check-equal? "yes" (if true "yes"))
