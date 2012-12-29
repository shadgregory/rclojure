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
(check-equal? [1 2 3 4] (conj [1 2 3] 4))
(check-equal? '(4 1 2 3) (conj '(1 2 3) 4))
;(check-equal? [[1 2] [3 4] [5 6]] (conj [[1 2] [3 4]] [5 6]))
(let ((v [1 2 3]))
  (check-equal? [1 2 3 4] (conj v 4)))
(let ((l '(1 2 3)))
  (check-equal? '(4 1 2 3) (conj l 4)))

