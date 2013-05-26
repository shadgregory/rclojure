#lang rclojure
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
(check-equal? [[1 2] [3 4] [5 6]] (conj [[1 2] [3 4]] [5 6]))
(check-equal?  [4 5 6 7] (subvec [12 3 4 5 6 7] 2))
(check-equal? [4 5] (subvec [12 3 4 5 6 7] 2 4))
(let [v [1 2 3]]
  (check-equal? [1 2 3 4] (conj v 4)))
(let [l '(1 2 3)]
  (check-equal? '(4 1 2 3) (conj l 4)))
(let [l (list 1 (list 2 3) 4)]
  (check-equal? 1 (first l)))
(let [l '(1 '(2 3) 4)]
  (check-equal? 1 (first l)))
(let [x 1 y 3]
  (check-equal? x 1)
  (check-equal? y 3))
(check-equal? 2 (get [1 2 3] 1))
(check-equal? nil (get [1 2 3] 5))
(check-equal? 2 (get {:a 1 :b 2} 'b))
(check-equal? 1 (rem 10 9))
(check-equal? 0 (rem 2 2))
(check-equal? 0 (mod 10 5))
(check-equal? 0 (mod 10 10))
(check-equal? 0 (mod 10 -1))
(check-equal? 4 (mod 10 6))
(check-equal? 3 (mod -2 5))

(letfn [(twice [x]
               (* x 2))
        (six-times [y]
          (* (twice y) 3))]
  (println "Twice 15 = " (twice 15))
  (println "Six times 15 = " (six-times 15)))

(check-equal? (true? true) #t)
(check-equal? (true? 1) #f)
(check-equal? (true? (= 1 1)) #t)
(check-equal? (false? false) #t)
(check-equal? (false? true) #f)
(check-equal? "no" (if nil "yes" "no"))
(check-equal? (false? "foo") #f)
(check-equal? (max 1 2 3 4 5) 5)
(check-equal? (max 5 4 3 2 1) 5)
(check-equal? (max 100) 100)
(check-equal? (min 1 2 3 4 5) 1)
(check-equal? (nil? nil) #t)
(check-equal? (nil? 0) #f)
(check-equal? (reduce + [1 2 3 4 5]) 15)
(def my-coll [1 2 3 4 5])
(check-equal? (reduce + my-coll) 15)
(check-equal? (reduce + []) 0)
(check-equal? (reduce + 1 []) 1)
(check-equal? (reduce + 1 [2 3]) 6)
(def my-vec [2 3])
(check-equal? (reduce + 1 my-vec) 6)
(check-equal? {:a 1 :b 2} (hash 'a 1 'b 2))
;; nth
(check-equal? (nth ["a" "b" "c" "d"] 0) "a")
(check-equal? (nth (list "a" "b" "c" "d") 0) "a")
(check-equal? (nth ["a" "b" "c" "d"] 1) "b")
(check-equal? (nth [] 0 "nothing found") "nothing found")
(check-equal? (nth [0 1 2] 77 1337) 1337)
(check-equal? (nth "Hello" 0) #\H)
(check-equal? (nth '(1 2 3) 0) 1)
(check-equal? (nth '(:alpha :bravo :charlie) 0) ':alpha)
(check-equal?
 (-> "a b c d"
     string-upcase
     (string-replace "A" "X")
     (string-split " ")
     first)
 "X")

(check-equal?
 (->> 5 (+ 3) (/ 2) (- 1))
 (/ 3 4))

(check-equal?
  (->> 1 ((fn [x] (+ x 1))))
  2)
