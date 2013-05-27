(ns quines
  (:use [clojure.core.logic]))

(defn test1 []
  (run 1 (q) (== 5 q)))

(defn test2 []
  (run* (q)
    (conde
      [(== 5 q)]
      [(== 6 q)])))

(defn test3 []
  (run* [r]
    (fresh [x y]
      (== (lcons x (lcons y 'salad)) r)))
