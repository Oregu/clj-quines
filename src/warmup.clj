(ns warmup
  (:refer-clojure :exclude [==])
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
      (== (lcons x (lcons y 'salad)) r))))

;;
;; Translation of canonical Prolog's list size goal
;;
(defnu sizeo [l n]
  ([[] 0])
  ([[_ . rest] _]
  	(fresh [n1]
      (sizeo rest n1)
      (project [n1]
        (== n (+ n1 1))))))

(defn test4 []
  (run* [q]
    (sizeo '(1 q q :r 4) q)))

;;
;; member(X,[X|_]).
;; member(X,[_|T]) :- member(X,T).
;;

(defn sqrto [sq n] (== sq (* n n)))

;; This will power up
(defn test5 []
  (run* (q) (sqrto q 9)))

;; But this will throw ex
;; No equation solving, sorry
(defn test6 []
  (run* (q) (sqrto 81 q)))

;;
;; Fibonacci naive relation
;;
(defnu fib-naiveo [n fibn]
  ([0 0])
  ([1 1])
  ([_ _]
    (fresh [fn-1 fn-2]
      (fib-naiveo (- n 1) fn-1)
      (fib-naiveo (- n 2) fn-2)
      (project [fn-1 fn-2]
        (== fibn (+ fn-1 fn-2))))))

(defn test7 []
  (run* (q) (fib-naiveo 10 q)))

;; And again this is not working. (Projecting didn't help.)
(defn test8 []
  (run* (q) (fib-naiveo q 21)))
