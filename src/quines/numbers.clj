(ns quines.numbers
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn zeroo [n]
  (== '() n))

(defn build-num [n]
  (cond
    (odd? n)
      (cons 1
        (build-num (quot (- n 1) 2)))
    (and (not (zero? n)) (even? n))
      (cons 0
        (build-num (quot n 2)))
    (zero? n) '()))

(defn poso [n]
  (fresh (a d)
    (== `(,a . ,d) n)))

(defn >1o [n]
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(defn full-addero [b x y r c]
  (conde
    ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
    ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
    ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))))
