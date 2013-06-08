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
  (matche [n]
    ([[?a . _]] succeed)))

;(define poso (lambda (n) (fresh (a d) (== `(,a . ,d) n)))))

(defn >1o [n]
  (matche [n]
    ([[?a ?ad . _]] succeed)))

(defn full-addero [b x y r c]
  (conde
    [(== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c)]
    [(== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c)]
    [(== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c)]
    [(== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c)]
    [(== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c)]
    [(== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c)]
    [(== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c)]
    [(== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c)]))

(declare gen-addero)

(defn addero [d n m r]
  (conde
    [(== 0 d) (== '() m) (== n r)]
    [(== 0 d) (== '() n) (== m r) (poso m)]
    [(== 1 d) (== '() m) (addero 0 n '(1) r)]
    [(== 1 d) (== '() n) (poso m) (addero 0 '(1) m r)]
    [(== '(1) n) (== '(1) m)
      (fresh [a c]
        (matche [r]
          ([[a c]] succeed))
        (full-addero d 1 1 a c))]
    [(== '(1) n) (gen-addero d n m r)]
    [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
    [(>1o n) (gen-addero d n m r)]))

(defn gen-addero [d n m r]
  (fresh [a b c e x y z]
    (matche [n] ([[,a . ,x]] succeed))
    (matche [m] ([[,b . ,y]] succeed)) (poso y)
    (matche [r] ([[,c . ,z]] succeed)) (poso z)
    (full-addero d a b c e)
    (addero e x y z)))

(defn pluso [n m k]
  (addero 0 n m k))

(defn minuso [n m k]
  (pluso m k n))

(declare odd-*o)

(defn *o [n m p]
  (conde
    [(== '() n) (== '() p)]
    [(poso n) (== '() m) (== '() p)]
    [(== '(1) n) (poso m) (== m p)]
    [(>1o n) (== '(1) m) (== n p)]
    [(matche [n] ([[0 . ?x]] (poso ?x)
      (matche [p] ([[0 . ?z]] (poso ?z)
        (>1o m)
        (*o ?x m ?z)))))]
    [(matche [n] ([[1 . ?x]] (poso ?x)))
      (matche [m] ([[0 . ?y]] (poso ?y)))
      (*o m n p)]
    [(matche [n] ([[1 . ?x]] (poso ?x)
      (matche [m] ([[1 . ?y]] (poso ?y)
        (odd-*o ?x n m p)))))]))

(declare bound-*o)

(defn odd-*o [x n m p]
  (fresh [q]
    (bound-*o q p n m)
    (*o x m q)
    (pluso `(0 . ,q) m p)))

(defn bound-*o [q p n m]
  (conde
    [(== '() q) (poso p)]
    [(fresh [a0 a1 a2 a3 x y z]
      (matche [q] ([[,a0 . ,x]] succeed))
      (matche [p] ([[,a1 . ,y]] succeed))
      (conde
        [(== '() n)
         (matche [m] ([[,a2 . ,z]] succeed))
         (bound-*o x y z '())]
        [(matche [n] ([[,a3 . ,z]] succeed))
         (bound-*o x y z m)]))]))

(defn =lo [n m]
  (conde
    [(== '() n) (== '() m)]
    [(== '(1) n) (== '(1) m)]
    [(fresh (x y)
      (matche [n] ([[_ . ,x]] (poso x)))
      (matche [m] ([[_ . ,y]] (poso y)))
      (=lo x y))]))

(defn <lo [n m]
  (conde
    [(== '() n) (poso m)]
    [(== '(1) n) (>1o m)]
    [(fresh (x y)
      (== `(_ . ,x) n) (poso x)
      (== `(_ . ,y) m) (poso y)
      (<lo x y))]))

(defn <=lo [n m]
  (conde
    [(=lo n m)]
    [(<lo n m)]))

(defn <o [n m]
  (conde
    [(<lo n m)]
    [(=lo n m)
     (fresh (x)
       (poso x)
       (pluso n x m))]))

(defn <=o [n m]
  (conde
    [(== n m)]
    [(<o n m)]))
