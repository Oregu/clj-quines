(ns quines.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn noo [tag u] (predc u (fn [x] (clojure.core/not= (if (seq? x) (first x) x) tag))))
(defn symbolo [x] (predc x symbol?))
(defn numbero [x] (predc x number?))

(declare not-in-envo)
(declare proper-listo)

(defn lookupo [x env t]
  (fresh [rest y v]
    (conso `(~y ~v) rest env)
    (conde
      [(== y x) (== v t)]
      [(!= y x) (lookupo x rest t)])))

(defn eval-expo [exp env val]
  (conde
    [(fresh [v]
      (== `(~'quote ~v) exp)
      (not-in-envo 'quote env)
      (noo 'closure v)
      (== v val))]
    [(fresh [a*]
      (conso 'list a* exp)
      (not-in-envo 'list env)
      (noo 'closure a*)
      (proper-listo a* env val))]
    [(symbolo exp)
      (lookupo exp env val)]
    [(fresh [rator rand x body env- a env2]
      (== `(~rator ~rand) exp)
      (eval-expo rator env `(~'closure ~x ~body ~env-))
      (eval-expo rand env a)
      (conso `(~x ~a) env- env2)
      (eval-expo body env2 val))]
    [(fresh [x body]
      (== `(~'fn [~x] ~body) exp)
      (symbolo x)
      (not-in-envo 'fn env)
      (== `(~'closure ~x ~body ~env) val))]))

(defn not-in-envo [x env]
  (conde
    [(fresh [y v rest]
      (conso `(~y ~v) rest env)
      (!= y x)
      (not-in-envo x rest))]
    [(== '() env)]))

(defn proper-listo [exp env val]
  (conde
    [(== '() exp)
     (== '() val)]
    [(fresh [a d t-a t-d]
       (conso a d exp)
       (conso t-a t-d val)
       (eval-expo a env t-a)
       (proper-listo d env t-d))]))


(defn test-eval-1 []
  (run 1 [q]
    (eval-expo `(~'quote ~q) '() q)
    (== q 42)))

;; Evaluates to 5
(defn test-eval-2 []
  (run 1 [q]
    (eval-expo '((fn [x] x) z) '((z 5)) q)))

;; Evaluates to (fn z z)
(defn test-eval-3 []
  (run 1 [q] (eval-expo '(((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) '() q)))

;; Evaluates to 7
(defn test-eval-4 []
  (run 1 [q] (eval-expo '((((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) m) '((m 7)) q)))

;; Evaluates to 'm
(defn test-eval-5 []
  (run 1 [q] (eval-expo '((((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) m) `((~q 7)) 7)))

;; Evaluates to '((m 7))
(defn test-eval-6 []
  (run 3 [q] (eval-expo '((fn [a] a) m) q 7)))

(def quine '((fn [x] (list x (list 'quote x)))
            '(fn [x] (list x (list 'quote x)))))

(defn test-quines-1 []
  (run 1 [q]
    (eval-expo q '() q)))

(defn test-quines-2 []
  (run 1 [q]
    (eval-expo quine '() q)))

(defn test-quines-3 []
  (run 1 [q]
    (eval-expo q '() quine)))

(defn test-twines-1 []
  (run 1 [r] (fresh [p q]
    (eval-expo p '() q) (eval-expo q '() p) (!= p q)
    (== r [p q]))))

(defn test-thrines-1 []
  (run 1 [x]
    (fresh (p q r)
      (!= p q)
      (!= q r)
      (!= r p)
      (eval-expo p '() q)
      (eval-expo q '() r)
      (eval-expo r '() p)
      (== [p q r] x))))
