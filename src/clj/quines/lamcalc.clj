(ns quines.lamcalc
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [quines.core]))

(def lam-zero '(fn [x] (x x)))
(def lam-succ '(fn [n] (fn [f] (fn [x] (f ((n f) x))))))

(def lam-one
  (first (run 1 [q] (eval-expo `(~lam-succ ~lam-zero) '() q))))