(ns logic
  (:refer-clojure :exclude [==])
  (:require-macros [cljs.core.logic.macros :as m])
  (:require [goog.dom :as dom])
  (:use [cljs.core.logic :only [membero conso]]))

(def members
  (m/run* [q]
    (membero q '(:cat :dog :bird :bat :debra))))

(defn ^:export show-simple-logic []
  (set! (.-innerHTML (dom/getElement "msg")) (str "membero: " members)))


(defn lookupo [x env t]
  (m/fresh [rest y v]
    (conso `(~y ~v) rest env)
    (m/conde
      [(m/== y x) (m/== v t)]
      #_[(!= y x) (lookupo x rest t)])))

(def quines
  (m/run* [q]
          (lookupo 'y '((y 7) (x 5)) q)))

(defn ^:export show-quines []
  (set! (.-innerHTML (dom/getElement "quines")) (str "lookupo: " quines)))
