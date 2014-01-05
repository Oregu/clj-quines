(ns logic1
  (:require-macros [cljs.core.logic.macros :as m])
  (:require [goog.dom :as dom])
  (:use [cljs.core.logic :only [membero]]))

(def members
  (m/run* [q]
    (membero q '(:cat :dog :bird :bat :debra))))

(defn ^:export do-logic []
  (set! (.-innerHTML (dom/getElement "logic-msg")) members))
