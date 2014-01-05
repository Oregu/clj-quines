(ns hello
  (:require [goog.dom :as dom]))

(defn ^:export do-hello []
  (set! (.-innerHTML (dom/getElement "msg")) "Hello, ClojureScript!"))
