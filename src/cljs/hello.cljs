(ns hello
  (:require [goog.dom :as dom]))

(set! (.-innerHTML (dom/getElement "msg")) "Hello, ClojureScript!")
