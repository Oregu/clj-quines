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

(defn substo [body vr vl body-ex]
  (conde
    [(symbolo body)
     (== body vr)
     (== body-ex vl)]
    [(symbolo body)
     (!= body vr)
     (== body body-ex)]
    [(numbero body)
     (== body body-ex)]
    [(fresh [rator rand rator-ex rand-ex]
      (== `(~rator ~rand) body)
      (substo rator vr vl rator-ex) ;; TODO: if rator is fn then apply it
      (substo rand vr vl rand-ex)
      (== body-ex `(~rator-ex ~rand-ex)))]
    [(fresh [x body2 body2-ex]
      (== `(~'fn [~x] ~body2) body)
      (substo body2 vr vl body2-ex) ;; TODO do not substitute bound variables
      (== body-ex `(~'fn [~x] ~body2-ex)))]))

(defn reduceo [body env body-ex] ;; TODO trim environment
  (conde
    [(emptyo env)
     (== body body-ex)]
    [(fresh [vr vl env-t body2]
      (conso `(~vr ~vl) env-t env)
      (substo body vr vl body2)
      (reduceo body2 env-t body-ex))]))

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
      (trace-lvars "symbolo" [exp val])
      (lookupo exp env val)]
    [(fresh [rator rand x body env- a env2]
      (== `(~rator ~rand) exp)
      (trace-lvars "app" [rator rand env])
      (eval-expo rator env `(~'closure ~x ~body ~env-))
      (eval-expo rand env a)
      (conso `(~x ~a) env- env2)
      (trace-lvars "app-eval" [body env2 val])
      (eval-expo body env2 val))]
    [(fresh [x body body-ex env2]
      (== `(~'fn [~x] ~body) exp)
      (trace-lvars "func" [x body env])
      (symbolo x)
      (not-in-envo 'fn env)
      (reduceo body env body-ex)
      (== `(~'closure ~x ~body-ex ~env) val))]))

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
