(ns quines.interp)

; Non-relational interpreter

(defn lookup [x env]
  (cond
  	(empty? env) nil
  	(= x (first (first env))) (second (first env))
  	:else (lookup x (rest env))))

(defn not-in-env [x env]
  (cond
    (empty? env) true
    (= x (first (first env))) false
    :else (not-in-env x (rest env))))

(defn eval-exp [exp env]
  (cond
    (symbol? exp)
      (lookup exp env)
    (and (= 'fn (first exp)) (not-in-env 'fn env))
      (let [x (second exp) body (first (next (next exp)))]
        ['closure x body env])
    :else
      (let [[rator rand] exp
        proc (eval-exp rator env)
        arg (eval-exp rand env)]
        (when (= 'closure (first proc))
          (let [[cl x body env2] proc]
            (eval-exp body (into [[x arg]] env2)))))))

(defn test-1 []
  (eval-exp '[[[fn x [fn y x]] [fn z z]] [fn a a]] []))

(defn test-2 []
  (eval-exp
    '[[fn fn [fn fn] [fn fn]]
      [fn fn [fn fn] [fn fn]]] []))
