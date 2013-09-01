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
        (list 'closure x body env))
    :else
      (let [[rator rand] exp
        proc (eval-exp rator env)
        arg (eval-exp rand env)]
        (when (= 'closure (first proc))
          (let [[_ x body env2] proc]
            (eval-exp body (into [[x arg]] env2)))))))

(def big-omega
  '((fn fn (fn fn)
    (fn fn (fn fn)))))

;; Evaluates to (fn z z)
(defn test-1 []
  (eval-exp '(((fn x (fn y x)) (fn z z)) (fn a a)) []))

;; Will go to infinite loop, it's Big Omega time
(defn test-2 []
  (eval-exp big-omega []))
