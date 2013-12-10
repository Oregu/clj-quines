(ns quines.core
  (:refer-clojure :exclude [== var?])
  (:use [clojure.core.logic]))

; (define a->s (lambda (a) (car a)))
; (define a->c* (lambda (a) (cadr a)))
; (define a->t (lambda (a) (caddr a)))

; (define-syntax lambdag@
;   (syntax-rules (:)
;     ((_ (a) e) (lambda (a) e))
;     ((_ (a : s c* t) e)
;      (lambda (a)
;        (let ((s (a->s a)) (c* (a->c* a)) (t (a->t a)))
;          e)))))


; (define var (lambda (dummy) (vector dummy)))
(defn var? [x] (vector? x))
; (define lhs (lambda (pr) (car pr)))
; (define rhs (lambda (pr) (cdr pr)))

(defn mzero [] nil)
(defn unit [a] a)
; (define choice (lambda (a f) (cons a f)))
; (define-syntax lambdaf@ 
;   (syntax-rules () ((_ () e) (lambda () e))))

(defn deep-tag? [tag]
  (not (or (= tag 'sym) (= tag 'num))))

(defn works-together? [t1 t2]
  (or (deep-tag? t1) (deep-tag? t2)))

(defn pr-t->pred [pr-t]
  (second (second pr-t)))

(defn pr-t->tag [pr-t]
  (first (second pr-t)))

(defn walk [x s]
  (let [a (find x s)]
    (cond
      a (let [u (second a)] (if (var? u) (walk u s) u))
      :else x)))

(defn ext-t [x tag pred s t-up]
  (let [x (walk x s)]
    (loop [t t-up]
      (cond
        (nil? t) (list `(~x . (~tag . ~pred)) t-up)
        (not= (walk (first (first t)) s) x) (recur (second t))
        (= (pr-t->tag  (first t)) tag) t-up
        (works-together? (pr-t->tag (first t)) tag)
          (recur (second t))
        :else false))))

(defn rem-dups [vars]
  (cond
    (nil? vars) '()
    (contains? (first vars) (second vars))
      (rem-dups (second vars))
    :else (list (first vars) (rem-dups (second vars)))))

(defn drop-from-t [x t]
  (filter
    (fn [pr-t]
      (not (and
        (= (first pr-t) x)
        (deep-tag? (pr-t->tag pr-t)))))
    t))

(defn new-c* [x tag c* s]
  (cond
    (some
      (fn [c]
        (and (nil? (second c))
          (= (walk (first (first c)) s) x)
          (= (second (first c)) tag)))
      c*)
      c*
    :else (list `((~x . ~tag)) c*)))

(defn subsumed-from-t-to-c* [x s c* t t-up]
  (cond
    (nil? t) `(~c* . ~t-up)
    :else
      (let [pr-t (first t)]
        (let [tag (pr-t->tag pr-t)
              y (first pr-t)]
          (cond
            (and (= y x) (deep-tag? tag))
              (subsumed-from-t-to-c* x s
                (new-c* x tag c* s)
                (second t)
                t-up)
            :else
              (subsumed-from-t-to-c* x s
                c*
                (second t)
                (list (first t) t-up)))))))

(defn have-flat-tag? [pred x]
  (fn [pr-t]
    (let [tag (pr-t->tag pr-t)]
      (and
        (not (deep-tag? tag))
        (= (first pr-t) x)
        (pred tag)))))

(defn subsume-c*-div-t [x s c* t]
  (cond
    (some (have-flat-tag? #(= % 'sym) x) t)
      (subsumed-from-t-to-c* x s c* t '())
    (some (have-flat-tag? #(not= % 'sym) x) t)
      `(~c* . ~(drop-from-t x t))
    :else `(~c* . ~t)))

(defn subsume-t [s c* t]
  (loop [x* (rem-dups (map first t)) c* c* t t]
    (cond
      (nil? x*) `(~s ~c* ~t)
      :else
        (let [c*-div-t (subsume-c*-div-t (first x*) s c* t)]
          (recur (second x*) (first c*-div-t) (second c*-div-t))))))

(defn subsumed-pr? [t]
  (fn [pr-c]
    (let [u (second pr-c)]
      (and (not (var? u))
        (let [x (first pr-c)]
          (let [pr-t (find x t)]
            (and pr-t
              (let [tag (pr-t->tag pr-t)]
                (cond
                  (and (deep-tag? tag) (= tag u))
                  (not ((pr-t->pred pr-t) u))
                  :else false)))))))))

(defn subsume [t c*]
  (filter #(not (some (subsumed-pr? t) %)) c*))

(declare noo-aux)

;; WRONG. NEXT IN THE LINE.
(defn noo [tag u]
  (let [pred #(not= % tag)]
    (fn [a] (let [s (get a 0) c* (get a 1) t (get a 2)] ; TODO This should be macro lambdag@
      (noo-aux tag u pred a s c* t)))))

(defn noo-aux [tag u pred a s c* t]
  (let [u (if (var? u) (walk u s) u)]
    (cond
      (and (list? u) (= (count u) 2))
        (cond
          (pred u)
            (let [a (noo-aux tag (first u) pred a s c* t)]
              (and a
                (((fn [a] (let [s (get a 0) c* (get a 1) t (get a 2)] ; TODO This should be macro lambdag@
                    (noo-aux tag (second u) pred a s c* t)))
                  a))))
         :else (mzero))
      (not (var? u))
        (cond
          (pred u) (unit a)
          :else (mzero))
      (ext-t u tag pred s t)
        (let [t0 (ext-t u tag pred s t)]
          (cond
            (not= t0 t)
              (let [t-up (list (first t0))]
                (let [c* (subsume t-up c*)]
                  (unit (subsume-t s c* t0)))))
            :else (unit a))
      :else (mzero))))

(defn symbolo [x] (pred x symbol?))
(defn numbero [x] (pred x number?))

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
      #_(trace-lvars "expo1" [exp env val])
      (== `(quote ~v) exp)
      (not-in-envo `quote env)
      (noo `closure v)
      (== v val))]
    [(fresh [a*]
      #_(trace-lvars "expo2" [exp env val])
      (conso `list a* exp)
      (not-in-envo `list env)
      (noo `closure a*)
      (proper-listo a* env val))]
    [(symbolo exp)
      (lookupo exp env val)]
    [(fresh [rator rand x body env* a env2]
      (== `(~rator ~rand) exp)
      (eval-expo rator env `(closure ~x ~body ~env*))
      (eval-expo rand env a)
      (== (lcons `(~x ~a) env*) env2)
      (eval-expo body env2 val))]
    [(fresh [x body]
      (== `(fn [~x] ~body) exp)
      (symbolo x)
      (not-in-envo `fn env)
      (== `(closure ~x ~body ~env) val))]))

(defn not-in-envo [x env]
  (conde
    [(fresh (y v rest)
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
    (eval-expo `(quote ~q) '() q)
    (== q 42)))

;; Evaluates to 5
(defn test-eval-2 []
  (run 1 [q]
    (eval-expo `((fn [x] x) z) `((z 5)) q)))

;; Evaluates to (fn z z)
(defn test-eval-3 []
  (run 1 [q] (eval-expo `(((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) `() q)))

;; Evaluates to 7
(defn test-eval-4 []
  (run 1 [q] (eval-expo `((((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) m) `((m 7)) q)))

;; Evaluates to 'm
(defn test-eval-5 []
  (run 1 [q] (eval-expo `((((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) m) `((~q 7)) 7)))

;; Evaluates to '((m 7))
(defn test-eval-6 []
  (run 3 [q] (eval-expo `((fn [a] a) m) q 7)))

;; DO NOT PASS
(defn test-quines-1 []
  (run 1 [q]
    (eval-expo q '() q)))

(def quine `((fn [x] (list x (list 'quote x)))
              '(fn [x] (list x (list 'quote x)))))

;; DO NOT PASS
(defn test-quines-2 []
  (run 1 [q]
    (eval-expo quine '() q)))

;; DO NOT PASS
(defn test-quines-3 []
  (run 1 [q]
    (eval-expo q '() quine)))
