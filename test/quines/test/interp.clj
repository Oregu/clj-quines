(ns quines.test.interp
  (:use quines.interp clojure.test))

(deftest test-eval
  ;; Evaluates to (fn z z)
  (is (= '(closure z z [])
    (eval-exp '(((fn x (fn y x)) (fn z z)) (fn a a)) [])))

  (is (= 8 (eval-exp '((((fn x (fn y x)) (fn z z)) (fn a a)) m) [['m 8]]))))


;; Will go to infinite loop, it's Big Omega time
;; Can't test it, but that's cool actually

(def big-omega
  '((fn fn (fn fn)
    (fn fn (fn fn)))))

(defn test-big []
  (eval-exp big-omega []))
