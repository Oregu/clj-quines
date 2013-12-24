(ns quines.test.numbers
  (:refer-clojure :exclude [==])
  (:use quines.numbers
        [clojure.core.logic :exclude [is] :as l]
        clojure.test))

(deftest test-poso
  (is (not (first (run 1 [q] (poso zeroo)))))
  (is (first (run 1 [q] (poso (build-num 1))))))

(deftest test->1o
  (is (not (first (run 1 [q] (>1o (build-num 1))))))
  (is (first (run 1 [q] (>1o (build-num 2))))))

(deftest test-pluso
  (is (= (build-num 7) (first (run 1 [q]
    (pluso (build-num 2) (build-num 5) q))))))

(deftest test-minuso
  (is (= (build-num 9) (first (run 1 [q]
    (minuso q (build-num 5) (build-num 4)))))))

(deftest test-*o
  (is (= (build-num 6) (first (run 1 [q]
    (*o (build-num 2) (build-num 3) q)))))

  (is (= (build-num 2) (first (run 1 [q]
    (*o q (build-num 2) (build-num 4))))))

  (is (= (build-num 11) (first (run 1 [q]
    (*o (build-num 5) q (build-num 55))))))

  (is (= (build-num 11) (first (run 1 [q]
    (*o q q (build-num 121)))))))

(deftest test-<o
  (is (not (first (run 1 [q]
    (<o (build-num 7) (build-num 3))))))

  (is (first (run 1 [q]
    (<o (build-num 2) (build-num 9))))))

(deftest test-divideo
  (let [[q r] (first (run 1 [q r]
                (divideo (build-num 7) (build-num 3) q r)))]
    (is (and (= q (build-num 2)) (= r (build-num 1)))))

  (let [[q r] (first (run 1 [q r]
                (divideo (build-num 18) (build-num 3) q r)))]
    (is (and (= q (build-num 6)) (= r '()))))

  (let [[q r] (first (run 1 [q r]
                (divideo (build-num 3) (build-num 7) q r)))]
    (is (and (= q '()) (= r (build-num 3))))))

(deftest test-logo
  (let [[q r] (first (run 1 [q r]
                (logo (build-num 16) (build-num 2) q r)))]
    (is (and (= q '(0 0 1)) (= r '()))))

  (let [[q r] (first (run 1 [q r]
                (logo (build-num 1000) (build-num 10) q r)))]
    (is (and (= q (build-num 3)) (= r '()))))

  (is (= '(1 1) (first (run 1 [q]
                (logo (build-num 9) q (build-num 2) '()))))))

(deftest test-expo
  (is (= (build-num 8) (first (run 1 [q]
                (expo (build-num 2) (build-num 3) q)))))

  (is (= (build-num 11) (first (run 1 [q]
                (expo q (build-num 2) (build-num 121))))))

  (is (= (build-num 10) (first (run 1 [q]
                (expo (build-num 2) q (build-num 1024)))))))

(deftest test-factors
  (let [factors (run* (q) (fresh (x y)
                  (*o x y (build-num 24))
                  (== [x y] q)))]

    (is (= 8 (count factors)))
    (is (some #{['(1) '(0 0 0 1 1)]} factors))   ;; 1, 24
    (is (some #{['(0 0 0 1 1) '(1)]} factors))   ;; 24, 1
    (is (some #{['(0 1) '(0 0 1 1)]} factors))   ;; 2, 12
    (is (some #{['(0 0 1) '(0 1 1)]} factors))   ;; 4, 6
    (is (some #{['(0 0 0 1) '(1 1)]} factors))   ;; 8, 3
    (is (some #{['(1 1) '(0 0 0 1)]} factors))   ;; 3, 8
    (is (some #{['(0 1 1) '(0 0 1)]} factors))   ;; 6, 4
    (is (some #{['(0 0 1 1) '(0 1)]} factors)))) ;; 12, 2
