(ns quines.test.core
  (:refer-clojure :exclude [==])
  (:use quines.core
        [clojure.core.logic :exclude [is] :as l]
        clojure.test))

(deftest eval-quote
  (is (= 42
    (first (run 1 [q]
      (eval-expo `(~'quote ~q) '() q)
      (== q 42))))))

(deftest eval-lambda-with-env
  (is (= 5
    (first (run 1 [q]
      (eval-expo '((fn [x] x) z) '((z 5)) q)))))
  (is (= 7
    (first (run 1 [q]
      (eval-expo '((((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) m) '((m 7)) q))))))

(deftest eval-lambda
  (is (= '(closure z z ())
    (first (run 1 [q]
      (eval-expo '(((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) '() q))))))

(deftest eval-env
  (is (= 'm
    (first (run 1 [q]
      (eval-expo '((((fn [x] (fn [y] x)) (fn [z] z)) (fn [a] a)) m) `((~q 7)) 7)))))
  (is (= '((m 7))
    (first (run 3 [q]
      (eval-expo '((fn [a] a) m) q 7))))))

(deftest test-eval-quine
  (let [quine '((fn [x] (list x (list 'quote x)))
               '(fn [x] (list x (list 'quote x))))]

    (is (= quine
      (first (run 1 [q]
        (eval-expo quine '() q)))))

    (is (= `(~'quote ~quine)
      (first (run 1 [q]
        (eval-expo q '() quine)))))))

(deftest test-quine-gen
  (let [quine-gen (first (first (run 1 [q] ; second first is to get func itself, others are constraints
                                     (eval-expo q '() q))))]

    (is (= quine-gen (first (run 1 [q] (eval-expo quine-gen '() q)))))))

(deftest test-quines-eval
  (let [quines (run 3 [q] (eval-expo q '() q))
        q1 (first (first quines))
        q2 (first (second quines))
        q3 (first (nth quines 2))]
    (is (= q1 (eval q1)))
    (is (= q2 (eval q2)))
    (is (= q3 (eval q3)))))

(deftest test-twines
  (let [twines (first (first
            (run 1 [r] (fresh [p q]
              (eval-expo p '() q)
              (eval-expo q '() p)
              (!= p q)
              (== r [p q])))))
        [pp qq] twines]

    (is (and (= qq (eval pp)) (= pp (eval qq))))))

(deftest test-thrines
  (let [thrines (first (first
          (run 1 [x]
            (fresh (p q r)
              (!= p q)
              (!= q r)
              (!= r p)
              (eval-expo p '() q)
              (eval-expo q '() r)
              (eval-expo r '() p)
              (== [p q r] x)))))
        [pp qq rr] thrines]

    (is (and (= qq (eval pp)) (= rr (eval qq)) (= pp (eval rr))))))
