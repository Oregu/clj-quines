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

(comment

(test-check "sums"
  (run 5 (q)
    (fresh (x y z)
      (pluso x y z)
      (== `(,x ,y ,z) q)))
  '((_.0 () _.0)
    (() (_.0 . _.1) (_.0 . _.1))
    ((1) (1) (0 1))
    ((1) (0 _.0 . _.1) (1 _.0 . _.1))
    ((1) (1 1) (0 0 1))))

(test-check "factors"
  (run* (q)
    (fresh (x y)
      (*o x y (build-num 24))
      (== `(,x ,y ,(build-num 24)) q)))
  '(((1) (0 0 0 1 1) (0 0 0 1 1))
    ((0 0 0 1 1) (1) (0 0 0 1 1))
    ((0 1) (0 0 1 1) (0 0 0 1 1))
    ((0 0 1) (0 1 1) (0 0 0 1 1))
    ((0 0 0 1) (1 1) (0 0 0 1 1))
    ((1 1) (0 0 0 1) (0 0 0 1 1))
    ((0 1 1) (0 0 1) (0 0 0 1 1))
    ((0 0 1 1) (0 1) (0 0 0 1 1))))
)