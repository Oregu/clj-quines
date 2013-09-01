(ns quines.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn noo [tag u]
  (let [pred #(not (eq? # tag))]
    (lambdag@ (a : s c* t)
      (noo-aux tag u pred a s c* t))))

(defn noo-aux [tag u pred a s c* t]
  (let ((u (if (var? u) (walk u s) u)))
    (cond
      ((pair? u)
       (cond
         ((pred u)
          (let ((a (noo-aux tag (car u) pred a s c* t)))
            (and a
              ((lambdag@ (a : s c* t)
                 (noo-aux tag (cdr u) pred a s c* t))
               a))))
         (else (mzero))))
      ((not (var? u))
       (cond
         ((pred u) (unit a))
         (else (mzero))))
      ((ext-t u tag pred s t) =>
       (lambda (t0)
         (cond
           ((not (eq? t0 t))
            (let ((t^ (list (car t0))))
              (let ((c* (subsume t^ c*)))
                (unit (subsume-t s c* t0)))))
           (else (unit a)))))
      (else (mzero)))))

(declare not-in-envo)

(defn eval-expo [exp env val]
  (conde
    ((fresh (v)
       (== `(quote ,v) exp)
       (not-in-envo 'quote env)
       (noo 'closure v)
       (== v val)))
    ((fresh (a*)
       (== `(list . ,a*) exp)
       (not-in-envo 'list env)
       (noo 'closure a*)
       (proper-listo a* env val)))
    ((symbolo exp) (lookupo exp env val))
    ((fresh (rator rand x body env* a)
       (== `(,rator ,rand) exp)
       (eval-expo rator env `(closure ,x ,body ,env*))
       (eval-expo rand env a)
       (eval-expo body `((,x . ,a) . ,env*) val)))
    ((fresh (x body)
       (== `(lambda (,x) ,body) exp)
       (symbolo x)
       (not-in-envo 'lambda env)
       (== `(closure ,x ,body ,env) val)))))

(defn not-in-envo [x env]
  (conde
    [(fresh (y v rest)
       (== `((,y . ,v) . ,rest) env)
       (!= y x)
       (not-in-envo x rest))]
    [(== '() env)]))

(defn proper-listo [exp env val]
  (conde
    [(== '() exp)
     (== '() val)]
    [(fresh (a d t-a t-d)
       (== `(,a . ,d) exp)
       (== `(,t-a . ,t-d) val)
       (eval-expo a env t-a)
       (proper-listo d env t-d))]))

(defn lookupo [x env t]
  (fresh (rest y v)
    (== `((,y . ,v) . ,rest) env)
    (conde
      [(== y x) (== v t)]
      [(!= y x) (lookupo x rest t)])))
