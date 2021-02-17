;; Exercise 1.27: Lagrange equations for total time derivatives
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_27.cljc :comments org
;; :END:


(ns ch1.ex1-27
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
