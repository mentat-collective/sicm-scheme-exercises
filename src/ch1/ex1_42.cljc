;; Exercise 1.42: Augmented Lagrangian
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_42.cljc :comments org
;; :END:


(ns ch1.ex1-42
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
