;; Exercise 1.25: Foucalt pendulum Lagrangian
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_25.cljc :comments org
;; :END:


(ns ch1.ex1-25
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
