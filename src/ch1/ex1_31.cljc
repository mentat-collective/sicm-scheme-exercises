;; Exercise 1.31: Foucault pendulum evolution
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_31.cljc :comments org
;; :END:


(ns ch1.ex1-31
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
