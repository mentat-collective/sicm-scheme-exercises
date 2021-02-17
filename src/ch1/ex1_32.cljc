;; Exercise 1.32: Time-dependent constraints
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_32.cljc :comments org
;; :END:


(ns ch1.ex1-32
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
