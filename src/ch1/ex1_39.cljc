;; Exercise 1.39: Combining Lagrangians
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_39.cljc :comments org
;; :END:


(ns ch1.ex1-39
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
