;; Exercise 1.36: Noether integral
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_36.cljc :comments org
;; :END:


(ns ch1.ex1-36
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
