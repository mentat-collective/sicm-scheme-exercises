;; Exercise 1.23: Fill in the details
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_23.cljc :comments org
;; :END:


(ns ch1.ex1-23
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
