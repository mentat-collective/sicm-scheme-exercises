;; Exercise 1.35: Restricted equations of motion
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_35.cljc :comments org
;; :END:


(ns ch1.ex1-35
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
