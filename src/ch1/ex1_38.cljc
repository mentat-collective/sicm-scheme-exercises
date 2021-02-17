;; Exercise 1.38: Properties of $E$
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_38.cljc :comments org
;; :END:


(ns ch1.ex1-38
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
