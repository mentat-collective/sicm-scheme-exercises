;; Exercise 1.26: Properties of $D_t$
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_26.cljc :comments org
;; :END:


(ns ch1.ex1-26
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
