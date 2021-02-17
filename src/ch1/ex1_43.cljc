;; Exercise 1.43: A numerical investigation
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_43.cljc :comments org
;; :END:


(ns ch1.ex1-43
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
