;; Exercise 1.33: Falling off a log
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_33.cljc :comments org
;; :END:


(ns ch1.ex1-33
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
