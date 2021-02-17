;; Exercise 1.37: Velocity transformation
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_37.cljc :comments org
;; :END:


(ns ch1.ex1-37
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
