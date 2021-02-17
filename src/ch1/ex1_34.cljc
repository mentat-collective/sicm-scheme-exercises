;; Exercise 1.34: Driven spherical pendulum
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_34.cljc :comments org
;; :END:


(ns ch1.ex1-34
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
