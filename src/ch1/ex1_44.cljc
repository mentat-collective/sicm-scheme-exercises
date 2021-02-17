;; Exercise 1.44: Double pendulum behavior
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_44.cljc :comments org
;; :END:


(ns ch1.ex1-44
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
