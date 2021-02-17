;; Exercise 1.30: Orbits in a central potential
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_30.cljc :comments org
;; :END:


(ns ch1.ex1-30
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
