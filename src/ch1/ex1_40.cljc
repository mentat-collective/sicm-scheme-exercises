;; Exercise 1.40: Bead on a triaxial surface
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_40.cljc :comments org
;; :END:


(ns ch1.ex1-40
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
