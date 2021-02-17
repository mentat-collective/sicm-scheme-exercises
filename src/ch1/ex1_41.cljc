;; Exercise 1.41: Motion of a tiny golf ball
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_41.cljc :comments org
;; :END:


(ns ch1.ex1-41
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
