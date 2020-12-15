;; Exercise 1.32: Time-dependent constraints
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_32.cljc :comments org
;; :END:


(ns ch1.ex1-32
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]
            [sicmutils.expression.render :as render]
            [taoensso.timbre :refer [set-level!]]))

(e/bootstrap-repl!)
(set-level! :fatal)

(defn ->tex-equation* [e]
  (let [eq (render/->TeX (simplify e))]
    (str "\\begin{equation}\n"
         eq
         "\n\\end{equation}")))

(defn ->tex-equation [e]
  (println
   (->tex-equation* e)))
