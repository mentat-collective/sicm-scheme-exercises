;; Exercise 9.2: Computing Derivatives
;;      :PROPERTIES:
;;      :header-args+: :tangle src/ch9/ex9-2.cljc :comments org
;;      :END:


(ns ch1.ex9-2
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)



;; #+RESULTS:

;; A further exercise is to try defining the functions so that they use explicit
;; tuples, so you can compose them:


(defn F* [[x y]]
  (* (square x) (cube y)))

(defn G* [[_ y :as v]]
  (up (F* v) y))

(def H* (compose F* G*))



;; #+RESULTS:
;; | #'ch1.ex9-2/F* |
;; | #'ch1.ex9-2/G* |
;; | #'ch1.ex9-2/H* |

;; to be really pro, I'd make a function that takes these as arguments and prints a
;; nice formatted exercise output. Let's do the final exercise, for fun:


(->tex-equation
 (up ((D F*) (up 'a 'b))
     ((D G*) (up 3 5))
     ((D H*) (up (* 3 (square 'a)) (* 5 (cube 'b))))))
