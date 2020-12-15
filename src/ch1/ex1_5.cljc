;; Exercise 1.5: Solution process
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_5.cljc :comments org
;; :END:

;; The goal of this The goal of this exercise is to watch the minimization process
;; that we just discussed proceed, from the initial guess of a straight-line path
;; to the final, natural looking harmonic oscillation.


(ns ch1.ex1-5
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


;; #+RESULTS:
;; : nilnil{:level :fatal, :ns-whitelist [], :ns-blacklist [], :ns-log-level [], :middleware [], :timestamp-opts {:pattern :iso8601, :locale :jvm-default, :timezone :utc}, :output-fn #function[taoensso.timbre/default-output-fn], :appenders {:println {:enabled? true, :async? false, :min-level nil, :rate-limit nil, :output-fn :inherit, :fn #function[taoensso.timbre.appenders.core/println-appender/fn--11899]}}}#'ch1.ex1-5/->tex-equation*#'ch1.ex1-5/->tex-equation

;; The exercise states:

;; #+begin_quote
;; We can watch the progress of the minimization by modifying the procedure
;; parametric-path-action to plot the path each time the action is computed.
;; #+end_quote

;; The functions the authors provide in the exercise define a window, and then a
;; version of =parametric-path-action= that updates the graph as it minimizes:


(define win2 (frame 0.0 :pi/2 0.0 1.2))

(defn L-harmonic [m k]
  (fn [local]
    (let [q (coordinate local)
          v (velocity local)]
      (- (* (/ 1 2) m (square v))
         (* (/ 1 2) k (square q))))))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1)
         intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))

    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))

    (Lagrangian-action Lagrangian path t0 t1)))


;; #+RESULTS:
;; : #| win2 |#
;; :
;; : #| L-harmonic |#
;; :
;; : #| parametric-path-action |#

;; Run the minimization with the same parameters as in the previous section:


(find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 2)
