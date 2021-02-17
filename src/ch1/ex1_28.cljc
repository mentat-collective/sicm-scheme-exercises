;; Exercise 1.28: Total Time Derivatives
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_28.cljc :comments org
;; :END:


(ns ch1.ex1-28
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
;; part A

;; TODO: Port the check-f stuff from utils.scm.

;; nice, easy to guess.


(defn FA [m]
  (fn [[_ x]]
    (* m x)))


;; #+RESULTS:
;; : #'ch1.ex1-28/FA

;; Show the function of t, and confirm that both methods are equivalent.


(check-f (FA 'm)
         (literal-function 'x))
;; Part B

;; NOT a total time derivative.

;; Define G directly:


(defn GB [m]
  (fn [[t _ v_x]]
    (let [GB0 0
          GB1 (* m (cos t))]
      (+ GB0 (* GB1 v_x)))))


;; #+RESULTS:
;; : #'ch1.ex1-28/GB

;; And show the full G, for fun:


(let [f (compose (GB 'm) (Gamma (literal-function 'x)))]
  (->tex-equation
   (f 't)))


;; #+RESULTS[854a7dd78cd3079bdf438b65c97ca014f49344db]:
;; :results:
;; \begin{equation}
;; m\,Dx\left(t\right)\,\cos\left(t\right)
;; \end{equation}
;; :end:

;; It's easier to confirm that this is not a total time derivative by checking the
;; partials.


(defn GB-properties [m]
  (let [GB0 (fn [local] 0)
        GB1 (fn [[t]]
              (* m (cos t)))]
    (G-properties
     GB0 GB1 (literal-function 'x))))


;; It's clear here that the second and third tuple entries aren't equal, so we
;; don't have a total time derivative.


(->tex-equation
 (GB-properties 'm))
;; Part C

;; no problem, we've got a total time derivative on our hands.


(defn FC [[t x]]
  (* x (cos t)))

(check-f FC (literal-function 'x))

(def GC-properties
  (let [GC0 (fn [[t q]]
              (* -1 q (sin t)))
        GC1 (fn [[t]]
              (cos t))]
    (G-properties
     GC0 GC1 (literal-function 'x))))


;; Boom, the second and third entries are equal, as we'd expect.


(->tex-equation GC-properties)
;; Part D

;; This is NOT a total time derivative; you can tell by taking the partials
;; of each side, G0 and G1, as we'll see here.


(def GD-properties
  (let [GD0 (fn [[t q]]
              (* q (sin t)))
        GD1 (fn [[t]]
              (cos t))]
    (G-properties
     GD0 GD1 (literal-function 'x))))


;; The partials for each side don't match.


(->tex-equation GD-properties)
;; Part E

;; This is strange to me, because I thought that this thing had to produce a tuple.

;; OH, but the secret is that Qdot is also a tuple, so you contract them together.

;; Here's the function F that we can use to derive it:


(defn FE [[t [x y]]]
  (* (+ (square x) (square y))
     (cos t)))


;; #+RESULTS:
;; : #'ch1.ex1-28/FE

;; Boom, total time derivative!


(check-f FE (up (literal-function 'x)
                (literal-function 'y)))


;; And let's show that we pass the tests by decomposing this into G0 and G1:


(def GE-properties
  (let [;; any piece of the function without a velocity multiplied.
        GE0 (fn [[t [x y]]]
              (* -1
                 (+ (square x) (square y))
                 (sin t)))



        GE1 (fn [[t [x y]]]
              (down
               (* 2 x (cos t))
               (* 2 y (cos t))))]
    (G-properties
     GE0 GE1 (up (literal-function 'x)
                 (literal-function 'y)))))


;; BOOM!

;; We've recovered F; the partials are equal, and the final matrix is symmetric.


(->tex-equation GE-properties)
;; Part F

;; This one is interesting, since the second partial is a tuple. This is not so
;; obvious to me, so first let's check the properties:


(def GF-properties
  (let [;; any piece of the function without a velocity multiplied.
        GF0 (fn [[t [x y]]]
              (* -1
                 (+ (square x) (square y))
                 (sin t)))



        G (fn [[t [x y]]]
            (down
             (+ (cube y) (* 2 x (cos t)))
             (+ x (* 2 y (cos t)))))]
    (G-properties
     GF0 GF1 (up (literal-function 'x)
                 (literal-function 'y)))))


;; AND it looks like we DO have a total time derivative, maybe. We certainly pass
;; the first test here, since the second and third tuple entries are equal.

;; BUT we fail the second test; the hessian that we get from ((partial 1) G1) is
;; not symmetric.


(->tex-equation GF-properties)
