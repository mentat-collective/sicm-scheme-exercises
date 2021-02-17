;; Exercise 1.24: Constraint forces
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_24.cljc :comments org
;; :END:

;; This is a special case of a solution we found in exercise 1.22. In that
;; exercise, we found the constraint forces on a driven pendulum. By setting
;; $y_s(t) = l$, we can read off the constraint forces for the undriven pendulum.


(ns ch1.ex1-24
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; Take some definitions that we need:


(defn U-constraint [q0 q1 F l]
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))

(defn extract-particle [pieces]
  (fn [[t q v] i]
    (let [indices (take pieces
                        (iterate
                         inc (* i pieces)))
          extract (fn [tuple]
                    (mapv (fn [i]
                            (ref tuple i))
                          indices))]
      (up t (extract q) (extract v)))))

(defn KE-particle [m v]
  (* (/ 1 2) m (square v)))

(defn L-driven-free [m l y U]
  (fn [local]
    (let [extract (extract-particle 2)
          [_ q qdot] (extract local 0)
          F (ref (coordinate local) 2)]
      (- (KE-particle m qdot)
         (U q)
         (U-constraint (up 0 (y (state->t local)))
                       q
                       F
                       l)))))

(defn U-gravity [g m]
  (fn [[_ y]]
    (* m g y)))

(defn driven-polar->rect [y]
  (fn [[t [theta c F]]]
    (up (* c (sin theta))
        (- (y t) (* c (cos theta)))
        F)))

(defn L-driven-pend [m l y U]
  (compose (L-driven-free m l y U)
           (F->C (driven-polar->rect y))))


;; #+RESULTS:
;; | #'ch1.ex1-24/U-constraint       |
;; | #'ch1.ex1-24/extract-particle   |
;; | #'ch1.ex1-24/KE-particle        |
;; | #'ch1.ex1-24/L-driven-free      |
;; | #'ch1.ex1-24/U-gravity          |
;; | #'ch1.ex1-24/driven-polar->rect |
;; | #'ch1.ex1-24/L-driven-pend      |

;; The second equation of motion, for the $c$ coordinate, gives us an equation in
;; terms of tension. Substitute in a constant pendulum support position by defining
;; the support position function to be =(lambda (t) 'l)=:


(let [q (up (literal-function 'theta)
            (fn [_] 'l)
            (literal-function 'F))
      y (fn [_] 'l)
      L (L-driven-pend 'm 'l y (U-gravity 'g 'm))
      f ((Lagrange-equations L) q)]
  (->tex-equation
   (ref (f 't) 1)))
