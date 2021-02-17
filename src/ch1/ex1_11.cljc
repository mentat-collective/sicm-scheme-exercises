;; Exercise 1.11: Kepler's third law
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_11.cljc :comments org
;; :END:

;; TODO continue from here.

;; This exercise asks us to derive [[https://en.wikipedia.org/wiki/Kepler%27s_laws_of_planetary_motion#Third_law_of_Kepler][Kepler's third law]] by considering a Lagrangian
;; that describes two particles rotating in a circular orbit around their center of
;; mass at some rate.


(ns ch1.ex1-11
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; Here's the Lagrangian for "central force", in polar coordinates. This is
;; rotational kinetic energy, minus some arbitrary potential $V$ that depends on
;; the distance $r$ between the two particles.


(defn L-central-polar [m V]
  (fn [[_ [r phi] [rdot phidot]]]
    (let [T (* (/ 1 2)
               m
               (+ (square rdot)
                  (square (* r phidot))))]
      (- T (V r)))))


;; #+RESULTS:
;; : #'ch1.ex1-11/L-central-polar

;; This function defines gravitational potential energy:


(defn gravitational-energy [G m1 m2]
  (fn [r]
    (- (/ (* G m1 m2) r))))


;; #+RESULTS:
;; : #'ch1.ex1-11/gravitational-energy

;; What is the mass $m$ in the Lagrangian above? It's the "[[https://en.wikipedia.org/wiki/Reduced_mass][reduced mass]]", totally
;; unjustified at this point in the book:


(defn reduced-mass [m1 m2]
  (/ (* m1 m2)
     (+ m1 m2)))


;; #+RESULTS:
;; : #'ch1.ex1-11/reduced-mass

;; If you want to see why the reduced mass has the form it does, check out [[https://en.wikipedia.org/wiki/Reduced_mass#Lagrangian_mechanics][this
;; derivation]].

;; The Lagrangian is written in terms of some angle $\phi$ and $r$, the distance
;; between the two particles. $q$ defines a circular path:


(defn q [r omega]
  (fn [t]
    (let [phi (* omega t)]
      (up r phi))))


;; #+RESULTS:
;; : #'ch1.ex1-11/q

;; Write the Lagrange equations, given $r = a$ and $\omega = n$:


(let [eqfn (Lagrange-equations
            (L-central-polar (reduced-mass 'm1 'm2)
                             (gravitational-energy 'G 'm1 'm2)))]
  (->tex-equation
   ((eqfn (q 'a 'n)) 't)))


;; #+RESULTS[32acd89d915bf30f67fe60876abf42dd043e6cf0]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{\frac{- {a}^{3}\,\mathsf{m1}\,\mathsf{m2}\,{n}^{2} + G\,{\mathsf{m1}}^{2}\,\mathsf{m2} + G\,\mathsf{m1}\,{\mathsf{m2}}^{2}}{{a}^{2}\,\mathsf{m1} + {a}^{2}\,\mathsf{m2}}} \cr \cr \displaystyle{0}\end{bmatrix}\n\end{equation}
;; :end:

;; These two entries are /residuals/, equal to zero. Stare at the top residual and
;; you might notice that you can can factor out:

;; - the reduced mass, and
;; - a factor of $1 \over a^2$

;; Manually factor these out:


(let [eqfn (Lagrange-equations
            (L-central-polar
             (reduced-mass 'm1 'm2)
             (gravitational-energy 'G 'm1 'm2)))]
  (->tex-equation
   (* ((eqfn (q 'a 'n)) 't)
      (/ (square 'a)
         (reduced-mass 'm1 'm2)))))
