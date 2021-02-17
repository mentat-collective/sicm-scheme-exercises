;; Exercise 1.29: Galilean Invariance
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_29.cljc :comments org
;; :END:


(ns ch1.ex1-29
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; I'll do this for a single particle, since it's annoying to get the sum going
;; for many; and the lagrangian is additive, so no problem.


(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* (/ 1 2) mass (square v)))))

(defn uniform-translate-shift->rect
  [[t [xprime delta_x delta_v]]]
  (+ xprime delta_x (* t delta_v)))

(defn L-translate-shift [m]
  (compose (L-free-particle m)
           (F->C uniform-translate-shift->rect)))


;; #+RESULTS:
;; | #'ch1.ex1-29/L-free-particle               |
;; | #'ch1.ex1-29/uniform-translate-shift->rect |
;; | #'ch1.ex1-29/L-translate-shift             |

;; First, confirm that if we have a constant, we get what we expected from paper.


(let [q (up (literal-function 'xprime)
            (fn [_] 'Delta_x)
            (fn [_] 'Delta_v))
      f (compose (L-translate-shift 'm) (Gamma q))]
  (->tex-equation (f 't)))


;; #+RESULTS[6d80ea55bf15d0e07af7448b1de2198d6e742243]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,{{\Delta}_v}^{2}\,m + {\Delta}_v\,m\,Dx^\prime\left(t\right) + \frac{1}{2}\,m\,{\left(Dx^\prime\left(t\right)\right)}^{2}\n\end{equation}
;; :end:

;; We can change this a little to see the extra terms; substract off the free
;; particle Lagrangian, to see the extra stuff.


(let [q (up (literal-function 'xprime)
            (fn [_] 'Delta_x)
            (fn [_] 'Delta_v))
      L (- (L-translate-shift 'm)
           (L-free-particle 'm))
      f (compose L (Gamma q))]
  (->tex-equation (f 't)))


;; #+RESULTS[6bd06cb88d4150ef03c736094abd817782efe2a4]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,{{\Delta}_v}^{2}\,m + {\Delta}_v\,m\,Dx^\prime\left(t\right)\n\end{equation}
;; :end:

;; Here's the gnarly version with both entries as actual functions. Can this be a
;; total time derivative? It CANNOT be, because we have a $(D \Delta_v(t))^2$ term
;; in there, and we know that total time derivatives have to be linear in the
;; velocities. The function $F$ would have had to have a velocity in it, which is
;; not allowed.


(let [q (up (literal-function 'xprime)
            (literal-function 'Delta_x)
            (literal-function 'Delta_v))
      L (- (L-translate-shift 'm)
           (L-free-particle 'm))
      f (compose L (Gamma q))]
  (->tex-equation (f 't)))


;; #+RESULTS[18dc1bcd401ff67e0be067f790f2c168f2185c00]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,m\,{t}^{2}\,{\left(D{\Delta}_v\left(t\right)\right)}^{2} + m\,t\,Dx^\prime\left(t\right)\,D{\Delta}_v\left(t\right) + m\,t\,D{\Delta}_x\left(t\right)\,D{\Delta}_v\left(t\right) + m\,t\,{\Delta}_v\left(t\right)\,D{\Delta}_v\left(t\right) + m\,Dx^\prime\left(t\right)\,D{\Delta}_x\left(t\right) + m\,Dx^\prime\left(t\right)\,{\Delta}_v\left(t\right) + m\,D{\Delta}_x\left(t\right)\,{\Delta}_v\left(t\right) + \frac{1}{2}\,m\,{\left({\Delta}_v\left(t\right)\right)}^{2} + \frac{-1}{2}\,m\,{\left(D{\Delta}_v\left(t\right)\right)}^{2}\n\end{equation}
;; :end:

;; Let's simplify by making the $\Delta_v$ constant and see if there's anything so
;; obvious about $\Delta_x$.

;; We know that we have a total derivative when $\Delta_x$ is constant, and we know
;; that total time derivatives are linear, so let's substract off the total time
;; derivative and see what happens:


(let [q (fn [dx]
          (up (literal-function 'xprime)
              dx
              (fn [_] 'Delta_v)))
      L (- (L-translate-shift 'm)
           (L-free-particle 'm))
      f (fn [dx]
          (compose L (Gamma (q dx))))]
  (->tex-equation
   ((- (f (literal-function 'Delta_x))
       (f (fn [_] 'Delta_x)))
    't)))
