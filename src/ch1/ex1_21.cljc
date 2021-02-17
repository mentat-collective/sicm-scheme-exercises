;; Exercise 1.21: A dumbbell
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_21.cljc :comments org
;; :END:

;; The uneven dumbbell. We've just made it through four exercises which embrace the
;; idea that you can bake constraints into the coordinate transformation. But why
;; should we believe that this is allowed?

;; This exercise comes after a section called "Why it Works".

;; The next exercise tries to do a coordinate change that is really careful about
;; /not/ changing the dimension of the configuration space, so that we can show
;; that this move is allowed. Here's the setup:

;; #+DOWNLOADED: https://tgvaughan.github.io/sicm/images/Art_P166.jpg @ 2020-06-29 05:40:00
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-29_05-40-00_Art_P166.jpg]]


(ns ch1.ex1-21
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
;; Multiple Particle API

;; Many exercises have been dealing with multiple particles so far. Let's introduce
;; some functions that let us pluck the appropriate coordinates out of the local
;; tuple.

;; If we have the velocity and mass of a particle, its kinetic energy is easy to
;; define:

;; #+name: KE-particle

(defn KE-particle [m v]
  (* (/ 1 2) m (square v)))


;; #+RESULTS: KE-particle
;; : #'ch1.ex1-21/KE-particle

;; This next function, =extract-particle=, takes a number of components -- 2 for a
;; particle with 2 components, 3 for a particle in space, etc -- and returns a
;; function of =local= and =i=, a particle index. This function can be used to
;; extract a sub-local-tuple for that particle from a flattened list.

;; #+name: extract-particle

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
;; Part B: Dumbbell Lagrangian

;; #+begin_quote
;; Write the formal Lagrangian

;; \begin{equation}
;; L(t; x_0, y_0, x_1, y_1, F; \dot{x}_0, \dot{y}_0, \dot{x}_1, \dot{y}_1, \dot{F})
;; \end{equation}

;; such that Lagrange's equations will yield the Newton's equations you derived in
;; part *a*.
;; #+end_quote

;; Here is how we model constraint forces. Each pair of particles has some
;; constraint potential acting between them:

;; #+name: U-constraint

(defn U-constraint [q0 q1 F l]
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))


;; #+RESULTS: U-constraint
;; : #'ch1.ex1-21/U-constraint

;; And here's a Lagrangian for two free particles, subject to a constraint
;; potential $F$ acting between them.

;; #+name: L-free-constrained

(defn L-free-constrained [m0 m1 l]
  (fn [local]
    (let [extract      (extract-particle 2)
          [_ q0 qdot0] (extract local 0)
          [_ q1 qdot1] (extract local 1)
          F (ref (coordinate local) 4)]
      (- (+ (KE-particle m0 qdot0)
            (KE-particle m1 qdot1))
         (U-constraint q0 q1 F l)))))


;; #+RESULTS: L-free-constrained
;; : #'ch1.ex1-21/L-free-constrained

;; Finally, the path. This is rectangular coordinates for each piece, plus $F$
;; between them.


(def q-rect
  (up (literal-function 'x_0)
      (literal-function 'y_0)
      (literal-function 'x_1)
      (literal-function 'y_1)
      (literal-function 'F)))


;; #+RESULTS:
;; : #'ch1.ex1-21/q-rect

;; This shows the lagrangian itself, which answers part b:


(let [L (L-free-constrained 'm_0 'm_1 'l)
      f (compose L (Gamma q-rect))]
  (->tex-equation
   (f 't)))


;; #+RESULTS[e63be69cd32356758d422b2755c95ce3365d7f22]:
;; :results:
;; \begin{equation}\n\frac{l\,m_0\,{\left(Dx_0\left(t\right)\right)}^{2} + l\,m_0\,{\left(Dy_0\left(t\right)\right)}^{2} + l\,m_1\,{\left(Dx_1\left(t\right)\right)}^{2} + l\,m_1\,{\left(Dy_1\left(t\right)\right)}^{2} + {l}^{2}\,F\left(t\right) - F\left(t\right)\,{\left(x_1\left(t\right)\right)}^{2} + 2\,F\left(t\right)\,x_1\left(t\right)\,x_0\left(t\right) - F\left(t\right)\,{\left(x_0\left(t\right)\right)}^{2} - F\left(t\right)\,{\left(y_1\left(t\right)\right)}^{2} + 2\,F\left(t\right)\,y_1\left(t\right)\,y_0\left(t\right) - F\left(t\right)\,{\left(y_0\left(t\right)\right)}^{2}}{2\,l}\n\end{equation}
;; :end:

;; Here are the Lagrange equations, which, if you squint, are like Newton's
;; equations from part a.


(let [L (L-free-constrained 'm_0 'm_1 'l)
      f ((Lagrange-equations L) q-rect)]
  (->tex-equation
   (f 't)))
;; Part C: Coordinate Change

;; #+begin_quote
;; Make a change of coordinates to a coordinate system with center of mass
;; coordinates $x_{CM}$, $y_{CM}$, angle $\theta$, distance between the particles
;; $c$, and tension force $F$. Write the Lagrangian in these coordinates, and write
;; the Lagrange equations.
;; #+end_quote

;; This is a coordinate change that is very careful not to reduce the degrees of
;; freedom.

;; First, the coordinate change:


(defn cm-theta->rect [m0 m1]
  (fn [[_ [x_cm y_cm theta c F]]]
    (let [total-mass (+ m0 m1)
          m0-distance (* c (/ m1 total-mass))
          m1-distance (* c (/ m0 total-mass))]
      (up (- x_cm (* m0-distance (cos theta)))
          (- y_cm (* m0-distance (sin theta)))
          (+ x_cm (* m1-distance (cos theta)))
          (+ y_cm (* m1-distance (sin theta)))
          F))))


;; #+RESULTS:
;; : #'ch1.ex1-21/cm-theta->rect

;; Then the coordinate change applied to the local tuple:


(let [local (up 't
                (up 'x_cm 'y_cm 'theta 'c 'F)
                (up 'xdot_cm 'ydot_cm 'thetadot 'cdot 'Fdot))
      C (F->C (cm-theta->rect 'm_0 'm_1))]
  (->tex-equation
   (C local)))


;; #+RESULTS[da7af001803256839a245be72183aa93c38421b0]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{t} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{\frac{- c\,m_1\,\cos\left(\theta\right) + m_0\,x_{cm} + m_1\,x_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\frac{- c\,m_1\,\sin\left(\theta\right) + m_0\,y_{cm} + m_1\,y_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\frac{c\,m_0\,\cos\left(\theta\right) + m_0\,x_{cm} + m_1\,x_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\frac{c\,m_0\,\sin\left(\theta\right) + m_0\,y_{cm} + m_1\,y_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{F}\end{pmatrix}} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{\frac{c\,m_1\,\dot {\theta}\,\sin\left(\theta\right) - \dot c\,m_1\,\cos\left(\theta\right) + m_0\,{\dot x}_{cm} + m_1\,{\dot x}_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\frac{- c\,m_1\,\dot {\theta}\,\cos\left(\theta\right) - \dot c\,m_1\,\sin\left(\theta\right) + m_0\,{\dot y}_{cm} + m_1\,{\dot y}_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\frac{- c\,m_0\,\dot {\theta}\,\sin\left(\theta\right) + \dot c\,m_0\,\cos\left(\theta\right) + m_0\,{\dot x}_{cm} + m_1\,{\dot x}_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\frac{c\,m_0\,\dot {\theta}\,\cos\left(\theta\right) + \dot c\,m_0\,\sin\left(\theta\right) + m_0\,{\dot y}_{cm} + m_1\,{\dot y}_{cm}}{m_0 + m_1}} \cr \cr \displaystyle{\dot F}\end{pmatrix}}\end{pmatrix}\n\end{equation}
;; :end:

;; Then the Lagrangian in the new coordinates;


(defn L-free-constrained* [m0 m1 l]
  (compose (L-free-constrained m0 m1 l)
           (F->C (cm-theta->rect m0 m1))))


;; #+RESULTS:
;; : #'ch1.ex1-21/L-free-constrained*

;; This shows the lagrangian itself, after the coordinate transformation:


(let [q (up (literal-function 'x_cm)
            (literal-function 'y_cm)
            (literal-function 'theta)
            (literal-function 'c)
            (literal-function 'F))
      L (L-free-constrained* 'm_0 'm_1 'l)
      f (compose L (Gamma q))]
  (->tex-equation
   (f 't)))


;; #+RESULTS[62d1e8b27b38151e2129a897c3452f40326a4b41]:
;; :results:
;; \begin{equation}\n\frac{l\,m_0\,m_1\,{\left(D\theta\left(t\right)\right)}^{2}\,{\left(c\left(t\right)\right)}^{2} + l\,{m_0}^{2}\,{\left(Dx_{cm}\left(t\right)\right)}^{2} + l\,{m_0}^{2}\,{\left(Dy_{cm}\left(t\right)\right)}^{2} + 2\,l\,m_0\,m_1\,{\left(Dx_{cm}\left(t\right)\right)}^{2} + l\,m_0\,m_1\,{\left(Dc\left(t\right)\right)}^{2} + 2\,l\,m_0\,m_1\,{\left(Dy_{cm}\left(t\right)\right)}^{2} + l\,{m_1}^{2}\,{\left(Dx_{cm}\left(t\right)\right)}^{2} + l\,{m_1}^{2}\,{\left(Dy_{cm}\left(t\right)\right)}^{2} + {l}^{2}\,m_0\,F\left(t\right) + {l}^{2}\,m_1\,F\left(t\right) - m_0\,F\left(t\right)\,{\left(c\left(t\right)\right)}^{2} - m_1\,F\left(t\right)\,{\left(c\left(t\right)\right)}^{2}}{2\,l\,m_0 + 2\,l\,m_1}\n\end{equation}
;; :end:

;; Here are the Lagrange equations:


(let [q (up (literal-function 'x_cm)
            (literal-function 'y_cm)
            (literal-function 'theta)
            (literal-function 'c)
            (literal-function 'F))
      L (L-free-constrained* 'm_0 'm_1 'l)
      f ((Lagrange-equations L) q)]
  (->tex-equation
   (f 't)))
;; Part D: Substitute $c(t) = l$

;; #+begin_quote
;; You may deduce from one of these equations that $c(t) = l$. From this fact we
;; get that $Dc = 0$ and $D^2c = 0$. Substitute these into the Lagrange equations
;; you just computed to get the equation of motion for $x_{CM}$, $y_{CM}$,
;; $\theta$.
;; #+end_quote

;; We can substitute the constant value of $c$ using a function that always returns
;; $l$ to get simplified equations:


(let [q (up (literal-function 'x_cm)
            (literal-function 'y_cm)
            (literal-function 'theta)
            (fn [t] 'l)
            (literal-function 'F))
      L (L-free-constrained* 'm_0 'm_1 'l)
      f ((Lagrange-equations L) q)]
  (->tex-equation
   (f 't)))


;; #+RESULTS[2b3634b4f1f5384f5746863913dc914a5beed55a]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{m_0\,{D}^{2}x_{cm}\left(t\right) + m_1\,{D}^{2}x_{cm}\left(t\right)} \cr \cr \displaystyle{m_0\,{D}^{2}y_{cm}\left(t\right) + m_1\,{D}^{2}y_{cm}\left(t\right)} \cr \cr \displaystyle{\frac{{l}^{2}\,m_0\,m_1\,{D}^{2}\theta\left(t\right)}{m_0 + m_1}} \cr \cr \displaystyle{\frac{- l\,m_0\,m_1\,{\left(D\theta\left(t\right)\right)}^{2} + m_0\,F\left(t\right) + m_1\,F\left(t\right)}{m_0 + m_1}} \cr \cr \displaystyle{0}\end{bmatrix}\n\end{equation}
;; :end:

;; This is saying that the acceleration on the center of mass is 0.

;; The fourth equation, the equation of motion for the $c(t)$, is interesting here.
;; We need to pull in the definition of "reduced mass" from exercise 1.11:


(defn reduced-mass [m1 m2]
  (/ (* m1 m2)
     (+ m1 m2)))


;; #+RESULTS:
;; : #'ch1.ex1-21/reduced-mass

;; If we let $m$ be the reduced mass, this equation states:

;; \begin{equation}
;; \label{eq:constraint-force}
;; F(t) = m l \dot{\theta}^2
;; \end{equation}

;; We can verify this with Scheme by subtracting the two equations:


(let [F (literal-function 'F)
      theta (literal-function 'theta)
      q (up (literal-function 'x_cm)
            (literal-function 'y_cm)
            theta
            (fn [_] 'l)
            F)
      L (L-free-constrained* 'm_0 'm_1 'l)
      f ((Lagrange-equations L) q)
      m (reduced-mass 'm_0 'm_1)]
  (->tex-equation
   (- (ref (f 't) 3)
      (- (F 't)
         (* m 'l (square ((D theta) 't)))))))
;; Part E: New Lagrangian

;; #+begin_quote
;; Make a Lagrangian ($= T − V$) for the system described with the irredundant
;; generalized coordinates $x_{CM}$, $y_{CM}$, $\theta$ and compute the Lagrange
;; equations from this Lagrangian. They should be the same equations as you derived
;; for the same coordinates in part d.
;; #+end_quote

;; For part e, I wrote this in the notebook - it is effectively identical to the
;; substitution that is happening on the computer, so I'm going to ignore this. You
;; just get more cancellations.

;; But let's go at it, for fun.

;; Here's the Lagrangian of 2 free particles:


(defn L-free2 [m0 m1]
  (fn [local]
    (let [extract (extract-particle 2)
          [_ q0 qdot0] (extract local 0)
          [_ q1 qdot1] (extract local 1)]
      (+ (KE-particle m0 qdot0)
         (KE-particle m1 qdot1)))))


;; #+RESULTS:
;; : #'ch1.ex1-21/L-free2

;; Then a version of =cm-theta->rect= where we ignore $F$, and sub in a constant
;; $l$:


(defn cm-theta->rect* [m0 m1 l]
  (fn [[_ [x_cm y_cm theta]]]
    (let [total-mass (+ m0 m1)
          m0-distance (* l (/ m1 total-mass))
          m1-distance (* l (/ m0 total-mass))]
      (up (- x_cm (* m0-distance (cos theta)))
          (- y_cm (* m0-distance (sin theta)))
          (+ x_cm (* m1-distance (cos theta)))
          (+ y_cm (* m1-distance (sin theta)))))))


;; #+RESULTS[3bad9e32d2f15dcf0db2e30f2a5c510d3e4bff2f]:
;; :results:
;; #'ch1.ex1-21/cm-theta->rect*
;; :end:

;; The Lagrangian:


(defn L-free-constrained2 [m0 m1 l]
  (compose (L-free2 m0 m1)
           (F->C (cm-theta->rect* m0 m1 l))))


;; #+RESULTS:
;; : #'ch1.ex1-21/L-free-constrained2

;; Equations:


(let [q (up (literal-function 'x_cm)
            (literal-function 'y_cm)
            (literal-function 'theta)
            (fn [_] 'l)
            (literal-function 'F))
      L1 (L-free-constrained* 'm_0 'm_1 'l)
      L2 (L-free-constrained2 'm_0 'm_1 'l)]
  (->tex-equation
   ((- ((Lagrange-equations L1) q)
       ((Lagrange-equations L2) q))
    't)))
