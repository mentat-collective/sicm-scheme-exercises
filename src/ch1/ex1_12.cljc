;; Exercise 1.12: Lagrange's equations (code)
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_12.cljc :comments org
;; :END:


(ns ch1.ex1-12
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; This exercise asks us to write Scheme implementations for each of the three
;; systems described in [[https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-9][Exercise 1.9]].

;; Before we begin, here is a function that will display an up-tuple of:

;; - $\partial_1 L \circ \Gamma[q]$, the generalized force
;; - $\partial_2 L \circ \Gamma[q]$, the generalized momenta
;; - $D(\partial_2 L \circ \Gamma[q])$, the derivative of our momenta
;; - The Lagrange equations for the system.


(defn lagrange-equation-steps [L q]
  (let [p1 (compose ((partial 1) L) (Gamma q))
        p2 (compose ((partial 2) L) (Gamma q))
        dp2 (D p2)]
    (->tex-equation
     ((up p1 p2 dp2 (- dp2 p1))
      't))))
;; Part A: Ideal Planar Pendulum

;; From the book:

;; #+begin_quote
;; An ideal planar pendulum consists of a bob of mass $m$ connected to a pivot by a
;; massless rod of length $l$ subject to uniform gravitational acceleration $g$. A
;; Lagrangian is $L(t, \theta, \dot{\theta}) = {1 \over 2} ml^2\dot{\theta}^2 +
;; mgl\cos \theta$. The formal parameters of $L$ are $t$, $\theta$, and
;; $\dot{\theta}$; $\theta$ measures the angle of the pendulum rod to a plumb line
;; and $\dot{\theta}$ is the angular velocity of the rod.
;; #+end_quote

;; Here is the Lagrangian described by the exercise:


(defn L-pendulum [m g l]
  (fn [[_ theta thetadot]]
    (+ (* (/ 1 2) m (square l) (square thetadot))
       (* m g l (cos theta)))))


;; #+RESULTS:
;; : #'ch1.ex1-12/L-pendulum

;; And the steps that lead us to Lagrange's equations:


(lagrange-equation-steps
 (L-pendulum 'm 'g 'l)
 (literal-function 'theta))


;; #+RESULTS[743c15f76087dcfc3e8f96fca6ff914111ae8809]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{- g\,l\,m\,\sin\left(\theta\left(t\right)\right)} \cr \cr \displaystyle{{l}^{2}\,m\,D\theta\left(t\right)} \cr \cr \displaystyle{{l}^{2}\,m\,{D}^{2}\theta\left(t\right)} \cr \cr \displaystyle{g\,l\,m\,\sin\left(\theta\left(t\right)\right) + {l}^{2}\,m\,{D}^{2}\theta\left(t\right)}\end{pmatrix}\n\end{equation}
;; :end:

;; The final entry is the Lagrange equation, equal to $0$. Divide out the shared
;; factors of $m$ and $l$:


(let [L (L-pendulum 'm 'g 'l)
      theta (literal-function 'theta)
      eqs ((Lagrange-equations L) theta)]
  (->tex-equation
   ((/ eqs (* 'm 'l))
    't)))
;; Part B: 2D Potential

;; The next problem is in rectangular coordinates. This means that we'll end up
;; with two Lagrange equations that have to be satisfied.

;; From the book:

;; #+begin_quote
;; A particle of mass $m$ moves in a two-dimensional potential $V(x, y) = {(x^2 +
;; y^2) \over 2} + x^2 y - {y^3 \over 3}$, where $x$ and $y$ are rectangular
;; coordinates of the particle. A Lagrangian is $L(t;x, y; v_x, v_y) = {1 \over 2}
;; m (v_x^2 + v_y^2) - V(x, y)$.
;; #+end_quote

;; I have no intuition for /what/ this potential is, by the way. One term, ${x^2 +
;; y^2} \over 2$, looks like half the square of the distance of the particle away
;; from 0, or ${1 \over 2} r^2$. What are the other terms? I've been so well
;; trained that I simply start calculating.

;; Define the Lagrangian to be the difference of the kinetic energy and some
;; potential $V$ that has access to the coordinates:


(defn L-2d-potential [m]
  (fn [V]
    (fn [local]
      (- (* (/ 1 2) m (square (velocity local)))
         (V (coordinate local))))))


;; #+RESULTS:
;; : #'ch1.ex1-12/L-2d-potential

;; Thanks to the tuple algebra of =scmutils=, This form of the Lagrangian is
;; general enough that it would work for any number of dimensions in rectangular
;; space, given some potential $V$. =square= takes a dot product, so we end up with
;; a kinetic energy term for every spatial dimension.

;; Note this for later, as this idea will become useful when the book reaches the
;; discussion of coordinate transformations.

;; Next define the potential from the problem description:


(defn V [[x y]]
  (- (+ (/ (+ (square x)
              (square y))
           2)
        (* (square x) y))
     (/ (cube y) 3)))


;; #+RESULTS:
;; : #'ch1.ex1-12/V

;; Our helpful function generates the Lagrange equations, along with each
;; intermediate step:


(lagrange-equation-steps
 ((L-2d-potential 'm) V)
 (up (literal-function 'x)
     (literal-function 'y)))
;; Part C: Particle on a Sphere

;; This problem is slightly more clear. From the book:

;; #+begin_quote
;; A Lagrangian for a particle of mass $m$ constrained to move on a sphere of
;; radius $R$ is $L(t; \theta, \phi; \alpha, \beta) = {1 \over 2} m
;; R^2(\alpha^2+(\beta \sin\theta)^2)$. The angle $\theta$ is the colatitude of the
;; particle and $\phi$ is the longitude; the rate of change of the colatitude is
;; $\alpha$ and the rate of change of the longitude is $\beta$.
;; #+end_quote

;; So the particle has some generalized kinetic energy with terms for:

;; - its speed north and south, and
;; - its speed east and west, scaled to be strongest at 0 longitude along the $x$
;;   axis and fall off to nothing at the $y$ axis.

;; Here is the Lagrangian:


(defn L-sphere [m R]
  (fn [[_ [theta] [alpha beta]]]
    (* (/ 1 2) m (square R)
       (+ (square alpha)
          (square (* beta (sin theta)))))))


;; #+RESULTS:
;; : #'ch1.ex1-12/L-sphere

;; Here is the full derivation:


(lagrange-equation-steps
 (L-sphere 'm 'R)
 (up (literal-function 'theta)
     (literal-function 'phi)))


;; #+RESULTS[c06373aad0b51e939e30b7df4899f73449bb4dc1]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{\begin{bmatrix}\displaystyle{{R}^{2}\,m\,\sin\left(\theta\left(t\right)\right)\,{\left(D\phi\left(t\right)\right)}^{2}\,\cos\left(\theta\left(t\right)\right)} \cr \cr \displaystyle{0}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{{R}^{2}\,m\,D\theta\left(t\right)} \cr \cr \displaystyle{{R}^{2}\,m\,{\sin}^{2}\left(\theta\left(t\right)\right)\,D\phi\left(t\right)}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{{R}^{2}\,m\,{D}^{2}\theta\left(t\right)} \cr \cr \displaystyle{2\,{R}^{2}\,m\,\sin\left(\theta\left(t\right)\right)\,D\theta\left(t\right)\,D\phi\left(t\right)\,\cos\left(\theta\left(t\right)\right) + {R}^{2}\,m\,{\sin}^{2}\left(\theta\left(t\right)\right)\,{D}^{2}\phi\left(t\right)}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{- {R}^{2}\,m\,\sin\left(\theta\left(t\right)\right)\,{\left(D\phi\left(t\right)\right)}^{2}\,\cos\left(\theta\left(t\right)\right) + {R}^{2}\,m\,{D}^{2}\theta\left(t\right)} \cr \cr \displaystyle{2\,{R}^{2}\,m\,\sin\left(\theta\left(t\right)\right)\,D\theta\left(t\right)\,D\phi\left(t\right)\,\cos\left(\theta\left(t\right)\right) + {R}^{2}\,m\,{\sin}^{2}\left(\theta\left(t\right)\right)\,{D}^{2}\phi\left(t\right)}\end{bmatrix}}\end{pmatrix}\n\end{equation}
;; :end:

;; The final Lagrange residuals have a few terms that we can divide out. Scheme
;; doesn't know that these are meant to be residuals, so it won't cancel out
;; factors that we can see by eye are missing.

;; Isolate the Lagrange equations from the derivation and manually simplify each
;; equation by dividing out, respectively, $mR^2$ and $mR^2 \sin \theta$:


(let [L     (L-sphere 'm 'R)
      theta (literal-function 'theta)
      q     (up theta (literal-function 'phi))
      le    ((Lagrange-equations L) q)
      eq1   (ref le 0)
      eq2   (ref le 1)]
  (->tex-equation
   ((up (/ eq1 (* 'm (square 'R)))
        (/ eq2 (* (sin theta) 'm (square 'R))))
    't)))
