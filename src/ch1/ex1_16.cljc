;; Exercise 1.16: Central force motion
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_16.cljc :comments org
;; :END:


(ns ch1.ex1-16
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
;; Scheme Approach

;; To show the rectangular Lagrangian, get the procedure from page 41:


(defn L-central-rectangular [m U]
  (fn [[_ q v]]
    (- (* (/ 1 2) m (square v))
       (U (sqrt (square q))))))


;; #+RESULTS:
;; : #'ch1.ex1-16/L-central-rectangular

;; This is already written in a form that can handle an arbitrary number of
;; coordiantes. Confirm the rectangular Lagrangian by passing in a local tuple with
;; 3 dimensional coordinates and velocities:


(->tex-equation
 ((L-central-rectangular 'm (literal-function 'U))
  (up 't
      (up 'x 'y 'z)
      (up 'v_x 'v_y 'v_z))))


;; #+RESULTS[9b6ec3b9f75263e7c2b7fa09a10ac997f802b72e]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,m\,{v_x}^{2} + \frac{1}{2}\,m\,{v_y}^{2} + \frac{1}{2}\,m\,{v_z}^{2} - U\left(\sqrt {{x}^{2} + {y}^{2} + {z}^{2}}\right)\n\end{equation}
;; :end:

;; Next, the spherical. Write down the coordinate transformation from spherical to
;; rectangular coordinates as a Scheme procedure:


(defn spherical->rect [[_ [r theta phi]]]
  (up (* r (sin theta) (cos phi))
      (* r (sin theta) (sin phi))
      (* r (cos theta))))


;; #+RESULTS:
;; : #'ch1.ex1-16/spherical->rect

;; Here are the velocities calculated above by hand:


(->tex-equation
 (velocity
  ((F->C spherical->rect)
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot)))))


;; #+RESULTS[b2c0978a0825f6416bb7690e463134976cbb5f81]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{- \dot {\phi}\,r\,\sin\left(\phi\right)\,\sin\left(\theta\right) + r\,\dot {\theta}\,\cos\left(\phi\right)\,\cos\left(\theta\right) + \dot r\,\cos\left(\phi\right)\,\sin\left(\theta\right)} \cr \cr \displaystyle{\dot {\phi}\,r\,\cos\left(\phi\right)\,\sin\left(\theta\right) + r\,\dot {\theta}\,\sin\left(\phi\right)\,\cos\left(\theta\right) + \dot r\,\sin\left(\phi\right)\,\sin\left(\theta\right)} \cr \cr \displaystyle{- r\,\dot {\theta}\,\sin\left(\theta\right) + \dot r\,\cos\left(\theta\right)}\end{pmatrix}\n\end{equation}
;; :end:


;; Now that we have $L$ and $C$, we can compose them to get $L'$, our spherical
;; Lagrangian:


(defn L-central-spherical [m U]
  (compose (L-central-rectangular m U)
           (F->C spherical->rect)))


;; #+RESULTS:
;; : #'ch1.ex1-16/L-central-spherical

;; Confirm that this is equivalent to the analytic solution:


(->tex-equation
 ((L-central-spherical 'm (literal-function 'U))
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))
;; Exercise 1.17: Bead on a helical wire
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_16.cljc :comments org
;; :END:


(ns ch1.ex1-17
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; This, and the next three exercises, are here to give you practice in the real
;; art, of difficulty, of any dynamics problem. It's easy to change coordinates. So
;; what coordinates do you use?

;; #+begin_quote
;; A bead of mass $m$ is constrained to move on a frictionless helical wire. The
;; helix is oriented so that its axis is horizontal. The diameter of the helix is
;; $d$ and its pitch (turns per unit length) is $h$. The system is in a uniform
;; gravitational field with vertical acceleration $g$. Formulate a Lagrangian that
;; describes the system and find the Lagrange equations of motion.
;; #+end_quote

;; I'll replace this with a better picture later, but this is the setup:

;; #+DOWNLOADED: screenshot @ 2020-06-25 11:03:55
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-25_11-03-55_screenshot.png]]


(defn turns->rect [d h]
  (fn [local]
    (let [turns (coordinate local)
          theta (* turns 2 'pi)]
      (up (/ turns h)
          (* (/ d 2) (cos theta))
          (* (/ d 2) (sin theta))))))


;; #+RESULTS:
;; : #'ch1.ex1-17/turns->rect

;; Or you could do this. Remember, these transformations need to be functions of a
;; local tuple, so if you're going to compose them, remember to put =coordinate= at
;; the beginning of the composition.


(defn turns->x-theta [h]
  (fn [q]
    (up (/ q h)
        (* q 2 'pi))))

(defn x-theta->rect [d]
  (fn [[x theta]]
    (up x
        (* (/ d 2) (cos theta))
        (* (/ d 2) (sin theta)))))

(defn turns->rect* [d h]
  (compose (x-theta->rect d)
           (turns->x-theta h)
           coordinate))


;; #+RESULTS:

;; The transformations are identical:


(->tex-equation
 ((- (turns->rect 'd 'h)
     (turns->rect* 'd 'h))
  (up 't 'n 'ndot)))


;; #+RESULTS[3c62091db7f0c1925b9500d98ab490a4272c1040]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{0} \cr \cr \displaystyle{0} \cr \cr \displaystyle{0}\end{pmatrix}\n\end{equation}
;; :end:

;; Define the Lagrangian:


(defn L-rectangular [m U]
  (fn [[_ q v]]
    (- (* (/ 1 2) m (square v))
       (U q))))

(defn L-turns [m d h U]
  (compose (L-rectangular m U)
           (F->C (turns->rect d h))))


;; #+RESULTS:

;; The potential is a uniform gravitational acceleration:


(defn U-grav [m g]
  (fn [q]
    (* m g (ref q 2))))


;; #+RESULTS:
;; : #'ch1.ex1-17/U-grav

;; Final Lagrangian:


(->tex-equation
 ((L-turns 'm 'd 'h (U-grav 'm 'g))
  (up 't 'n 'ndot)))


;; #+RESULTS[5fd23110114f9beb52584fe2f969645da1381033]:
;; :results:
;; \begin{equation}\n\frac{{d}^{2}\,{h}^{2}\,m\,{\dot n}^{2}\,{\pi}^{2} - d\,g\,{h}^{2}\,m\,\sin\left(2\,n\,\pi\right) + m\,{\dot n}^{2}}{2\,{h}^{2}}\n\end{equation}
;; :end:

;; Lagrange equations of motion:


(let [L (L-turns 'm 'd 'h (U-grav 'm 'g))
      n (literal-function 'n)]
  (->tex-equation
   (((Lagrange-equations L) n) 't)))
;; Exercise 1.18: Bead on a triaxial surface
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_16.cljc :comments org
;; :END:


(ns ch1.ex1-18
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; #+begin_quote
;; A bead of mass $m$ moves without friction on a triaxial ellipsoidal surface. In
;; rectangular coordinates the surface satisfies

;; \begin{equation}
;;   {x^2 \over a^2} + {y^2 \over b^2} + {z^2 \over c^2} = 1
;; \end{equation}

;; for some constants $a$, $b$, and $c$. Identify suitable generalized coordinates,
;; formulate a Lagrangian, and find Lagrange's equations.
;; #+end_quote

;; The transformation to elliptical coordinates is very similar to the spherical
;; coordinate transformation, but with a fixed $a$, $b$ and $c$ coefficient for
;; each rectangular dimension, and no more radial degree of freedom:


(defn elliptical->rect [a b c]
  (fn [[_ [theta phi]]]
    (up (* a (sin theta) (cos phi))
        (* b (sin theta) (sin phi))
        (* c (cos theta)))))


;; #+RESULTS:
;; : #'ch1.ex1-18/elliptical->rect

;; Next, the Lagrangian:


(defn L-free-particle [m]
  (fn [[_ _ v]]
    (* (/ 1 2) m (square v))))

(defn L-central-triaxial [m a b c]
  (compose (L-free-particle m)
           (F->C (elliptical->rect a b c))))


;; #+RESULTS:
;; | #'ch1.ex1-18/L-free-particle    |
;; | #'ch1.ex1-18/L-central-triaxial |

;; Final Lagrangian:


(let [local (up 't
                (up 'theta 'phi)
                (up 'thetadot 'phidot))]
  (->tex-equation
   ((L-central-triaxial 'm 'a 'b 'c) local)))


;; #+RESULTS[2d6f5b64206ae399aae3d9e63eca6d7612c3303f]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,{a}^{2}\,m\,{\dot {\phi}}^{2}\,{\sin}^{2}\left(\theta\right)\,{\sin}^{2}\left(\phi\right) - {a}^{2}\,m\,\dot {\phi}\,\dot {\theta}\,\cos\left(\phi\right)\,\cos\left(\theta\right)\,\sin\left(\theta\right)\,\sin\left(\phi\right) + \frac{1}{2}\,{a}^{2}\,m\,{\dot {\theta}}^{2}\,{\cos}^{2}\left(\phi\right)\,{\cos}^{2}\left(\theta\right) + \frac{1}{2}\,{b}^{2}\,m\,{\dot {\phi}}^{2}\,{\cos}^{2}\left(\phi\right)\,{\sin}^{2}\left(\theta\right) + {b}^{2}\,m\,\dot {\phi}\,\dot {\theta}\,\cos\left(\phi\right)\,\cos\left(\theta\right)\,\sin\left(\theta\right)\,\sin\left(\phi\right) + \frac{1}{2}\,{b}^{2}\,m\,{\dot {\theta}}^{2}\,{\cos}^{2}\left(\theta\right)\,{\sin}^{2}\left(\phi\right) + \frac{1}{2}\,{c}^{2}\,m\,{\dot {\theta}}^{2}\,{\sin}^{2}\left(\theta\right)\n\end{equation}
;; :end:

;; I'm sure there's some simplification in there for us. But why?

;; Lagrange equations of motion:


(let [L (L-central-triaxial 'm 'a 'b 'c)
      theta (literal-function 'theta)
      phi (literal-function 'phi)]
  (->tex-equation
   (((Lagrange-equations L) (up theta phi))
    't)))
;; Exercise 1.19: Two-bar linkage
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_16.cljc :comments org
;; :END:

;; Double pendulum, sort of, except the whole thing can fly around the plane.

;; The system description is:


(ns ch1.ex1-19
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; #+begin_quote
;; The two-bar linkage shown in figure 1.3 is constrained to move in the plane. It
;; is composed of three small massive bodies interconnected by two massless rigid
;; rods in a uniform gravitational field with vertical acceleration g. The rods are
;; pinned to the central body by a hinge that allows the linkage to fold. The
;; system is arranged so that the hinge is completely free: the members can go
;; through all configurations without collision. Formulate a Lagrangian that
;; describes the system and find the Lagrange equations of motion. Use the computer
;; to do this, because the equations are rather big.
;; #+end_quote

;; This is new. Now we have multiple bodies:

;; #+DOWNLOADED: https://tgvaughan.github.io/sicm/images/Art_P146.jpg @ 2020-06-29 05:39:01
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-29_05-39-01_Art_P146.jpg]]

;; We can handle this by treating our coordinate space as having new dimensions
;; for, say, $x_0$, $y_0$, $x_1$, $y_1$. The fact that multiple coordinates refer
;; to the same particle doesn't matter for the Lagrangian. But it's a confusing
;; API.

;; /Without/ any constraints, we have six degrees of freedom. $x, y$ for each
;; particle. With the constraints we have:

;; 1) $x, y$ for the central body
;; 2) $\theta$ and $\phi$ for the angles off center.

;; (Sketch these out on the picture for the final version.)

;; \begin{equation}
;; \begin{aligned}
;;   x_2(t) & = x_2(t) \cr
;;   y_2(t) & = y_2(t) \cr
;;   x_1(t) & = x_2(t) + l_1 \sin \theta \cr
;;   y_1(t) & = y_2(t) - l_1 \cos \theta \cr
;;   x_3(t) & = x_2(t) + l_2 \sin \phi \cr
;;   y_3(t) & = y_2(t) - l_2 \cos \phi
;; \end{aligned}
;; \end{equation}

;; Sketch out why this makes sense. Each angle is positive CCW for consistency,
;; since they can swing all the way around.

;; Write the coordinate transformation in scheme.


(defn double-linkage->rect [l1 l2]
  (fn [[_ [theta phi x2 y2]]]
    (up (+ x2 (* l1 (sin theta)))
        (- y2 (* l1 (cos theta)))
        x2
        y2
        (+ x2 (* l2 (sin phi)))
        (- y2 (* l2 (cos phi))))))


;; #+RESULTS:
;; : #'ch1.ex1-19/double-linkage->rect

;; Next, the Lagrangian given rectangular coordinates, assuming no constraints.
;; Remember, we have a uniform gravitational field pointing down; this means that
;; each of the components has a potential dragging on it.


(defn L-double-linkage-rect [m1 m2 m3 U]
  (fn [[_ q [vx1 vy1 vx2 vy2 vx3 vy3]]]
    (- (+ (* m1 (+ (square vx1)
                   (square vy1)))
          (* m2 (+ (square vx2)
                   (square vy2)))
          (* m3 (+ (square vx3)
                   (square vy3))))
       (U q))))


;; #+RESULTS:
;; : #'ch1.ex1-19/L-double-linkage-rect

;; And the composition:


(defn L-double-linkage [l1 l2 m1 m2 m3 U]
  (compose (L-double-linkage-rect m1 m2 m3 U)
           (F->C (double-linkage->rect l1 l2))))


;; #+RESULTS:
;; : #'ch1.ex1-19/L-double-linkage

;; Gravitational potential:


(defn U-gravity [g m1 m2 m3]
  (fn [[_ y1 _ y2 _ y3]]
    (* g (+ (* m1 y1)
            (* m2 y2)
            (* m3 y3)))))


;; #+RESULTS:
;; : #'ch1.ex1-19/U-gravity


(let [local (up 't
                (up 'theta 'phi 'x_2 'y_2)
                (up 'thetadot 'phidot 'xdot_2 'ydot_2))
      U (U-gravity 'g 'm_1 'm_2 'm_3)]
  (->tex-equation
   ((L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U) local)))


;; #+RESULTS[774c280417796b62ee8e1c67f93caad5998b0e2c]:
;; :results:
;; \begin{equation}\n{l_1}^{2}\,m_1\,{\dot {\theta}}^{2} + 2\,l_1\,m_1\,\dot {\theta}\,{\dot x}_2\,\cos\left(\theta\right) + 2\,l_1\,m_1\,\dot {\theta}\,{\dot y}_2\,\sin\left(\theta\right) + {l_2}^{2}\,m_3\,{\dot {\phi}}^{2} + 2\,l_2\,m_3\,\dot {\phi}\,{\dot x}_2\,\cos\left(\phi\right) + 2\,l_2\,m_3\,\dot {\phi}\,{\dot y}_2\,\sin\left(\phi\right) + g\,l_1\,m_1\,\cos\left(\theta\right) + g\,l_2\,m_3\,\cos\left(\phi\right) - g\,m_1\,y_2 - g\,m_2\,y_2 - g\,m_3\,y_2 + m_1\,{{\dot x}_2}^{2} + m_1\,{{\dot y}_2}^{2} + m_2\,{{\dot x}_2}^{2} + m_2\,{{\dot y}_2}^{2} + m_3\,{{\dot x}_2}^{2} + m_3\,{{\dot y}_2}^{2}\n\end{equation}
;; :end:

;; Lagrange equations of motion:


(let [U (U-gravity 'g 'm_1 'm_2 'm_3)
      L (L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U)
      theta (literal-function 'theta)
      phi (literal-function 'phi)
      x2 (literal-function 'x_2)
      y2 (literal-function 'y_2)]
  (->tex-equation
   (((Lagrange-equations L) (up theta phi x2 y2))
    't)))


;; #+RESULTS[39b88b443a8dd7df4fec651e2f3d3c9adb5508c6]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{g\,l_1\,m_1\,\sin\left(\theta\left(t\right)\right) + 2\,{l_1}^{2}\,m_1\,{D}^{2}\theta\left(t\right) + 2\,l_1\,m_1\,\cos\left(\theta\left(t\right)\right)\,{D}^{2}x_2\left(t\right) + 2\,l_1\,m_1\,\sin\left(\theta\left(t\right)\right)\,{D}^{2}y_2\left(t\right)} \cr \cr \displaystyle{g\,l_2\,m_3\,\sin\left(\phi\left(t\right)\right) + 2\,{l_2}^{2}\,m_3\,{D}^{2}\phi\left(t\right) + 2\,l_2\,m_3\,\cos\left(\phi\left(t\right)\right)\,{D}^{2}x_2\left(t\right) + 2\,l_2\,m_3\,\sin\left(\phi\left(t\right)\right)\,{D}^{2}y_2\left(t\right)} \cr \cr \displaystyle{-2\,l_1\,m_1\,{\left(D\theta\left(t\right)\right)}^{2}\,\sin\left(\theta\left(t\right)\right) -2\,l_2\,m_3\,{\left(D\phi\left(t\right)\right)}^{2}\,\sin\left(\phi\left(t\right)\right) + 2\,l_1\,m_1\,\cos\left(\theta\left(t\right)\right)\,{D}^{2}\theta\left(t\right) + 2\,l_2\,m_3\,\cos\left(\phi\left(t\right)\right)\,{D}^{2}\phi\left(t\right) + 2\,m_1\,{D}^{2}x_2\left(t\right) + 2\,m_2\,{D}^{2}x_2\left(t\right) + 2\,m_3\,{D}^{2}x_2\left(t\right)} \cr \cr \displaystyle{2\,l_1\,m_1\,\cos\left(\theta\left(t\right)\right)\,{\left(D\theta\left(t\right)\right)}^{2} + 2\,l_2\,m_3\,\cos\left(\phi\left(t\right)\right)\,{\left(D\phi\left(t\right)\right)}^{2} + 2\,l_1\,m_1\,\sin\left(\theta\left(t\right)\right)\,{D}^{2}\theta\left(t\right) + 2\,l_2\,m_3\,\sin\left(\phi\left(t\right)\right)\,{D}^{2}\phi\left(t\right) + g\,m_1 + g\,m_2 + g\,m_3 + 2\,m_1\,{D}^{2}y_2\left(t\right) + 2\,m_2\,{D}^{2}y_2\left(t\right) + 2\,m_3\,{D}^{2}y_2\left(t\right)}\end{bmatrix}\n\end{equation}
;; :end:

;; Kill some clear factors:


(let [U (U-gravity 'g 'm_1 'm_2 'm_3)
      L (L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U)
      theta (literal-function 'theta)
      phi (literal-function 'phi)
      x2 (literal-function 'x_2)
      y2 (literal-function 'y_2)
      eqs (((Lagrange-equations L) (up theta phi x2 y2))
           't)]
  (->tex-equation
   (up (/ (ref eqs 0) 'l_1 'm_1)
       (/ (ref eqs 1) 'l_2 'm_3)
       (/ (ref eqs 2) 2)
       (ref eqs 3))))
;; Exercise 1.20: Sliding pendulum
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_16.cljc :comments org
;; :END:


(ns ch1.ex1-20
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; #+begin_quote
;; Consider a pendulum of length $l$ attached to a support that is free to move
;; horizontally, as shown in figure 1.4. Let the mass of the support be $m1$ and
;; the mass of the pendulum bob be $m2$. Formulate a Lagrangian and derive
;; Lagrange's equations for this system.
;; #+end_quote

;; This is interesting, and totally not-obvious how to represent with Newtonian
;; mechanics. Here it is pretty simple. The setup:

;; #+DOWNLOADED: https://tgvaughan.github.io/sicm/images/Art_P147.jpg @ 2020-06-29 05:39:33
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-29_05-39-33_Art_P147.jpg]]

;; We can use 2 coordinates:

;; 1. the horizontal position of the cart
;; 2. the angle $\theta$ of the bob.

;; Here's the conversion to rectangular:

;; \begin{equation}
;; \begin{aligned}
;;   x_1(t) & = x_1(t) \cr
;;   y_1(t) & = l \cr
;;   x_2(t) & = x_1(t) + l \sin \theta \cr
;;   y_2(t) & = l(1 - \cos \theta)
;; \end{aligned}
;; \end{equation}

;; Draw these on the picture to make it clearer.

;; Write the coordinate transformation in scheme.


(defn sliding-pend->rect [l]
  (fn [[_ [x1 theta]]]
    (up x1
        l
        (+ x1 (* l (sin theta)))
        (* l (- 1 (cos theta))))))


;; #+RESULTS:
;; : #'ch1.ex1-20/sliding-pend->rect

;; Next, the Lagrangian given rectangular coordinates, assuming no constraints:


(defn L-sliding-pend-rect [m1 m2 U]
  (fn [[_ q [vx1 vy1 vx2 vy2]]]
    (- (+ (* m1 (+ (square vx1)
                   (square vy1)))
          (* m2 (+ (square vx2)
                   (square vy2))))
       (U q))))


;; #+RESULTS:
;; : #'ch1.ex1-20/L-sliding-pend-rect

;; And the composition:


(defn L-sliding-pend [l m1 m2 U]
  (compose (L-sliding-pend-rect m1 m2 U)
           (F->C (sliding-pend->rect l))))


;; #+RESULTS:
;; : #'ch1.ex1-20/L-sliding-pend

;; Gravitational potential. I could include the cart here, but since we know it's
;; fixed gravitationally it wouldn't change the equations of motion.


(defn U-gravity [g m2]
  (fn [q]
    (let [y2 (ref q 3)]
      (* m2 g y2))))


;; #+RESULTS:
;; : #'ch1.ex1-20/U-gravity


(let [local (up 't
                (up 'x_1 'theta)
                (up 'xdot_1 'thetadot))
      U (U-gravity 'g 'm_2)]
  (->tex-equation
   ((L-sliding-pend 'l 'm_1 'm_2 U) local)))


;; #+RESULTS[e52b23ca8e418fe1eeac4cfb66e9e3dc68e52201]:
;; :results:
;; \begin{equation}\n{l}^{2}\,m_2\,{\dot {\theta}}^{2} + 2\,l\,m_2\,\dot {\theta}\,{\dot x}_1\,\cos\left(\theta\right) + g\,l\,m_2\,\cos\left(\theta\right) - g\,l\,m_2 + m_1\,{{\dot x}_1}^{2} + m_2\,{{\dot x}_1}^{2}\n\end{equation}
;; :end:

;; Lagrange equations of motion:


(let [U (U-gravity 'g 'm_2)
      L (L-sliding-pend 'l 'm_1 'm_2 U)
      x1 (literal-function 'x_1)
      theta (literal-function 'theta)]
  (->tex-equation
   (((Lagrange-equations L) (up x1 theta))
    't)))


;; #+RESULTS[7973aa8ff319b1baac4007de886efd0c57be2755]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{-2\,l\,m_2\,{\left(D\theta\left(t\right)\right)}^{2}\,\sin\left(\theta\left(t\right)\right) + 2\,l\,m_2\,\cos\left(\theta\left(t\right)\right)\,{D}^{2}\theta\left(t\right) + 2\,m_1\,{D}^{2}x_1\left(t\right) + 2\,m_2\,{D}^{2}x_1\left(t\right)} \cr \cr \displaystyle{g\,l\,m_2\,\sin\left(\theta\left(t\right)\right) + 2\,{l}^{2}\,m_2\,{D}^{2}\theta\left(t\right) + 2\,l\,m_2\,\cos\left(\theta\left(t\right)\right)\,{D}^{2}x_1\left(t\right)}\end{bmatrix}\n\end{equation}
;; :end:

;; Cleaner:


(let [U (U-gravity 'g 'm_2)
      L (L-sliding-pend 'l 'm_1 'm_2 U)
      x1 (literal-function 'x_1)
      theta (literal-function 'theta)
      eqs (((Lagrange-equations L) (up x1 theta))
           't)]
  (->tex-equation
   (up (ref eqs 0)
       (/ (ref eqs 1) 'l 'm_2))))
