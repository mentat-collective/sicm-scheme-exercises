;; Exercise 1.16: Central force motion
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-16.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; Scheme Approach

;; To show the rectangular Lagrangian, get the procedure from page 41:


(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))


;; This is already written in a form that can handle an arbitrary number of
;; coordiantes. Confirm the rectangular Lagrangian by passing in a local tuple with
;; 3 dimensional coordinates and velocities:


(->tex-equation
 ((L-central-rectangular 'm (literal-function 'U))
  (up 't
      (up 'x 'y 'z)
      (up 'v_x 'v_y 'v_z))))


;; #+RESULTS[9bf8352b0d3e667e7149e5519cc568efbbf2f331]:
;; \begin{equation}
;; {{1}\over {2}} m {{v}_{x}}^{2} + {{1}\over {2}} m {{v}_{y}}^{2} + {{1}\over {2}} m {{v}_{z}}^{2} - U\left( \sqrt{{x}^{2} + {y}^{2} + {z}^{2}} \right)
;; \end{equation}


;; Next, the spherical. Write down the coordinate transformation from spherical to
;; rectangular coordinates as a Scheme procedure:


(define (spherical->rect local)
  (let* ((q (coordinate local))
         (r (ref q 0))
         (theta (ref q 1))
         (phi (ref q 2)))
    (up (* r (sin theta) (cos phi))
        (* r (sin theta) (sin phi))
        (* r (cos theta)))))


;; #+RESULTS:
;; : #| spherical->rect |#

;; Here are the velocities calculated above by hand:


(->tex-equation
 (velocity
  ((F->C spherical->rect)
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot)))))


;; #+RESULTS[b41cbe257b2a859243c9b36052b54e533d08484e]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{  - \dot{\phi} r \sin\left( \phi \right) \sin\left( \theta \right) + r \dot{\theta} \cos\left( \phi \right) \cos\left( \theta \right) + \dot{r} \cos\left( \phi \right) \sin\left( \theta \right)} \cr \cr \displaystyle{ \dot{\phi} r \cos\left( \phi \right) \sin\left( \theta \right) + r \dot{\theta} \sin\left( \phi \right) \cos\left( \theta \right) + \dot{r} \sin\left( \phi \right) \sin\left( \theta \right)} \cr \cr \displaystyle{  - r \dot{\theta} \sin\left( \theta \right) + \dot{r} \cos\left( \theta \right)}\end{pmatrix}
;; \end{equation}


;; Now that we have $L$ and $C$, we can compose them to get $L'$, our spherical
;; Lagrangian:


(define (L-central-spherical m U)
  (compose (L-central-rectangular m U)
           (F->C spherical->rect)))


;; #+RESULTS:
;; : #| L-central-spherical |#

;; Confirm that this is equivalent to the analytic solution:


(->tex-equation
 ((L-central-spherical 'm (literal-function 'U))
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))
;; Exercise 1.17: Bead on a helical wire
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-16.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

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


(define ((turns->rect d h) local)
  (let* ((turns (coordinate local))
         (theta (* turns 2 'pi)))
    (up (/ turns h)
        (* (/ d 2) (cos theta))
        (* (/ d 2) (sin theta)))))


;; #+RESULTS:
;; : #| turns->rect |#

;; Or you could do this. Remember, these transformations need to be functions of a
;; local tuple, so if you're going to compose them, remember to put =coordinate= at
;; the beginning of the composition.


(define ((turns->x-theta h) q)
  (up (/ q h)
      (* q 2 'pi)))

(define ((x-theta->rect d) q)
  (let* ((x (ref q 0))
         (theta (ref q 1)))
    (up x
        (* (/ d 2) (cos theta))
        (* (/ d 2) (sin theta)))))

(define (turns->rect* d h)
  (compose (x-theta->rect d)
           (turns->x-theta h)
           coordinate))


;; #+RESULTS:
;; : #| turns->x-theta |#
;; :
;; : #| x-theta->rect |#
;; :
;; : #| turns->rect* |#

;; The transformations are identical:


(->tex-equation
 ((- (turns->rect 'd 'h)
     (turns->rect* 'd 'h))
  (up 't 'n 'ndot)))


;; #+RESULTS[3fa8a307ddde828e0f3b9b6c573e5935c577f76a]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{pmatrix}
;; \end{equation}

;; Define the Lagrangian:


(define ((L-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U q))))

(define (L-turns m d h U)
  (compose (L-rectangular m U)
           (F->C (turns->rect d h))))


;; #+RESULTS:
;; : #| L-rectangular |#
;; :
;; : #| L-turns |#

;; The potential is a uniform gravitational acceleration:


(define ((U-grav m g) q)
  (* m g (ref q 2)))


;; #+RESULTS:
;; : #| U-grav |#

;; Final Lagrangian:


(->tex-equation
 ((L-turns 'm 'd 'h (U-grav 'm 'g))
  (up 't 'n 'ndot)))


;; #+RESULTS[a486b9871810f8a74d1883be47666fa323a6de81]:
;; \begin{equation}
;; {{{{1}\over {2}} {d}^{2} {h}^{2} m {\dot{n}}^{2} {\pi}^{2} - {{1}\over {2}} d g {h}^{2} m \sin\left( 2 n \pi \right) + {{1}\over {2}} m {\dot{n}}^{2}}\over {{h}^{2}}}
;; \end{equation}

;; Lagrange equations of motion:


(let* ((L (L-turns 'm 'd 'h (U-grav 'm 'g)))
       (n (literal-function 'n)))
  (->tex-equation
   (((Lagrange-equations L) n) 't)))
;; Exercise 1.18: Bead on a triaxial surface
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-16.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

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


(define ((elliptical->rect a b c) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (phi (ref q 1)))
    (up (* a (sin theta) (cos phi))
        (* b (sin theta) (sin phi))
        (* c (cos theta)))))


;; #+RESULTS:
;; : #| elliptical->rect |#

;; Next, the Lagrangian:


(define ((L-free-particle m) local)
  (* 1/2 m (square
            (velocity local))))

(define (L-central-triaxial m a b c)
  (compose (L-free-particle m)
           (F->C (elliptical->rect a b c))))


;; #+RESULTS:
;; : #| L-free-particle |#
;; :
;; : #| L-central-triaxial |#

;; Final Lagrangian:


(let ((local (up 't
                 (up 'theta 'phi)
                 (up 'thetadot 'phidot))))
  (->tex-equation
   ((L-central-triaxial 'm 'a 'b 'c) local)))


;; #+RESULTS[96029124f8a649771c860516d1c6e668422de93e]:
;; \begin{equation}
;; {{1}\over {2}} {a}^{2} m {\dot{\phi}}^{2} {\left( \sin\left( \phi \right) \right)}^{2} {\left( \sin\left( \theta \right) \right)}^{2} - {a}^{2} m \dot{\phi} \dot{\theta} \sin\left( \phi \right) \sin\left( \theta \right) \cos\left( \phi \right) \cos\left( \theta \right) + {{1}\over {2}} {a}^{2} m {\dot{\theta}}^{2} {\left( \cos\left( \phi \right) \right)}^{2} {\left( \cos\left( \theta \right) \right)}^{2} + {{1}\over {2}} {b}^{2} m {\dot{\phi}}^{2} {\left( \sin\left( \theta \right) \right)}^{2} {\left( \cos\left( \phi \right) \right)}^{2} + {b}^{2} m \dot{\phi} \dot{\theta} \sin\left( \phi \right) \sin\left( \theta \right) \cos\left( \phi \right) \cos\left( \theta \right) + {{1}\over {2}} {b}^{2} m {\dot{\theta}}^{2} {\left( \sin\left( \phi \right) \right)}^{2} {\left( \cos\left( \theta \right) \right)}^{2} + {{1}\over {2}} {c}^{2} m {\dot{\theta}}^{2} {\left( \sin\left( \theta \right) \right)}^{2}
;; \end{equation}

;; I'm sure there's some simplification in there for us. But why?

;; Lagrange equations of motion:


(let* ((L (L-central-triaxial 'm 'a 'b 'c))
       (theta (literal-function 'theta))
       (phi (literal-function 'phi)))
  (->tex-equation
   (((Lagrange-equations L) (up theta phi))
    't)))
;; Exercise 1.19: Two-bar linkage
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-16.scm :comments org
;; :END:

;; Double pendulum, sort of, except the whole thing can fly around the plane.

;; The system description is:


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

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


(define ((double-linkage->rect l1 l2) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (phi (ref q 1))
         (x2 (ref q 2))
         (y2 (ref q 3)))
    (up (+ x2 (* l1 (sin theta)))
        (- y2 (* l1 (cos theta)))
        x2
        y2
        (+ x2 (* l2 (sin phi)))
        (- y2 (* l2 (cos phi))))))


;; #+RESULTS:
;; : #| double-linkage->rect |#

;; Next, the Lagrangian given rectangular coordinates, assuming no constraints.
;; Remember, we have a uniform gravitational field pointing down; this means that
;; each of the components has a potential dragging on it.


(define ((L-double-linkage-rect m1 m2 m3 U) local)
  (let* ((v (velocity local))
         (vx1 (ref v 0))
         (vy1 (ref v 1))
         (vx2 (ref v 2))
         (vy2 (ref v 3))
         (vx3 (ref v 4))
         (vy3 (ref v 5)))
    (- (+ (* m1 (+ (square vx1)
                   (square vy1)))
          (* m2 (+ (square vx2)
                   (square vy2)))
          (* m3 (+ (square vx3)
                   (square vy3))))
       (U (coordinate local)))))


;; #+RESULTS:
;; : #| L-double-linkage-rect |#

;; And the composition:


(define (L-double-linkage l1 l2 m1 m2 m3 U)
  (compose (L-double-linkage-rect m1 m2 m3 U)
           (F->C (double-linkage->rect l1 l2))))


;; #+RESULTS:
;; : #| L-double-linkage |#

;; Gravitational potential:


(define ((U-gravity g m1 m2 m3) q)
  (let* ((y1 (ref q 1))
         (y2 (ref q 3))
         (y3 (ref q 5)))
    (* g (+ (* m1 y1)
            (* m2 y2)
            (* m3 y3)))))


;; #+RESULTS:
;; : #| U-gravity |#


(let ((local (up 't
                 (up 'theta 'phi 'x_2 'y_2)
                 (up 'thetadot 'phidot 'xdot_2 'ydot_2)))
      (U (U-gravity 'g 'm_1 'm_2 'm_3)))
  (->tex-equation
   ((L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U) local)))


;; #+RESULTS[871defa58c4637289b4aa4236e470eceed35fc24]:
;; \begin{equation}
;; {{l}_{1}}^{2} {m}_{1} {\dot{\theta}}^{2} + 2 {l}_{1} {m}_{1} \dot{\theta} {\dot{x}}_{2} \cos\left( \theta \right) + 2 {l}_{1} {m}_{1} \dot{\theta} {\dot{y}}_{2} \sin\left( \theta \right) + {{l}_{2}}^{2} {m}_{3} {\dot{\phi}}^{2} + 2 {l}_{2} {m}_{3} \dot{\phi} {\dot{x}}_{2} \cos\left( \phi \right) + 2 {l}_{2} {m}_{3} \dot{\phi} {\dot{y}}_{2} \sin\left( \phi \right) + g {l}_{1} {m}_{1} \cos\left( \theta \right) + g {l}_{2} {m}_{3} \cos\left( \phi \right) - g {m}_{1} {y}_{2} - g {m}_{2} {y}_{2} - g {m}_{3} {y}_{2} + {m}_{1} {{\dot{x}}_{2}}^{2} + {m}_{1} {{\dot{y}}_{2}}^{2} + {m}_{2} {{\dot{x}}_{2}}^{2} + {m}_{2} {{\dot{y}}_{2}}^{2} + {m}_{3} {{\dot{x}}_{2}}^{2} + {m}_{3} {{\dot{y}}_{2}}^{2}
;; \end{equation}

;; Lagrange equations of motion:


(let* ((U (U-gravity 'g 'm_1 'm_2 'm_3))
       (L (L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U))
       (theta (literal-function 'theta))
       (phi (literal-function 'phi))
       (x2 (literal-function 'x_2))
       (y2 (literal-function 'y_2)))
  (->tex-equation
   (((Lagrange-equations L) (up theta phi x2 y2))
    't)))


;; #+RESULTS[891b221c072e1a0a84d8a553f44953d7e1708fec]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{ g {l}_{1} {m}_{1} \sin\left( \theta\left( t \right) \right) + 2 {{l}_{1}}^{2} {m}_{1} {D}^{2}\theta\left( t \right) + 2 {l}_{1} {m}_{1} \sin\left( \theta\left( t \right) \right) {D}^{2}{y}_{2}\left( t \right) + 2 {l}_{1} {m}_{1} \cos\left( \theta\left( t \right) \right) {D}^{2}{x}_{2}\left( t \right)} \cr \cr \displaystyle{ g {l}_{2} {m}_{3} \sin\left( \phi\left( t \right) \right) + 2 {{l}_{2}}^{2} {m}_{3} {D}^{2}\phi\left( t \right) + 2 {l}_{2} {m}_{3} \sin\left( \phi\left( t \right) \right) {D}^{2}{y}_{2}\left( t \right) + 2 {l}_{2} {m}_{3} \cos\left( \phi\left( t \right) \right) {D}^{2}{x}_{2}\left( t \right)} \cr \cr \displaystyle{  - 2 {l}_{1} {m}_{1} \sin\left( \theta\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} - 2 {l}_{2} {m}_{3} \sin\left( \phi\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + 2 {l}_{1} {m}_{1} {D}^{2}\theta\left( t \right) \cos\left( \theta\left( t \right) \right) + 2 {l}_{2} {m}_{3} {D}^{2}\phi\left( t \right) \cos\left( \phi\left( t \right) \right) + 2 {m}_{1} {D}^{2}{x}_{2}\left( t \right) + 2 {m}_{2} {D}^{2}{x}_{2}\left( t \right) + 2 {m}_{3} {D}^{2}{x}_{2}\left( t \right)} \cr \cr \displaystyle{ 2 {l}_{1} {m}_{1} {\left( D\theta\left( t \right) \right)}^{2} \cos\left( \theta\left( t \right) \right) + 2 {l}_{2} {m}_{3} {\left( D\phi\left( t \right) \right)}^{2} \cos\left( \phi\left( t \right) \right) + 2 {l}_{1} {m}_{1} {D}^{2}\theta\left( t \right) \sin\left( \theta\left( t \right) \right) + 2 {l}_{2} {m}_{3} {D}^{2}\phi\left( t \right) \sin\left( \phi\left( t \right) \right) + g {m}_{1} + g {m}_{2} + g {m}_{3} + 2 {m}_{1} {D}^{2}{y}_{2}\left( t \right) + 2 {m}_{2} {D}^{2}{y}_{2}\left( t \right) + 2 {m}_{3} {D}^{2}{y}_{2}\left( t \right)}\end{bmatrix}
;; \end{equation}

;; Kill some clear factors:


(let* ((U (U-gravity 'g 'm_1 'm_2 'm_3))
       (L (L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U))
       (theta (literal-function 'theta))
       (phi (literal-function 'phi))
       (x2 (literal-function 'x_2))
       (y2 (literal-function 'y_2))
       (eqs (((Lagrange-equations L) (up theta phi x2 y2))
             't)))
  (->tex-equation
   (up (/ (ref eqs 0) 'l_1 'm_1)
       (/ (ref eqs 1) 'l_2 'm_3)
       (/ (ref eqs 2) 2)
       (ref eqs 3))))
;; Exercise 1.20: Sliding pendulum
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-16.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

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


(define ((sliding-pend->rect l) local)
  (let* ((q (coordinate local))
         (x1 (ref q 0))
         (theta (ref q 1)))
    (up x1
        l
        (+ x1 (* l (sin theta)))
        (* l (- 1 (cos theta))))))


;; #+RESULTS:
;; : #| sliding-pend->rect |#

;; Next, the Lagrangian given rectangular coordinates, assuming no constraints:


(define ((L-sliding-pend-rect m1 m2 U) local)
  (let* ((v (velocity local))
         (vx1 (ref v 0))
         (vy1 (ref v 1))
         (vx2 (ref v 2))
         (vy2 (ref v 3)))
    (- (+ (* m1 (+ (square vx1)
                   (square vy1)))
          (* m2 (+ (square vx2)
                   (square vy2))))
       (U (coordinate local)))))


;; #+RESULTS:
;; : #| L-sliding-pend-rect |#

;; And the composition:


(define (L-sliding-pend l m1 m2 U)
  (compose (L-sliding-pend-rect m1 m2 U)
           (F->C (sliding-pend->rect l))))


;; #+RESULTS:
;; : #| L-sliding-pend |#

;; Gravitational potential. I could include the cart here, but since we know it's
;; fixed gravitationally it wouldn't change the equations of motion.


(define ((U-gravity g m2) q)
  (let* ((y2 (ref q 3)))
    (* m2 g y2)))


;; #+RESULTS:
;; : #| U-gravity |#


(let ((local (up 't
                 (up 'x_1 'theta)
                 (up 'xdot_1 'thetadot)))
      (U (U-gravity 'g 'm_2)))
  (->tex-equation
   ((L-sliding-pend 'l 'm_1 'm_2 U) local)))


;; #+RESULTS[a0d77887cb7ea10a807f5718c1383453f676c148]:
;; \begin{equation}
;; {l}^{2} {m}_{2} {\dot{\theta}}^{2} + 2 l {m}_{2} \dot{\theta} {\dot{x}}_{1} \cos\left( \theta \right) + g l {m}_{2} \cos\left( \theta \right) - g l {m}_{2} + {m}_{1} {{\dot{x}}_{1}}^{2} + {m}_{2} {{\dot{x}}_{1}}^{2}
;; \end{equation}

;; Lagrange equations of motion:


(let* ((U (U-gravity 'g 'm_2))
       (L (L-sliding-pend 'l 'm_1 'm_2 U))
       (x1 (literal-function 'x_1))
       (theta (literal-function 'theta)))
  (->tex-equation
   (((Lagrange-equations L) (up x1 theta))
    't)))


;; #+RESULTS[a556eaea3f66fe4d94bd82a34c9f2e786a4187a1]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{  - 2 l {m}_{2} \sin\left( \theta\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} + 2 l {m}_{2} {D}^{2}\theta\left( t \right) \cos\left( \theta\left( t \right) \right) + 2 {m}_{1} {D}^{2}{x}_{1}\left( t \right) + 2 {m}_{2} {D}^{2}{x}_{1}\left( t \right)} \cr \cr \displaystyle{ g l {m}_{2} \sin\left( \theta\left( t \right) \right) + 2 {l}^{2} {m}_{2} {D}^{2}\theta\left( t \right) + 2 l {m}_{2} {D}^{2}{x}_{1}\left( t \right) \cos\left( \theta\left( t \right) \right)}\end{bmatrix}
;; \end{equation}

;; Cleaner:


(let* ((U (U-gravity 'g 'm_2))
       (L (L-sliding-pend 'l 'm_1 'm_2 U))
       (x1 (literal-function 'x_1))
       (theta (literal-function 'theta))
       (eqs (((Lagrange-equations L) (up x1 theta))
             't)))
  (->tex-equation
   (up (ref eqs 0)
       (/ (ref eqs 1) 'l 'm_2))))
