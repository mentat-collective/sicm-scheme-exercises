;; Exercise 1.21: A dumbbell
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-21.scm :comments org
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


(load "ch1/utils.scm")
;; Multiple Particle API

;; Many exercises have been dealing with multiple particles so far. Let's introduce
;; some functions that let us pluck the appropriate coordinates out of the local
;; tuple.

;; If we have the velocity and mass of a particle, its kinetic energy is easy to
;; define:


(define (KE-particle m v)
  (* 1/2 m (square v)))


;; #+RESULTS:
;; : #| KE-particle |#

;; This next function, =extract-particle=, takes a number of components -- 2 for a
;; particle with 2 components, 3 for a particle in space, etc -- and returns a
;; function of =local= and =i=, a particle index. This function can be used to
;; extract a sub-local-tuple for that particle from a flattened list.


(define ((extract-particle pieces) local i)
  (let* ((indices (apply up (iota pieces (* i pieces))))
         (extract (lambda (tuple)
                    (vector-map (lambda (i)
                                  (ref tuple i))
                                indices))))
    (up (time local)
        (extract (coordinate local))
        (extract (velocity local)))))
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

(define (U-constraint q0 q1 F l)
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))


;; #+RESULTS:
;; : #| U-constraint |#

;; And here's a Lagrangian for two free particles, subject to a constraint
;; potential $F$ acting between them.

;; #+name: L-free-constrained

(define ((L-free-constrained m0 m1 l) local)
  (let* ((extract (extract-particle 2))
         (p0 (extract local 0))
         (q0 (coordinate p0))
         (qdot0 (velocity p0))

         (p1 (extract local 1))
         (q1 (coordinate p1))
         (qdot1 (velocity p1))

         (F (ref (coordinate local) 4)))
    (- (+ (KE-particle m0 qdot0)
          (KE-particle m1 qdot1))
       (U-constraint q0 q1 F l))))


;; #+RESULTS:
;; : #| L-free-constrained |#

;; Finally, the path. This is rectangular coordinates for each piece, plus $F$
;; between them.


(define q-rect
  (up (literal-function 'x_0)
      (literal-function 'y_0)
      (literal-function 'x_1)
      (literal-function 'y_1)
      (literal-function 'F)))


;; #+RESULTS:
;; : #| q-rect |#

;; This shows the lagrangian itself, which answers part b:


(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f (compose L (Gamma q-rect))))
  (->tex-equation
   (f 't)))


;; #+RESULTS[9e535179734061e62fa19be6d57ad8f8846008d9]:
;; \begin{equation}
;; {{{{1}\over {2}} l {m}_{0} {\left( D{x}_{0}\left( t \right) \right)}^{2} + {{1}\over {2}} l {m}_{0} {\left( D{y}_{0}\left( t \right) \right)}^{2} + {{1}\over {2}} l {m}_{1} {\left( D{x}_{1}\left( t \right) \right)}^{2} + {{1}\over {2}} l {m}_{1} {\left( D{y}_{1}\left( t \right) \right)}^{2} + {{1}\over {2}} {l}^{2} F\left( t \right) - {{1}\over {2}} F\left( t \right) {\left( {x}_{1}\left( t \right) \right)}^{2} + F\left( t \right) {x}_{1}\left( t \right) {x}_{0}\left( t \right) - {{1}\over {2}} F\left( t \right) {\left( {x}_{0}\left( t \right) \right)}^{2} - {{1}\over {2}} F\left( t \right) {\left( {y}_{1}\left( t \right) \right)}^{2} + F\left( t \right) {y}_{1}\left( t \right) {y}_{0}\left( t \right) - {{1}\over {2}} F\left( t \right) {\left( {y}_{0}\left( t \right) \right)}^{2}}\over {l}}
;; \end{equation}

;; Here are the Lagrange equations, which, if you squint, are like Newton's
;; equations from part a.


(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q-rect)))
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


(define ((cm-theta->rect m0 m1) local)
  (let* ((q (coordinate local))
         (x_cm (ref q 0))
         (y_cm (ref q 1))
         (theta (ref q 2))
         (c (ref q 3))
         (F (ref q 4))
         (total-mass (+ m0 m1))
         (m0-distance (* c (/ m1 total-mass)))
         (m1-distance (* c (/ m0 total-mass))))
    (up (- x_cm (* m0-distance (cos theta)))
        (- y_cm (* m0-distance (sin theta)))
        (+ x_cm (* m1-distance (cos theta)))
        (+ y_cm (* m1-distance (sin theta)))
        F)))


;; #+RESULTS:
;; : #| cm-theta->rect |#

;; Then the coordinate change applied to the local tuple:


(let ((local (up 't
                 (up 'x_cm 'y_cm 'theta 'c 'F)
                 (up 'xdot_cm 'ydot_cm 'thetadot 'cdot 'Fdot)))
      (C (F->C (cm-theta->rect 'm_0 'm_1))))
  (->tex-equation
   (C local)))


;; #+RESULTS[2c67e287b90be4d75ab8c396222d968e56d71383]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ {{ - c {m}_{1} \cos\left( \theta \right) + {m}_{0} {x}_{cm} + {m}_{1} {x}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{ - c {m}_{1} \sin\left( \theta \right) + {m}_{0} {y}_{cm} + {m}_{1} {y}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{c {m}_{0} \cos\left( \theta \right) + {m}_{0} {x}_{cm} + {m}_{1} {x}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{c {m}_{0} \sin\left( \theta \right) + {m}_{0} {y}_{cm} + {m}_{1} {y}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ F}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ {{c {m}_{1} \dot{\theta} \sin\left( \theta \right) - \dot{c} {m}_{1} \cos\left( \theta \right) + {m}_{0} {\dot{x}}_{cm} + {m}_{1} {\dot{x}}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{ - c {m}_{1} \dot{\theta} \cos\left( \theta \right) - \dot{c} {m}_{1} \sin\left( \theta \right) + {m}_{0} {\dot{y}}_{cm} + {m}_{1} {\dot{y}}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{ - c {m}_{0} \dot{\theta} \sin\left( \theta \right) + \dot{c} {m}_{0} \cos\left( \theta \right) + {m}_{0} {\dot{x}}_{cm} + {m}_{1} {\dot{x}}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{c {m}_{0} \dot{\theta} \cos\left( \theta \right) + \dot{c} {m}_{0} \sin\left( \theta \right) + {m}_{0} {\dot{y}}_{cm} + {m}_{1} {\dot{y}}_{cm}}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ \dot{F}}\end{pmatrix}}\end{pmatrix}
;; \end{equation}

;; Then the Lagrangian in the new coordinates;


(define (L-free-constrained* m0 m1 l)
  (compose (L-free-constrained m0 m1 l)
           (F->C (cm-theta->rect m0 m1))))


;; #+RESULTS:
;; : #| L-free-constrained* |#

;; This shows the lagrangian itself, after the coordinate transformation:


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f (compose L (Gamma q))))
  (->tex-equation
   (f 't)))


;; #+RESULTS[d36e7f3c5c5e5a8663d599a055face7a7f6ddb61]:
;; \begin{equation}
;; {{l {m}_{0} {m}_{1} {\left( c\left( t \right) \right)}^{2} {\left( D\theta\left( t \right) \right)}^{2} + l {{m}_{0}}^{2} {\left( D{x}_{cm}\left( t \right) \right)}^{2} + l {{m}_{0}}^{2} {\left( D{y}_{cm}\left( t \right) \right)}^{2} + 2 l {m}_{0} {m}_{1} {\left( D{x}_{cm}\left( t \right) \right)}^{2} + 2 l {m}_{0} {m}_{1} {\left( D{y}_{cm}\left( t \right) \right)}^{2} + l {m}_{0} {m}_{1} {\left( Dc\left( t \right) \right)}^{2} + l {{m}_{1}}^{2} {\left( D{x}_{cm}\left( t \right) \right)}^{2} + l {{m}_{1}}^{2} {\left( D{y}_{cm}\left( t \right) \right)}^{2} + {l}^{2} {m}_{0} F\left( t \right) + {l}^{2} {m}_{1} F\left( t \right) - {m}_{0} F\left( t \right) {\left( c\left( t \right) \right)}^{2} - {m}_{1} F\left( t \right) {\left( c\left( t \right) \right)}^{2}}\over {2 l {m}_{0} + 2 l {m}_{1}}}
;; \end{equation}

;; Here are the Lagrange equations:


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
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


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))


;; #+RESULTS[a51bdf5bbe673771262278b5dca048646c257752]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{ {m}_{0} {D}^{2}{x}_{cm}\left( t \right) + {m}_{1} {D}^{2}{x}_{cm}\left( t \right)} \cr \cr \displaystyle{ {m}_{0} {D}^{2}{y}_{cm}\left( t \right) + {m}_{1} {D}^{2}{y}_{cm}\left( t \right)} \cr \cr \displaystyle{ {{{l}^{2} {m}_{0} {m}_{1} {D}^{2}\theta\left( t \right)}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ {{ - l {m}_{0} {m}_{1} {\left( D\theta\left( t \right) \right)}^{2} + {m}_{0} F\left( t \right) + {m}_{1} F\left( t \right)}\over {{m}_{0} + {m}_{1}}}} \cr \cr \displaystyle{ 0}\end{bmatrix}
;; \end{equation}

;; This is saying that the acceleration on the center of mass is 0.

;; The fourth equation, the equation of motion for the $c(t)$, is interesting here.
;; We need to pull in the definition of "reduced mass" from exercise 1.11:


(define (reduced-mass m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))


;; #+RESULTS:
;; : #| reduced-mass |#

;; If we let $m$ be the reduced mass, this equation states:

;; \begin{equation}
;; \label{eq:constraint-force}
;; F(t) = m l \dot{\theta}^2
;; \end{equation}

;; We can verify this with Scheme by subtracting the two equations:


(let* ((F (literal-function 'F))
       (theta (literal-function 'theta))
       (q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              theta
              (lambda (t) 'l)
              F))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q))

       (m (reduced-mass 'm_0 'm_1)))
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


(define ((L-free2 m0 m1) local)
  (let* ((extract (extract-particle 2))

         (p0 (extract local 0))
         (q0 (coordinate p0))
         (qdot0 (velocity p0))

         (p1 (extract local 1))
         (q1 (coordinate p1))
         (qdot1 (velocity p1)))
    (+ (KE-particle m0 qdot0)
       (KE-particle m1 qdot1))))


;; #+RESULTS:
;; : #| L-free2 |#

;; Then a version of =cm-theta->rect= where we ignore $F$, and sub in a constant
;; $l$:


(define ((cm-theta->rect* m0 m1 l) local)
  (let* ((q (coordinate local))
         (x_cm (ref q 0))
         (y_cm (ref q 1))
         (theta (ref q 2))
         (total-mass (+ m0 m1))
         (m0-distance (* l (/ m1 total-mass)))
         (m1-distance (* l (/ m0 total-mass))))
    (up (- x_cm (* m0-distance (cos theta)))
        (- y_cm (* m0-distance (sin theta)))
        (+ x_cm (* m1-distance (cos theta)))
        (+ y_cm (* m1-distance (sin theta))))))


;; #+RESULTS[aee3cbebd833662ba2610e81cef9e42936dcb703]:
;; #| cm-theta->rect* |#

;; The Lagrangian:


(define (L-free-constrained2 m0 m1 l)
  (compose (L-free2 m0 m1)
           (F->C (cm-theta->rect* m0 m1 l))))


;; #+RESULTS:
;; : #| L-free-constrained2 |#

;; Equations:


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (L1 (L-free-constrained* 'm_0 'm_1 'l))
       (L2 (L-free-constrained2 'm_0 'm_1 'l)))
  (->tex-equation
   ((- ((Lagrange-equations L1) q)
       ((Lagrange-equations L2) q))
    't)))
