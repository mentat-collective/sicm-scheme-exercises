;; Exercise 1.14: Coordinate-independence of Lagrange equations
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-14.scm :comments org
;; :END:

;; Look carefully at what this exercise is asking us to do:

;; #+begin_quote
;; Check that the Lagrange equations for central force motion in polar coordinates
;; and in rectangular coordinates are equivalent. Determine the relationship among
;; the second derivatives by substituting paths into the transformation equations
;; and computing derivatives, then substitute these relations into the equations of
;; motion.
;; #+end_quote

;; The punchline that we'll encounter soon is that a coordinate transformation of
;; applied to some path function $q$ can commute with $\Gamma$. You can always
;; write some function $C$ of the local tuple that handles the coordinate
;; transformation /after/ $\Gamma[q]$ instead of transforming the path directly. In
;; other words, you can always find some $C$ such that

;; \begin{equation}
;; C \circ \Gamma[q] = \Gamma[q']
;; \end{equation}

;; Because function composition is associative, instead of ever transforming the
;; path, you can push the coordinate transformation into the Lagrangian to generate
;; a new Lagrangian: $L = L' \circ C$.

;; The section of textbook just before the exercise has given us two Lagrangians in
;; different coordinates -- =L-central-polar= and =L-rectangular= -- and generated
;; Lagrange equations from each.

;; Our task is to directly transform the Lagrange equations by substituting the
;; first and second derivatives of the coordinate transformation expression into
;; one of the sets of equations, and looking to see that it's equivalent to the
;; other.

;; Fair warning: this is much more painful than transforming the Lagrangian
;; /before/ generating the Lagrange equations. This exercise continues the theme of
;; devastating you with algebra as a way to show you the horror that the later
;; techniques were developed to avoid. Let us proceed.


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

;; Here are the two Lagrangians from the book:


(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot)
               (square (* r phidot))) )
         (U r)))))


;; #+RESULTS:
;; : #| L-central-rectangular |#
;; :
;; : #| L-central-polar |#

;; Here are the rectangular equations of motion:


(->tex-equation
 (((Lagrange-equations
    (L-central-rectangular 'm (literal-function 'U)))
   (up (literal-function 'x)
       (literal-function 'y)))
  't)
 "eq:rect-equations")


;; #+RESULTS[3d6b30672d7d2fe77035ee8a5ae5b0f3046f2f90]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{ {{m {D}^{2}x\left( t \right) \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} + x\left( t \right) DU\left( \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} \right)}\over {\sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}}}}} \cr \cr \displaystyle{ {{m \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} {D}^{2}y\left( t \right) + y\left( t \right) DU\left( \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} \right)}\over {\sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}}}}}\end{bmatrix}
;; \label{eq:rect-equations}
;; \end{equation}


;; And the polar Lagrange equations:


(->tex-equation
  (((Lagrange-equations
      (L-central-polar 'm (literal-function 'U)))
    (up (literal-function 'r)
        (literal-function 'phi)))
   't))


;; #+RESULTS[557893e62f632f0900e9ece883c176eaf5bcfd05]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{  - m r\left( t \right) {\left( D\phi\left( t \right) \right)}^{2} + m {D}^{2}r\left( t \right) + DU\left( r\left( t \right) \right)} \cr \cr \displaystyle{ m {D}^{2}\phi\left( t \right) {\left( r\left( t \right) \right)}^{2} + 2 m r\left( t \right) D\phi\left( t \right) Dr\left( t \right)}\end{bmatrix}
;; \end{equation}

;; Once again, our goal is to show that, if you can write down coordinate
;; transformations for the coordinates, velocities and accelerations and substitute
;; them in to one set of Lagrange equations, the other will appear.

;; To do this by hand, take the coordinate transformation described in 1.64 in the
;; book:

;; \begin{equation}
;;   \begin{split}
;;     x &= r \cos \phi \cr
;;     y &= r \sin \phi
;;   \end{split}
;; \end{equation}

;; Note that $x$, $y$, $r$ and $\phi$ are functions of $t$. Take the derivative of
;; each equation (Use the product and chain rules) to obtain expressions for the
;; rectangular velocities in terms of the polar coordinates, just like equation
;; 1.66 in the book:

;; \begin{equation}
;;   \begin{split}
;;     Dx(t) &= Dr(t) \cos \phi(t) - r(t) D\phi(t) \sin \phi(t) \cr
;;     Dy(t) &= Dr(t) \sin \phi(t) + r(t) D\phi(t) \cos \phi(t)
;;   \end{split}
;; \end{equation}

;; The rectangular equations of motion have second derivatives, so we need to keep
;; going. This is too devastating to imagine doing by hand. Let's move to Scheme.

;; Write the coordinate transformation for polar coordinates to rectangular in
;; Scheme:


(define (p->r local)
  (let* ((polar-tuple (coordinate local))
         (r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))


;; #+RESULTS:
;; : #| p->r |#

;; Now use =F->C=, first described on page 46. This is a function that takes a
;; coordinate transformation like =p->r= and returns a /new/ function that can
;; convert an entire local tuple from one coordinate system to another; the $C$
;; discussed above.

;; The version that the book presents on page 46 can only generate a velocity
;; transformation given a coordinate transformation, but =scmutils= contains a more
;; general version that will convert as many path elements as you pass to it.

;; Here are the rectangular positions, velocities and accelerations, written in
;; polar coordinates:


(let ((convert-path (F->C p->r))
      (polar-path (up 't
                      (up 'r 'phi)
                      (up 'rdot 'phidot)
                      (up 'rdotdot 'phidotdot))))
  (->tex-equation
   (convert-path polar-path)))


;; #+RESULTS[1bac37835829a8c17e06699ef20f2e42676725b9]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ r \cos\left( \phi \right)} \cr \cr \displaystyle{ r \sin\left( \phi \right)}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{  - \dot{\phi} r \sin\left( \phi \right) + \dot{r} \cos\left( \phi \right)} \cr \cr \displaystyle{ \dot{\phi} r \cos\left( \phi \right) + \dot{r} \sin\left( \phi \right)}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{  - {\dot{\phi}}^{2} r \cos\left( \phi \right) - 2 \dot{\phi} \dot{r} \sin\left( \phi \right) - \ddot{\phi} r \sin\left( \phi \right) + \ddot{r} \cos\left( \phi \right)} \cr \cr \displaystyle{  - {\dot{\phi}}^{2} r \sin\left( \phi \right) + 2 \dot{\phi} \dot{r} \cos\left( \phi \right) + \ddot{\phi} r \cos\left( \phi \right) + \ddot{r} \sin\left( \phi \right)}\end{pmatrix}}\end{pmatrix}
;; \end{equation}


;; Ordinarily, it would be too heartbreaking to substitute these in to the
;; rectangular equations of motion. The fact that we have Scheme on our side gives
;; me the strength to proceed.

;; Write the rectangular Lagrange equations as a function of the local tuple, so we
;; can call it directly:


(define (rect-equations local)
  (let* ((q (coordinate local))
         (x (ref q 0))
         (y (ref q 1))

         (v (velocity local))
         (xdot (ref v 0))
         (ydot (ref v 1))

         (a (acceleration local))
         (xdotdot (ref a 0))
         (ydotdot (ref a 1))

         (U (literal-function 'U)))
    (up (/ (+ (* 'm xdotdot (sqrt (+ (square x) (square y))))
              (* x ((D U) (sqrt (+ (square x) (square y))))))
           (sqrt (+ (square x) (square y))))
        (/ (+ (* 'm ydotdot (sqrt (+ (square x) (square y))))
              (* y ((D U) (sqrt (+ (square x) (square y))))))
           (sqrt (+ (square x) (square y)))))))


;; #+RESULTS:
;; : #| rect-equations |#

;; Verify that these are, in fact, the rectangular equations of motion by passing
;; in a symbolic rectangular local tuple:


(let ((rect-path (up 't
                     (up 'x 'y)
                     (up 'xdot 'ydot)
                     (up 'xdotdot 'ydotdot))))
  (->tex-equation
   (rect-equations rect-path)))


;; #+RESULTS[5e06ee310a8c8e3036c17c5863647c80960dc568]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ {{m \ddot{x} \sqrt{{x}^{2} + {y}^{2}} + x DU\left( \sqrt{{x}^{2} + {y}^{2}} \right)}\over {\sqrt{{x}^{2} + {y}^{2}}}}} \cr \cr \displaystyle{ {{m \ddot{y} \sqrt{{x}^{2} + {y}^{2}} + y DU\left( \sqrt{{x}^{2} + {y}^{2}} \right)}\over {\sqrt{{x}^{2} + {y}^{2}}}}}\end{pmatrix}
;; \end{equation}

;; Now use the =p->r= conversion to substitute each of the rectangular values above
;; with their associated polar values:


(let* ((convert-path (F->C p->r))
       (polar-path (up 't
                       (up 'r 'phi)
                       (up 'rdot 'phidot)
                       (up 'rdotdot 'phidotdot)))
       (local (convert-path polar-path)))
  (->tex-equation
   (rect-equations local)))


;; #+RESULTS[577a75ce68ba364856b29d9b49e8f95c130ec027]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{  - m {\dot{\phi}}^{2} r \cos\left( \phi \right) - 2 m \dot{\phi} \dot{r} \sin\left( \phi \right) - m \ddot{\phi} r \sin\left( \phi \right) + m \ddot{r} \cos\left( \phi \right) + DU\left( r \right) \cos\left( \phi \right)} \cr \cr \displaystyle{  - m {\dot{\phi}}^{2} r \sin\left( \phi \right) + 2 m \dot{\phi} \dot{r} \cos\left( \phi \right) + m \ddot{\phi} r \cos\left( \phi \right) + m \ddot{r} \sin\left( \phi \right) + DU\left( r \right) \sin\left( \phi \right)}\end{pmatrix}
;; \end{equation}

;; Oh no. This looks quite different from the polar Lagrange equations above. What
;; is the problem?

;; I had to stare at this for a long time before I saw what to do. Notice that the
;; terms we want from the polar Lagrange equations all seem to appear in the first
;; equation with a $\cos \phi$, and in the second equation with a $\sin \phi$.
;; Using the trigonometric identity:

;; \begin{equation}
;; (\cos \phi)^2 + (\sin \phi)^2 = 1
;; \end{equation}

;; I realized that I could recover the first equation through a linear combination
;; of both terms. Multiply the first by $\cos \phi$ and the second by $\sin \phi$,
;; add them together and the unwanted terms all drop away.

;; A similar trick recovers the second equation,given an extra factor of $r$:


(let* ((convert-path (F->C p->r))
       (polar-path (up 't
                       (up 'r 'phi)
                       (up 'rdot 'phidot)
                       (up 'rdotdot 'phidotdot)))
       (local (convert-path polar-path))
       (eq (rect-equations local)))
  (->tex-equation
   (up (+ (* (cos 'phi) (ref eq 0))
          (* (sin 'phi) (ref eq 1)))
       (- (* 'r (cos 'phi) (ref eq 1))
          (* 'r (sin 'phi) (ref eq 0))))))
