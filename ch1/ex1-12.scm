;; Exercise 1.12: Lagrange's equations (code)
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-12.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; This exercise has us writing code for the three systems described in [[https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-9][Exercise
;; 1.9]]. Before we start, here's a function that will display an up-tuple of:

;; - $\partial_1 L \circ \Gamma[q]$, the generalized force
;; - $\partial_2 L \circ \Gamma[q]$, the generalized momenta
;; - $D(\partial_2 L \circ \Gamma[q])$, the derivative of our momenta
;; - The Lagrange equations for the system.


(define (lagrange-equation-steps L q)
  (let* ((p1 (compose ((partial 1) L) (Gamma q)))
         (p2 (compose ((partial 2) L) (Gamma q)))
         (dp2 (D p2)))
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

;; Here is the Lagrangian described above:


(define ((L-pendulum m g l) local)
  (let ((theta (coordinate local))
        (theta_dot (velocity local)))
    (- (* 1/2 m (square l) (square theta_dot))
       (* m g l (cos theta)))))


;; #+RESULTS:
;; : #| L-pendulum |#

;; And the steps that lead us to Lagrange's equations:


(lagrange-equation-steps
 (L-pendulum 'm 'g 'l)
 (literal-function 'theta))


;; #+RESULTS[aaf5812bee20b3464cf996ad648f7000e663545b]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ g l m \sin\left( \theta\left( t \right) \right)} \cr \cr \displaystyle{ {l}^{2} m D\theta\left( t \right)} \cr \cr \displaystyle{ {l}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{  - g l m \sin\left( \theta\left( t \right) \right) + {l}^{2} m {D}^{2}\theta\left( t \right)}\end{pmatrix}
;; \end{equation}

;; The final Lagrange equation needs some hand simplification. Divide out $g$ and
;; $l$, and include a factor of $-1$ to make things look nice:


(let* ((L (L-pendulum 'm 'g 'l))
       (theta (literal-function 'theta))
       (eq ((Lagrange-equations L) theta)))
  (->tex-equation
   ((/ eq (* -1 'm 'l))
    't)))
;; Part B: 2D Potential

;; From the book:

;; #+begin_quote
;; A particle of mass $m$ moves in a two-dimensional potential $V(x, y) = {(x^2 +
;; y^2) \over 2} + x^2 y - {y^3 \over 3}$, where $x$ and $y$ are rectangular
;; coordinates of the particle. A Lagrangian is $L(t;x, y; v_x, v_y) = {1 \over 2}
;; m (v_x^2 + v_y^2) - V(x, y)$.
;; #+end_quote

;; Define the potential $V$ and the Lagrangian separately. The Lagrangian here is
;; actually written in a form general enough that it would work for any number of
;; dimensions in rectangular space, given some potential $V$. This is something to
;; note for later, when we discuss coordinate transformations.


(define (V q)
  (let ((x (ref q 0))
        (y (ref q 1)))
    (- (+ (/ (+ (square x)
                (square y))
             2)
          (* (square x) y))
       (/ (cube y) 3))))

(define (((L-2d-potential m) V) local)
  (- (* 1/2 m (square (velocity local)))
     (V (coordinate local))))


;; #+RESULTS:
;; : #| V |#
;; :
;; : #| L-2d-potential |#

;; Next, the derivation of the Lagrange equations:


(lagrange-equation-steps
 ((L-2d-potential 'm) V)
 (up (literal-function 'x)
     (literal-function 'y)))
;; Part C: Particle on a Sphere

;; From the book:

;; #+begin_quote
;; A Lagrangian for a particle of mass $m$ constrained to move on a sphere of
;; radius $R$ is $L(t; \theta, \phi; \alpha, \beta) = {1 \over 2} m
;; R^2(\alpha^2+(\beta \sin\theta)^2)$. The angle $\theta$ is the colatitude of the
;; particle and $\phi$ is the longitude; the rate of change of the colatitude is
;; $\alpha$ and the rate of change of the longitude is $\beta$.
;; #+end_quote

;; Here is the Lagrangian:


(define ((L-sphere m R) local)
  (let* ((q (coordinate local))
         (qdot (velocity local))
         (theta (ref q 0))
         (alpha (ref qdot 0))
         (beta (ref qdot 1)))
    (* 1/2 m (square R)
       (+ (square alpha)
          (square (* beta (sin theta)))))))


;; #+RESULTS:
;; : #| L-sphere |#

;; The final Lagrange equations have a few terms that we can cancel out. Scheme
;; doesn't know that these are meant to be residuals, so it won't cancel out
;; factors that we can see by eye are missing. Here is the full derivation:


(lagrange-equation-steps
 (L-sphere 'm 'R)
 (up (literal-function 'theta)
     (literal-function 'phi)))


;; #+RESULTS[5d05668e1d73bcd0f884eb8ba013c4eb56e72fda]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2}} \cr \cr \displaystyle{ 0}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m D\theta\left( t \right)} \cr \cr \displaystyle{ {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} D\phi\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {R}^{2} m \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\phi\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{  - {R}^{2} m \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + {R}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {R}^{2} m \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\phi\left( t \right)}\end{bmatrix}}\end{pmatrix}
;; \end{equation}

;; Next, get the Lagrange equations in hand and manually simplify each equation by
;; dividing out, respectively, $mR^2$ and $mR^2 \sin \theta$:


(let* ((L (L-sphere 'm 'R))
       (theta (literal-function 'theta))
       (q (up theta (literal-function 'phi)))
       (le ((Lagrange-equations L) q)))
  (let ((eq1 (ref le 0))
        (eq2 (ref le 1)))
    (->tex-equation
     ((up (/ eq1 (* 'm (square 'R)))
          (/ eq2 (* (sin theta) 'm (square 'R))))
      't))))
