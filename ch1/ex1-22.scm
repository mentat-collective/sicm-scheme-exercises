;; Exercise 1.22: Driven pendulum
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-22.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; Part B: Lagrangian

;; Now write the Lagrangian for the driven pendulum in rectangular coordinates. The
;; constraint force takes the same shape as in exercise 1.21:


(define (U-constraint q0 q1 F l)
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))


;; #+RESULTS:
;; : #| U-constraint |#

;; The Lagrangian is similar, but only involves a single particle -- the pendulum
;; bob. We can generate the constraint force by directly building the support's
;; coordinates, rather than extracting them from the local tuple.

;; #+name: L-driven-free

(define ((L-driven-free m l y U) local)
  (let* ((extract (extract-particle 2))
         (bob (extract local 0))
         (q (coordinate bob))
         (qdot (velocity bob))
         (F (ref (coordinate local) 2)))
    (- (KE-particle m qdot)
       (U q)
       (U-constraint (up 0 (y (time local)))
                     q
                     F
                     l))))


;; Here is the now-familiar equation for a uniform gravitational potential, acting
;; on the $y$ coordinate:

;; #+name: U-gravity

(define ((U-gravity g m) q)
  (let* ((y (ref q 1)))
    (* m g y)))


;; #+RESULTS:
;; : #| U-gravity |#

;; Now use the new Lagrangian to generate equations of motion for the three
;; coordinates $x$, $y$ and $F$:


(let* ((q (up (literal-function 'x)
              (literal-function 'y)
              (literal-function 'F)))
       (U (U-gravity 'g 'm))
       (y (literal-function 'y_s))
       (L (L-driven-free 'm 'l y U))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))


;; #+RESULTS[a7c6fc40bbcba84c2c0ef9058455e719c4ac8045]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{ {{l m {D}^{2}x\left( t \right) + F\left( t \right) x\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{g l m + l m {D}^{2}y\left( t \right) - F\left( t \right) {y}_{s}\left( t \right) + F\left( t \right) y\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{ - {{1}\over {2}} {l}^{2} + {{1}\over {2}} {\left( {y}_{s}\left( t \right) \right)}^{2} - {y}_{s}\left( t \right) y\left( t \right) + {{1}\over {2}} {\left( y\left( t \right) \right)}^{2} + {{1}\over {2}} {\left( x\left( t \right) \right)}^{2}}\over {l}}}\end{bmatrix}
;; \end{equation}

;; The first two equations of motion match the equations we derived in part A,
;; using Newton's equations. The third states that

;; \begin{equation}
;; l^2 = x(t)^2 + (y_s(t) - y(t))^&2
;; \end{equation}

;; Verified, with some extra terms to force the simplification:


(let* ((q (up (literal-function 'x)
              (literal-function 'y)
              (literal-function 'F)))
       (U (U-gravity 'g 'm))
       (y (literal-function 'y_s))
       (L (L-driven-free 'm 'l y U))
       (f ((Lagrange-equations L) q))
       (eq (ref (f 't) 2)))
  (->tex-equation
   (- eq
      (/ (* 1/2 (- (+ (square ((literal-function 'x) 't))
                      (square ((- y (literal-function 'y)) 't)))
                   (square 'l)))
         'l))))
;; Part C: Coordinate Change

;; Now we want to verify that we get the same Lagrangian and equations of motion as
;; in 1.88 from the book. We also want to analyze the constraint forces. To do this
;; we need to introduce a coordinate change.

;; To analyze the constraint forces, we have to do the same trick as in exercise
;; 1.21 and use a coordinate $c(t) = l$. The new coordinates are $(\theta, c, F)$:

;; #+name: driven-polar->rect

(define ((driven-polar->rect y) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (c (ref q 1))
         (F (ref q 2)))
    (up (* c (sin theta))
        (- (y (time local)) (* c (cos theta)))
        F)))


;; #+RESULTS:
;; : #| driven-polar->rect |#

;; Compose the coordinate change with the rectangular Lagrangian:

;; #+name: L-driven-pend

(define (L-driven-pend m l y U)
  (compose (L-driven-free m l y U)
           (F->C (driven-polar->rect y))))


;; #+RESULTS:
;; : #| L-driven-pend |#

;; Examine the Lagrangian itself, after the coordinate transformation. (Notice that
;; we're using a constant function for $c(t)$ that always returns $l$.)


(let* ((q (up (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (y (literal-function 'y_s))
       (L (L-driven-pend 'm 'l y (U-gravity 'g 'm)))
       (f (compose L (Gamma q))))
  (->tex-equation
   (f 't)))


;; #+RESULTS[306c957e09bfd22ebe85e5c6fb9380ff977df1e9]:
;; \begin{equation}
;; {{1}\over {2}} {l}^{2} m {\left( D\theta\left( t \right) \right)}^{2} + l m D{y}_{s}\left( t \right) \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) + g l m \cos\left( \theta\left( t \right) \right) - g m {y}_{s}\left( t \right) + {{1}\over {2}} m {\left( D{y}_{s}\left( t \right) \right)}^{2}
;; \end{equation}

;; Looks just like equation 1.88.

;; Next, examine the Lagrange equations, using the same substitution of $c(t) = l$:


(let* ((q (up (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (y (literal-function 'y_s))
       (L (L-driven-pend 'm 'l y (U-gravity 'g 'm)))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))
