;; Exercise 1.11: Kepler's third law
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-11.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; Lagrangian from the problem:


(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))     (phi (ref q 1))
                            (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot) (square (* r phidot))) )
         (V r)))))


;; #+RESULTS:
;; : #| L-central-polar |#

;; Helper function, plus a Scheme version of the reduced mass definition:


(define ((gravitational-energy G m1 m2) r)
  (- (/ (* G m1 m2) r)))

(define (reduced-mass m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))


;; #+RESULTS:
;; : #| gravitational-energy |#
;; :
;; : #| reduced-mass |#

;; The Lagrangian is written in terms of some angle $\phi$ and $r$, the distance
;; between the two particles. $q$ defines a circular path:


(define ((q r omega) t)
  (let ((phi (* omega t)))
    (up r phi)))


;; #+RESULTS:
;; : #| q |#

;; Let's write the Lagrange equations, given $r = a$ and $\omega = n$:


(let ((eqfn (Lagrange-equations
             (L-central-polar (reduced-mass 'm1 'm2)
                              (gravitational-energy 'G 'm1 'm2)))))
  (->tex-equation
   ((eqfn (q 'a 'n)) 't)))


;; #+RESULTS[e8565d29067487b8460bcb96e1a427d9eef22a0c]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{ {{ - {a}^{3} \cdot m1 \cdot m2 \cdot {n}^{2} + G {m1}^{2} \cdot m2 + G \cdot m1 \cdot {m2}^{2}}\over {{a}^{2} \cdot m1 + {a}^{2} \cdot m2}}} \cr \cr \displaystyle{ 0}\end{bmatrix}
;; \end{equation}

;; If you fiddle with this you should see that you can factor out the reduced mass
;; and an $a^2$ term. The whole equations is zero, so it's fine to multiply the
;; residual by any non-zero factor you like.


(let ((eqfn (Lagrange-equations
             (L-central-polar (reduced-mass 'm1 'm2)
                              (gravitational-energy 'G 'm1 'm2)))))
  (->tex-equation
   (* ((eqfn (q 'a 'n)) 't)
      (/ (square 'a)
         (reduced-mass 'm1 'm2)))))
