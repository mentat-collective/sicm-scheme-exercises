;; Exercise 1.11: Kepler's third law
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-11.scm :comments org
;; :END:

;; This exercise asks us to derive [[https://en.wikipedia.org/wiki/Kepler%27s_laws_of_planetary_motion#Third_law_of_Kepler][Kepler's third law]] by considering a Langrangian
;; that describes two particles rotating in a circular orbit around their center of
;; mass at some rate.


(load "ch1/utils.scm")


;; Here's the Lagrangian for "central force", in polar coordinates. This is
;; rotational kinetic energy, minus some arbitrary potential $V$ that depends on
;; the distance $r$ between the two particles.


(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))     (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot) (square (* r phidot))))
         (V r)))))


;; #+RESULTS:
;; : #| L-central-polar |#

;; This function defines gravitational potential energy:


(define ((gravitational-energy G m1 m2) r)
  (- (/ (* G m1 m2) r)))


;; #+RESULTS:
;; : #| gravitational-energy |#

;; What is the mass $m$ in the Lagrangian above? It's the "[[https://en.wikipedia.org/wiki/Reduced_mass][reduced mass]]", totally
;; unjustified at this point in the book:


(define (reduced-mass m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))


;; #+RESULTS:
;; : #| reduced-mass |#

;; If you want to see why the reduced mass has the form it does, check out [[https://en.wikipedia.org/wiki/Reduced_mass#Lagrangian_mechanics][this
;; derivation]].

;; The Lagrangian is written in terms of some angle $\phi$ and $r$, the distance
;; between the two particles. $q$ defines a circular path:


(define ((q r omega) t)
  (let ((phi (* omega t)))
    (up r phi)))


;; #+RESULTS:
;; : #| q |#

;; Write the Lagrange equations, given $r = a$ and $\omega = n$:


(let ((eqfn (Lagrange-equations
             (L-central-polar (reduced-mass 'm1 'm2)
                              (gravitational-energy 'G 'm1 'm2)))))
  (->tex-equation
   ((eqfn (q 'a 'n)) 't)))


;; #+RESULTS[e8565d29067487b8460bcb96e1a427d9eef22a0c]:
;; \begin{equation}
;; \begin{bmatrix} \displaystyle{ {{ - {a}^{3} \cdot m1 \cdot m2 \cdot {n}^{2} + G {m1}^{2} \cdot m2 + G \cdot m1 \cdot {m2}^{2}}\over {{a}^{2} \cdot m1 + {a}^{2} \cdot m2}}} \cr \cr \displaystyle{ 0}\end{bmatrix}
;; \end{equation}

;; These two entries are /residuals/, equal to zero. Stare at the top residual and
;; you might notice that you can can factor out:

;; - the reduced mass, and
;; - a factor of $1 \over a^2$

;; Manually factor these out:


(let ((eqfn (Lagrange-equations
             (L-central-polar (reduced-mass 'm1 'm2)
                              (gravitational-energy 'G 'm1 'm2)))))
  (->tex-equation
   (* ((eqfn (q 'a 'n)) 't)
      (/ (square 'a)
         (reduced-mass 'm1 'm2)))))
