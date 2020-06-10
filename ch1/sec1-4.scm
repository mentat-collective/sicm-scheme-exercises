;; Section 1.4: Computing Actions
;; :PROPERTIES:
;; :header-args+: :tangle ch1/sec1-4.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; This is the first demo of how any of this stuff works, starting on page 15.

;; Here's our first Lagrangian, super simple.


(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))


;; Suppose we let q denote a coordinate path function that maps time to position
;; components:


(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))


;; "Gamma" (looks like an L reflected across the origin) takes a coordinate path
;; and returns a function of time that gives the local tuple. Looks like that
;; fucker defaults to 2 levels deep, but you can call (Gamma 4) to get more derivatives.

;; The returned thing is called the "local tuple":


((Gamma q) 't)


;; This is just (t, q(t), (Dq)(t), ....) Where D is the derivative maybe of the
;; vector, which does partial derivatives along each component. (Q: can a component
;; of the coordinate path depend on the others? YES, and that would impose
;; constraints beyond the degrees of freedom.)


((compose (L-free-particle 'm) (Gamma q)) 't)


;; So this little bastard doesn't depend on the coordinate system you choose, as
;; long as its true that the lagrangian (kinetic energy equation) is the same for
;; all reference frames.

;; Lagrangian action! Minimal lagrangian action is key.


(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))


;; For a particle with mass 3, between 0 and 10... look at page 17 for an example
;; here. This is an example path:


(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))


;; And we can run it here:


(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
