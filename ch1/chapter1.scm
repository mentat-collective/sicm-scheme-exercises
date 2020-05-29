(load "ch1/utils.scm")

;; Suppose we let q denote a coordinate path function that maps time
;; to position components:

(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

;; "Gamma" (looks like an L reflected across the origin) takes a
;; coordinate path and returns a function of time that gives the local
;; tuple. Looks like that fucker defaults to 2 levels deep, but you
;; can call (Gamma 4) to get more derivatives.
;;
;; The returned thing is called the "local tuple" (def!)
((Gamma q) 't)
;; => (up t (up (x t) (y t) (z t)) (up ((D x) t) ((D y) t) ((D z) t)))
;;
;; This is just (t, q(t), (Dq)(t), ....) Where D is the derivative
;; maybe of the vector, which does partial derivatives along each
;; component. (Q: can a component of the coordinate path depend on the
;; others? No, because that would impose constraints beyond the
;; degrees of freedom. Maybe later we'll look at that.)

((compose (L-free-particle 'm) (Gamma q)) 't)
#|
(+ (* 1/2 m (expt ((D z) t) 2))
   (* 1/2 m (expt ((D y) t) 2))
   (* 1/2 m (expt ((D x) t) 2)))
|#

;; So this little bastard doesn't depend on the coordinate system you
;; choose, as long as its true that the lagrangian (kinetic energy
;; equation) is the same for all reference frames.
;;
;; Lagrangian action! Minimal lagrangian action is key.
(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

;; For a particle with mass 3, between 0 and 10...
(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
;;=> 435.0

;; Exercise 1.4:
;;
;; For a straight line... this one goes down by hand, check Roam.

;; ## Paths of Minimum action!
;;
;; Next, we'll take the old langrangian calculation for the straight
;; line and fuck with the line by small variations; or some small
;; epsilon multiplied by the effect of some other function added to
;; the original q. Eta baby!

;; This makes a new function that has zeroes at t1 and t2.
(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))

;; Boom, calculate the action AGAIN for a path that's pretty close...
(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* epsilon eta))
                       t1
                       t2)))

(let ((action-fn (varied-free-particle-action 3 test-path
                                              (up sin cos square)
                                              0.0 10.0)))
  (action-fn 0.001))
;;=> #| 436.2912142857153 |#

;; Now we can do a minimization over -2.0 to 1.0:
(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (minimize action-fn -2.0 1.0))

#|
(5.134781488891349e-15 435. 5)
|#

;; ## Finding trajectories that minimize action:
;;
;; First build up a function that hits all the listed points.

(define (make-path t0 q0 t1 q1 qs)
  (let ((n (length qs)))
    (let ((ts (linear-interpolants t0 t1 n)))
      (Lagrange-interpolation-function
       (append (list q0) qs (list q1))
       (append (list t0) ts (list t1))))))

(define ((parametric-path-action L t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action L path t0 t1)))

(define (find-path L t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action L t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))

;; That's pretty sick. Now we can write a version that does some
;; mother fucking plotting. First, a new lagrangian for a spring
;; system. This one's the difference between the kinetic and potential
;; energies of a spring system.

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

;; Exercise 1.5
;;
;; watch the progress of the minimization. This is not great design, since we're
;; overwriting the previous function and depending on the closure, but let's
;; follow the text.
;;
;; Defines a window:

(define win2
  (frame 0. :pi/2 0. 1.2))

;; new version of this that prints:

(define ((parametric-path-action Lagrangian t0 q0 t1 q1)
         intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
    ;; display path
    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
    ;; compute action
    (Lagrangian-action Lagrangian path t0 t1)))

;; And boom, we find a path (and get to watch a pretty chart):

(define (run-q)
  (find-path (L-harmonic 1.0 1.0) 0. 1. :pi/2 0. 3))

;; Page 31, Orbital Motion example

(define ((L-orbital mass mu) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (+ (* 1/2 mass (square qdot))
       (/ mu (sqrt (square q))))))

(define q2
  (up (literal-function 'xi)
      (literal-function 'eta)))

;; to test:
((compose ((partial 1) (L-orbital 'm 'mu)) (Gamma q2)) 't)

;; Exercise 1.9*, ps1, p32 - Lagrange's Equations
;; Exercise 1.12*, ps1, p36 is the same thing, on the computer.

;; Exercise 1.10, p32 - Higher-derivative Lagrangians
;; Exercise 1.13, 36 is the same thing, on the computer.

;; Exercise 1.14, p43 - Coordinate-independence of Lagrange Equations. Test this
;; out on the comp, but it's obvious once you prove the next exercise, 1.15.

;; Exercise 1.15, p46 - took forever in the notebook :)

;; Exercise 1.16*, ps2, p47 - Central force motion
