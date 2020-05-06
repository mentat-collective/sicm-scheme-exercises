;; Structure and Interpretation of Classical Mechanics
;;
;; ## Section 1.4
;;
;; 1/2 mv^2. This is the Lagrangian for a free particle, though I have no idea
;; "why". Nor would I have thought about it had the text not said "we know you
;; don't know why"...
;;
;; Now it's 2020, and I totally know why. The Lagrangian is defined this way
;; because there's no potential, no other forces acting on the particle; so all
;; it has is its kinetic energy.
;;
;; WHY is the Lagrangian defined this way? Because, when we can split the
;; functions into one that depends on velocity ("kinetic energy") and a
;; potential that depends only on position, it just works out this way.

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

;; * would give you matrix multiplication. If you multiply an "up" by
;; a "down" that's equal to the dot product. So "up" is a row vector,
;; down is a column vector.
;;
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
;; For a straight line... this one goes down by hand. No big.

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

;; ## Exercise 1.6
;;
;; Suppose we try to obtain a path by minimizing an action for an
;; impossible problem. For example, suppose we have a free particle
;; and we impose endpoint conditions on the velocities as well as the
;; positions that are inconsistent with the particle being free. Does
;; the formalism protect itself from such an unpleasant attack? You
;; may find it illuminating to program it and see what happens.
;;
;;
;; Answer: I did this one based on some stuff I found online, by
;; forcing the linear interpolation, then adding an offset to the
;; start and finish. I didn't really get how the question was
;; phrased. Are you supposed to fuck up the implementation of gamma
;; and calculate a derivative that just doesn't make sense?
;;
;; The multidimensional-minimize function seemed to have a harder time
;; figuring out what to do, but I think the action still HAS a minimum
;; value. It just didn't make sense physically. So it "works", just
;; doesn't match the physics.
;;
;; Anyway, I promised myself I wouldn't get stuck on problems like
;; this. Better to move on and come back to it later.
;;
;; This version of parametric-path-action lets us add an offset to
;; some initial points after the interpolation.

(define ((parametric-path-action*
          Lagrangian t0 q0 offset0 t1 q1 offset1)
         intermediate-qs)
  (let ((intermediate-qs* (append (list (- q0 offset0))
                                  intermediate-qs
                                  (list (+ q1 offset1)))))
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs*)))
      ;; display path
      (graphics-clear win2)
      (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1))))

;; Version of find path that allows for an offset to the initial and
;; final points.
(define (find-path* L t0 q0 offset0 t1 q1 offset1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action* L t0 q0 offset0 t1 q1 offset1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))

;; This runs (and graphs!) the motion of a free particle using the
;; fucked up path.
(define (one-six)
  (find-path* (L-free-particle 3) 0. 1. 1. 10. 2. 1. 3))
