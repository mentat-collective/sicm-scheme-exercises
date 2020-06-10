;; Paths of Minimum Action
;; :PROPERTIES:
;; :header-args+: :tangle ch1/min-action-paths.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; Next, we'll take the old langrangian calculation for the straight line and fuck
;; with the line by small variations; or some small epsilon multiplied by the
;; effect of some other function added to the original q. Eta baby!

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



;; Now we can do a minimization over -2.0 to 1.0:


(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (minimize action-fn -2.0 1.0))
;; Finding trajectories that minimize action

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


;; That's pretty sick. Now we can write a version that does some mother fucking
;; plotting. First, a new lagrangian for a spring system. This one's the difference
;; between the kinetic and potential energies of a spring system.


(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))
