;; Paths of Minimum Action
;; :PROPERTIES:
;; :header-args+: :tangle ch1/min-action-paths.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

;; This section in the textbook implements path variation, so we can see the action
;; change (and increase!) off of the optimal path.

;; =make-eta= returns a function that equals 0 at $t_1$ and $t_2$:


(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))


;; #+RESULTS:
;; : #| make-eta |#

;; Next, define a function that calculates the Lagrangian for a free particle, like
;; before, but adds in the path variation multiplied by some small scaling factor
;; $\epsilon$.


(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* epsilon eta))
                       t1
                       t2)))


;; #+RESULTS:
;; : #| varied-free-particle-action |#

;; The action for a small variation of $v(t) = (\sin(t), \cos(t), t^2)$ is larger
;; (top entry) vs the non-varied path (bottom entry), as expected.


(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(let ((action-fn (varied-free-particle-action 3.0 test-path
                                              (up sin cos square)
                                              0.0 10.0)))
  (->tex-equation
   (up (action-fn 0.001)
       (action-fn 0))))


;; #+RESULTS[3b26d789e2b0e25f6eb715c26a42c1ba4afe9017]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 436.2912142857153} \cr \cr \displaystyle{ 435.}\end{pmatrix}
;; \end{equation}

;; What value of $\epsilon$ minimizes the action for the test path?

;;  Search over -2.0 to 1.0:


(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (->tex-equation
   (minimize action-fn -2.0 1.0)))
;; Finding trajectories that minimize the action

;; Is it possible to use this principle to actually /find/ a path, instead of
;; simply checking it?

;; First we need a function that builds a path. This version generates a path of
;; individual points, bracketed by the supplied start and end points $(t_0, q_0)$
;; and $(t_1, q_1)$. $qs$ is a list of intermediate points.


(define (make-path t0 q0 t1 q1 qs)
  (let ((n (length qs)))
    (let ((ts (linear-interpolants t0 t1 n)))
      (Lagrange-interpolation-function
       (append (list q0) qs (list q1))
       (append (list t0) ts (list t1))))))


;; #+RESULTS:
;; : #| make-path |#

;; This function sort-of-composes =make-path= and =Lagrangian-action=:


(define ((parametric-path-action L t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action L path t0 t1)))


;; #+RESULTS:
;; : #| parametric-path-action |#

;; Finally, a function that generates a path that minimizes the action:


(define (find-path L t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action L t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))


;; #+RESULTS:
;; : #| find-path |#

;; Let's test it out with a Lagrangian for a one dimensional harmonic oscillator
;; with spring constant $k$:

;; #+name: L-harmonic

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))


;; #+RESULTS:
;; : #| L-harmonic |#


(define win2 (frame 0.0 :pi/2 0 1))

(define harmonic-path
  (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 3))

(plot-function win2 harmonic-path 0 :pi (/ :pi 100))
