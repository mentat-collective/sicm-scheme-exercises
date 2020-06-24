;; Paths of Minimum Action
;; :PROPERTIES:
;; :header-args+: :tangle ch1/min-action-paths.scm :comments org
;; :END:

;; This section takes us through an example action calculation on a path with an
;; adjustable "variation", or wiggle. We should see that, if we consider a
;; "realizable path", then any wiggle we add will increase the calculated action.


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

;; The only restriction on a variation is that it can't affect the endpoints of the
;; realizable path. the times and positions of the start and end of the path are
;; pinned.

;; =make-eta= takes some path $\nu$ and returns a function that wiggles in some
;; similar way to $\nu$, but equals 0 at $t_1$ and $t_2$:


(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))


;; #+RESULTS:
;; : #| make-eta |#

;; Next, define a function that calculates the Lagrangian for a free particle, like
;; before, but adds in the path variation $\eta$ multiplied by some small scaling factor
;; $\epsilon$:


(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* epsilon eta))
                       t1
                       t2)))


;; #+RESULTS:
;; : #| varied-free-particle-action |#

;; Consider some variation like $v(t) = (\sin(t), \cos(t), t^2)$. The action of the
;; path with this small wiggle (processed through =make-eta= to pin its endpoints)
;; is larger (top entry) than the action of the non-varied path (bottom entry), as
;; expected:


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


;; #+RESULTS[10c4c8e6a40701b7176b4b1b2e9dd30b80185e6f]:
;; #| test-path |#

;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 436.2912142857153} \cr \cr \displaystyle{ 435.}\end{pmatrix}
;; \end{equation}

;; What value of $\epsilon$ minimizes the action for the test path?

;; We can search over values of $\epsilon$ from $-2.0$ to $1.0$ using the built-in
;; =minimize= function:


(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (->tex-equation
   (car (minimize action-fn -2.0 1.0))))
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

;; The next function sort-of-composes =make-path= and =Lagrangian-action= into a
;; function that takes $L$ and the endpoints, and returns the total action along
;; the path.


(define ((parametric-path-action L t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action L path t0 t1)))


;; #+RESULTS:
;; : #| parametric-path-action |#

;; Finally, =find-path= takes the previous function's arguments, plus a parameter
;; $n$. $n$ controls how many intermediate points the optimizer will inject and
;; modify in its attempt to find an action-minimizing path. The more points you
;; specify, the longer minimization will take, but the more accurate the final path
;; will be.


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
;; with spring constant $k$. Here is the Lagrangian, equal to the kinetic energy
;; minus the potential from the spring:

;; #+name: L-harmonic

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))


;; #+RESULTS:
;; : #| L-harmonic |#

;; Now we invoke the procedures we've built, and plot the final, path-minimizing
;; trajectory.


(define harmonic-path
  (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 3))

(define win2 (frame 0.0 :pi/2 0 1))

(plot-function win2 harmonic-path 0 :pi (/ :pi 100))
