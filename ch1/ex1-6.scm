;; Exercise 1.6: Minimizing action
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-6.scm :comments org
;; :END:

;; The authors have lightly demonstrated that plausible-looking paths have
;; stationary action between fixed endpoints. What happens if we overconstrain the
;; problem?

;; The exercise asks:

;; #+begin_quote
;; Suppose we try to obtain a path by minimizing an action for an impossible
;; problem. For example, suppose we have a free particle and we impose endpoint
;; conditions on the velocities as well as the positions that are inconsistent with
;; the particle being free. Does the formalism protect itself from such an
;; unpleasant attack? You may find it illuminating to program it and see what
;; happens.
;; #+end_quote


(load "ch1/utils.scm")
;; Implementation

;; Here's the implementation of the modification described earlier:


(define (((parametric-path-action* win)
          Lagrangian t0 q0 offset0 t1 q1 offset1)
         intermediate-qs)
  ;; See the two new points?
  (let ((intermediate-qs* (append (list (+ q0 offset0))
                                  intermediate-qs
                                  (list (+ q1 offset1)))))
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs*)))
      ;; display path
      (graphics-clear win)
      (plot-function win path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1))))


;; #+RESULTS:
;; : #| parametric-path-action* |#

;; You might try a similar trick by modifying the first and last entries of
;; =intermediate-qs= instead of appending a point, but I suspect that the optimizer
;; would be able to figure out how to undo your offset. (Try this as an exercise.)

;; Next, a new version of =find-path= that passes the offsets through to the new
;; =parametric-path-action*=:


(define ((find-path* win) L t0 q0 offset0 t1 q1 offset1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let* ((action (parametric-path-action* win))
           (minimizing-qs
            (multidimensional-minimize
             (action L t0 q0 offset0 t1 q1 offset1)
             initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))


;; #+RESULTS:
;; : #| find-path* |#

;; And finally, a function that can execute runs of our formalism-killing
;; experiment.


(define (one-six offset0 offset1 n)
  (let* ((tmax 10)
         (win (frame -1 (+ tmax 1) -0.2 (+ 1.2 offset0 offset1)))
         (find (find-path* win))
         (L (L-free-particle 3.0))
         (path (find L
                     0. 1. offset0
                     tmax 0. offset1
                     n)))
    (Lagrangian-action L path 0 tmax)))
;; Execution

;; Let's run the code with 0 offsets and 3 interpolation points. Note that this
;; should /still/ distort the path, since we now have two fixed points at the start
;; and end. This is effectively imposing a 0 velocity constraint at the beginning
;; and end.

;; Here's the code, and its output:


(one-six 0 0 3)


;; #+DOWNLOADED: file:///Users/samritchie/Desktop/ex1_6_nooffset.gif @ 2020-06-10 15:10:46
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_15-10-46_ex1_6_nooffset.gif]]


;; The path ends up looking almost sinusoidal, and takes a while to converge. This
;; is the best polynomial that the system can come up with that matches the 7
;; points (3 interpolated, 2 offsets, 1 start and 1 end).

;; The actual realizable path should be a straight line between the two points. The
;; initial velocity of

;; Here's a small positive velocity imposed at the beginning, and 0 at the end:


(one-six 0.2 0 3)


;; #+DOWNLOADED: file:///Users/samritchie/Desktop/ex1_6_02offset.gif @ 2020-06-10 15:10:53
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_15-10-53_ex1_6_02offset.gif]]

;; The system takes longer to converge. Here's a larger impulse of 0.5 at the
;; beginning:


(one-six 0.5 0 3)


;; #+DOWNLOADED: file:///Users/samritchie/Desktop/ex1_6_05offset.gif @ 2020-06-10 15:11:10
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_15-11-10_ex1_6_05offset.gif]]


;; And a moderate negative velocity, just for fun:


(one-six -0.5 0 3)


;; #+DOWNLOADED: file:///Users/samritchie/Desktop/ex1_6_neg5offset.gif @ 2020-06-10 15:11:27
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_15-11-27_ex1_6_neg5offset.gif]]


;; The process __does__ converge, but this is only because we only used 3
;; intermediate points. If you bump up to 10 points, with this code:


(one-six -0.5 0 10)
