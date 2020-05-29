;; Exercise 1.6: Minimizing action
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-6.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; The problem asks:

;; #+begin_quote
;; Suppose we try to obtain a path by minimizing an action for an impossible
;; problem. For example, suppose we have a free particle and we impose endpoint
;; conditions on the velocities as well as the positions that are inconsistent with
;; the particle being free. Does the formalism protect itself from such an
;; unpleasant attack? You may find it illuminating to program it and see what
;; happens.
;; #+end_quote

;; I spent some time thinking about this, because when I attacked this book five
;; years ago this problem clearly tripped me up.

;; Let's say you take, as they suggest, some path, and impose velocity constraints
;; on the endpoints in addition to the position constraints.

;; Usually, you constrain the coordinates at each endpoint and force a path that
;; minimizes the action between two times. So what does it mean to impose velocity
;; conditions? The key is to realize that on the computer, you're forcing a path to
;; be composed of a bunch of discrete points. If you can force a point into the
;; path that is NOT controlled by the optimizer, then you can force a velocity at
;; some point in the path that makes no sense for minimal action.


(define (((parametric-path-action* win)
          Lagrangian t0 q0 offset0 t1 q1 offset1)
         intermediate-qs)
  (let ((intermediate-qs* (append (list (- q0 offset0))
                                  intermediate-qs
                                  (list (+ q1 offset1)))))
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs*)))
      ;; display path
      (graphics-clear win)
      (plot-function win path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1))))

;; Version of find path that allows for an offset to the initial and
;; final points.

(define ((find-path* win) L t0 q0 offset0 t1 q1 offset1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let* ((action (parametric-path-action* win))
           (minimizing-qs
            (multidimensional-minimize
             (action L t0 q0 offset0 t1 q1 offset1)
             initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))

;; This runs (and graphs!) the motion of a free particle using the
;; fucked up path.
(define (one-six offset0 offset1 n)
  (let* ((tmax 10)
         (win (frame -1 (+ tmax 1) 0. (+ 1.2 offset0 offset1)))
         (find (find-path* win))
         (L (L-free-particle 3.0))
         (path (find L
                     0. 1. offset0
                     tmax 0. offset1
                     n)))
    (Lagrangian-action L path 0 tmax)))


;; What happens when you program this? You get a funky, wiggly path like this:
;; (insert the gif!)

;; And you can show that the action you calculate is NOT in fact the minimum.
;; Here's an interface that says "take 3 interpolated points, and force an offset
;; point of a small amount into the action.


(one-six 0 0 3)
(one-six 0.2 0 3)
