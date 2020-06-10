;; Exercise 1.5: Solution process
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-5.scm :comments org
;; :END:

;; watch the progress of the minimization. This is not great design, since we're
;; overwriting the previous function and depending on the closure, but let's
;; follow the text.


(load "ch1/utils.scm")


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
