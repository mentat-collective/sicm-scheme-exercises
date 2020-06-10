;; Exercise 1.21: A dumbbell
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-21.scm :comments org
;; :END:

;; The uneven dumbbell.

;; NOTE for when I write this up. This exercise is quite careful to NOT change the
;; dimension of the configuration space, when it does coordinate transformations.
;; We show later that you can do that, but that's the reason, good to note, why you
;; introduce a new variable $c$ that's equal to the distance between the dumbbells.


(load "ch1/utils.scm")


;; Takes in any number of up tuples and zips them into a new list of up-tuples by
;; taking each element.


(define (up-zip . ups)
  (apply vector-map up (map up->vector ups)))


;; I spent some time trying to make a nice API... but without map, filter,
;; reduce etc on tuples it is quite annoying. So let's go ad hoc first and see
;; what happens.


(define (KE-particle m v)
  (* 1/2 m (square v)))
;; gets the particle itself
(define ((extract-particle pieces) local i)
  (let* ((q (coordinate local))
         (qdot (velocity local))
         (indices (apply up (iota pieces (* i pieces))))
         (extract (lambda (tuple)
                    (vector-map (lambda (i) (ref tuple i))
                                indices))))
    (up (time q)
        (extract q)
        (extract qdot))))

(define (constraint q0 q1 F l)
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))

(define ((L-free-constrained m0 m1 l) local)
  (let* ((extract (extract-particle 2))
         (p0 (extract local 0))
         (q_0 (coordinate p0))
         (qdot_0 (velocity p0))

         (p1 (extract local 1))
         (q_1 (coordinate p1))
         (qdot_1 (velocity p1))

         (F (ref (coordinate local) 4)))
    (- (+ (KE-particle m0 qdot_0)
          (KE-particle m1 qdot_1))
       (constraint q_0 q_1 F l))))

(define q-rect
  (up (literal-function 'x_0)
      (literal-function 'y_0)
      (literal-function 'x_1)
      (literal-function 'y_1)
      (literal-function 'F)))


;; This shows the lagrangian itself, which answers part b:


(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f (compose L (Gamma q-rect))))
  (se (f 't)))


;; Here are the lagrange equations, confirming part b.


(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q-rect)))
  (se (f 't)))


;; Part c - make a change of coordinates.


(define ((cm-theta->rect m0 m1) local)
  (let* ((q (coordinate local))
         (x_cm (ref q 0))
         (y_cm (ref q 1))
         (theta (ref q 2))
         (c (ref q 3))
         (F (ref q 4))
         (total-mass (+ m0 m1))
         (m0-distance (* c (/ m1 total-mass)))
         (m1-distance (* c (/ m0 total-mass))))
    (up (- x_cm (* m0-distance (cos theta)))
        (- y_cm (* m0-distance (sin theta)))
        (+ x_cm (* m1-distance (cos theta)))
        (+ y_cm (* m1-distance (sin theta)))
        F)))

(se
 ((F->C (cm-theta->rect 'm_0 'm_1))
  (up 't
      (up 'x_cm 'y_cm 'theta 'c 'F)
      (up 'xdot_cm 'ydot_cm 'thetadot 'cdot 'Fdot))))

(define (L-free-constrained-new m0 m1 l)
  (compose (L-free-constrained m0 m1 l)
           (F->C (cm-theta->rect m0 m1))))


;; This shows the lagrangian itself, after the coordinate transformation:


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained-new 'm_0 'm_1 'l))
       (f (compose L (Gamma q))))
  (se (f 't)))


;; Here are the lagrange equations for part c.


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained-new 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (se (f 't)))


;; For part d, we can substitute the constant value of c to get simplified
;; equations.


(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (L (L-free-constrained-new 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (se (f 't)))
