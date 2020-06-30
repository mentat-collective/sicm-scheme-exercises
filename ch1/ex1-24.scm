;; Exercise 1.24: Constraint forces
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-24.scm :comments org
;; :END:

;; This is a special case of a solution we found in exercise 1.22. In that
;; exercise, we found the constraint forces on a driven pendulum. By setting
;; $y_s(t) = l$, we can read off the constraint forces for the undriven pendulum.


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

;; Take some definitions that we need:


(define ((L-driven-free m l y U) local)
  (let* ((extract (extract-particle 2))
         (bob (extract local 0))
         (q (coordinate bob))
         (qdot (velocity bob))
         (F (ref (coordinate local) 2)))
    (- (KE-particle m qdot)
       (U q)
       (U-constraint (up 0 (y (time local)))
                     q
                     F
                     l))))

(define ((U-gravity g m) q)
  (let* ((y (ref q 1)))
    (* m g y)))

(define ((driven-polar->rect y) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (c (ref q 1))
         (F (ref q 2)))
    (up (* c (sin theta))
        (- (y (time local)) (* c (cos theta)))
        F)))

(define (L-driven-pend m l y U)
  (compose (L-driven-free m l y U)
           (F->C (driven-polar->rect y))))


;; #+RESULTS:
;; : #| L-driven-free |#
;; :
;; : #| U-gravity |#
;; :
;; : #| driven-polar->rect |#
;; :
;; : #| L-driven-pend |#

;; The second equation of motion, for the $c$ coordinate, gives us an equation in
;; terms of tension. Substitute in a constant pendulum support position by defining
;; the support position function to be =(lambda (t) 'l)=:


(let* ((q (up (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (y (lambda (t) 'l))
       (L (L-driven-pend 'm 'l y (U-gravity 'g 'm)))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (ref (f 't) 1)))
