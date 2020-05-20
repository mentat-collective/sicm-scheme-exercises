;; messing around to make sure I understand what I'm seeing in the coordinate
;; transforms on page 45.

(load "utils.scm")

(define (p->r local)
  (let* ((polar-tuple (coordinate local))
         (r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))

(define (spherical->rect local)
  (let* ((spherical-tuple (coordinate local))
         (r (ref spherical-tuple 0))
         (theta (ref spherical-tuple 1))
         (phi (ref spherical-tuple 2)))
    (up (* r (sin theta) (cos phi))
        (* r (sin theta) (sin phi))
        (* r (cos theta)))))


;; check polar:
(show-expression
 ((F->C p->r)
  (up 't
      (up 'r 'phi)
      (up 'rdot 'phidot))))

;; spherical coordinate change, check velocities:
(show-expression
 ((F->C spherical->rect)
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))

(show-expression
 (square (ref (ref ((F->C spherical->rect)
             (up 't
                 (up 'r 'theta 'phi)
                 (up 'rdot 'thetadot 'phidot))) 2) 0)))

;; get the Langrangian from page 41:
(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

;; BOOM, now we can compose these things!
(define (L-central-polar m U)
  (compose (L-central-rectangular m U) (F->C p->r)))

(define (L-central-spherical m U)
  (compose (L-central-rectangular m U)
           (F->C spherical->rect)))

;; Confirm the polar coordinate version...
(show-expression
 ((L-central-polar 'm (literal-function 'U))
  (up 't
      (up 'r 'phi)
      (up 'rdot 'phidot))))

;; BOOM, much better than calculating by hand!
(show-expression
 ((L-central-spherical 'm (literal-function 'U))
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))


;; rectangular, for fun:
(show-expression
 ((L-central-rectangular 'm (literal-function 'U))
  (up 't
      (up 'x 'y 'z)
      (up 'xdot 'ydot 'zdot))))
