;; Galilean Invariance

(load "utils.scm")

;; I'll do this for a single particle, since it's annoying to get the sum going
;; for many; and the lagrangian is additive, so no problem.

(define (uniform-translate-shift->rect local)
  (let* ((t (time local))
         (q (coordinate local))
         (xprime (ref q 0))
         (delta_x (ref q 1))
         (delta_v (ref q 2)))
    (+ xprime delta_x (* t delta_v))))

(define (L-translate-shift m)
  (compose (L-free-particle m)
           (F->C uniform-translate-shift->rect)))

;; First, confirm that if we have a constant, we get what we expected from paper.
(let* ((q (up (literal-function 'xprime)
              (lambda (t) 'Delta_x)
              (lambda (t) 'Delta_v)))
       (f (compose (L-translate-shift 'm) (Gamma q))))
  (se (f 't)))


;; We can change this a little to see the extra terms; substract off the free
;; particle lagrangian, to see the extra stuff.
(let* ((q (up (literal-function 'xprime)
              (lambda (t) 'Delta_x)
              (lambda (t) 'Delta_v)))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (compose L (Gamma q))))
  (se (f 't)))


;; Here's the gnarly version with both entries as actual functions. Can this be
;; a total time derivative? It CANNOT be, because we have a (D Delta_v(t))^2
;; term in there, and we know that total time derivatives have to be linear in
;; the velocities. The function F would have had to have a velocity in it, which
;; is not allowed.
(let* ((q (up (literal-function 'xprime)
              (literal-function 'Delta_x)
              (literal-function 'Delta_v)))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (compose L (Gamma q))))
  (se (f 't)))

;; Let's simplify by making the delta v constant and see if there's anything so
;; obvious about Delta_x.
;;
;; We know that we have a total derivative when 'Delta_x is constant, and we
;; know that total time derivatives are linear, so let's substract off the total
;; time derivative and see what happens:
(let* ((q (lambda (dx)
            (up (literal-function 'xprime)
                dx
                (lambda (t) 'Delta_v))))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (lambda (dx)
            (compose L (Gamma (q dx))))))
  (se ((- (f (literal-function 'Delta_x))
          (f (lambda (t) 'Delta_x)))
       't)))


;; Take a look... there is a quadratic velocity term in here! We have D(Delta_x)
;; * D(x_prime). This is not allowed in a total time derivative.
;;
;; SO, only if the shift and uniform translation are constant do we not affect
;; the Lagrangian value.
