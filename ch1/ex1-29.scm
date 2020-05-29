;; Exercise 1.29: Galilean Invariance
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-29.scm :comments org
;; :END:


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

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


;; #+RESULTS:
;; : #| uniform-translate-shift->rect |#
;; :
;; : #| L-translate-shift |#

;; First, confirm that if we have a constant, we get what we expected from paper.


(let* ((q (up (literal-function 'xprime)
              (lambda (t) 'Delta_x)
              (lambda (t) 'Delta_v)))
       (f (compose (L-translate-shift 'm) (Gamma q))))
  (->tex-equation (f 't)))


;; #+RESULTS[5d2b4de08cfab4779bf7cdab31d518191b40a4d2]:
;; \[\begin{equation}
;; {{1}\over {2}} {{\Delta}_{v}}^{2} m + {\Delta}_{v} m D{x}^\prime\left( t \right) + {{1}\over {2}} m {\left( D{x}^\prime\left( t \right) \right)}^{2}
;; \end{equation}\]

;; We can change this a little to see the extra terms; substract off the free
;; particle lagrangian, to see the extra stuff.


(let* ((q (up (literal-function 'xprime)
              (lambda (t) 'Delta_x)
              (lambda (t) 'Delta_v)))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (compose L (Gamma q))))
  (->tex-equation (f 't)))


;; #+RESULTS[c17004e61fec7edb3835203cdc99c562940bee7c]:
;; \[\begin{equation}
;; {{1}\over {2}} {{\Delta}_{v}}^{2} m + {\Delta}_{v} m D{x}^\prime\left( t \right)
;; \end{equation}\]

;; Here's the gnarly version with both entries as actual functions. Can this be a
;; total time derivative? It CANNOT be, because we have a $(D \Delta_v(t))^2$ term
;; in there, and we know that total time derivatives have to be linear in the
;; velocities. The function $F$ would have had to have a velocity in it, which is
;; not allowed.


(let* ((q (up (literal-function 'xprime)
              (literal-function 'Delta_x)
              (literal-function 'Delta_v)))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (compose L (Gamma q))))
  (->tex-equation (f 't)))


;; #+RESULTS[ded4f6dec25954c9b7536153e1db8db0315cb399]:
;; \[ \begin{equation}
;; {{1}\over {2}} m {t}^{2} {\left( D{\Delta}_{v}\left( t \right) \right)}^{2} + m t D{x}^\prime\left( t \right) D{\Delta}_{v}\left( t \right) + m t D{\Delta}_{v}\left( t \right) {\Delta}_{v}\left( t \right) + m t D{\Delta}_{v}\left( t \right) D{\Delta}_{x}\left( t \right) + m D{x}^\prime\left( t \right) {\Delta}_{v}\left( t \right) + m D{x}^\prime\left( t \right) D{\Delta}_{x}\left( t \right) - {{1}\over {2}} m {\left( D{\Delta}_{v}\left( t \right) \right)}^{2} + {{1}\over {2}} m {\left( {\Delta}_{v}\left( t \right) \right)}^{2} + m {\Delta}_{v}\left( t \right) D{\Delta}_{x}\left( t \right)
;; \end{equation} \]

;; Let's simplify by making the $\Delta_v$ constant and see if there's anything so
;; obvious about $\Delta_x$.

;; We know that we have a total derivative when $\Delta_x$ is constant, and we know
;; that total time derivatives are linear, so let's substract off the total time
;; derivative and see what happens:


(let* ((q (lambda (dx)
            (up (literal-function 'xprime)
                dx
                (lambda (t) 'Delta_v))))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (lambda (dx)
            (compose L (Gamma (q dx))))))
  (->tex-equation
   ((- (f (literal-function 'Delta_x))
       (f (lambda (t) 'Delta_x)))
    't)))
