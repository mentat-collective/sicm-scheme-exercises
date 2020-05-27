;; Exercise 9.1 Chain Rule
;;      :PROPERTIES:
;;      :header-args+: :tangle ch9/ex9-1.scm :comments org
;;      :END:

;; You're supposed to do these by hand, so I'll do that in the textbook. But here,
;; let's redo them on the machine.

(load "ch1/utils.scm")

;; Compute $\partial_0 F(x, y)$ and $\partial_1 F(x, y)$

;; First, let's define the functions we need.


(define (F x y)
  (* (square x)
     (cube y)))

(define (G x y)
  (up (F x y) y))

(define (H x y)
  (F (F x y) y))



;; #+RESULTS:
;; : #| F |#
;; :
;; : #| G |#
;; :
;; : #| H |#

;;  You can do this with explicit partials:


(let ((f (down ((partial 0) F) ((partial 1) F))))
  (->tex-equation
   (f 'x 'y)))



;; #+RESULTS[be9a90487e3a5e53f5a9c479dd79dbd76bd0e18c]:
;; \begin{equation}
;; \left[ \matrix{ \displaystyle{ 2 x {y}^{3}} \cr \cr \displaystyle{ 3 {x}^{2} {y}^{2}}} \right]
;; \end{equation}

;; Or with the $D$ symbol:


(->tex-equation
 ((D F) 'x 'y))



;; #+RESULTS[ad5e3457e7a32bea193ba2301f548b99b871656c]:
;; \begin{equation}
;; \left[ \matrix{ \displaystyle{ 2 x {y}^{3}} \cr \cr \displaystyle{ 3 {x}^{2} {y}^{2}}} \right]
;; \end{equation}

;; Or, we could show that they're equivalent this way:


(let ((f (down ((partial 0) F) ((partial 1) F))))
  (->tex-equation
   (- ((D F) 'x 'y)
      (f 'x 'y))))

;; Compute $\partial_0 F(F(x, y), y)$ and $\partial_1 F(F(x, y), y)$

;; $H$ is already that composition, so:


(->tex-equation
 ((D H) 'x 'y))

;; Compute $\partial_0 G(x, y)$ and $\partial_1 G(x, y)$


(->tex-equation
 ((D G) 'x 'y))

;; Compute $DF(a, b)$, $DG(3, 5)$ and $DH(3a^2, 5b^3)$


(->tex-equation
 (up ((D F) 'a 'b)
     ((D G) 3 5)
     ((D H) (* 3 (square 'a)) (* 5 (cube 'b)))))
