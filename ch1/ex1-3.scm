;; Exercise 1.3: Fermat optics
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-3.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; Calculus

;; We can also solve this with calculus. Because the beam doesn't change media, its
;; speed $v$ stays constant, so minimizing the total distance $d$ is equivalent to
;; minimizing the time $t = {d \over v}$.

;; Set $x_1 = 0$ for convenience, and write the total distance the light travels as
;; a function of $x_p$:

;; \begin{equation}
;; d(x_p) = \sqrt{y_1^2 + x_p^2} + \sqrt{(x_2 - x_p)^2 + y_2^2}
;; \end{equation}

;; For practice, we can also define this function in Scheme.


(define ((total-distance x1 y1 x2 y2) xp)
  (+ (sqrt (+ (square (+ x1 xp))
              (square y1)))
     (sqrt (+ (square (- x2 (+ x1 xp)))
              (square y2)))))


;; #+RESULTS:
;; : #| total-distance |#

;; Here's the function again, generated from code, with general $t_1$:


(->tex-equation
 ((total-distance 'x_1 'y_1 'x_2 'y_2) 'x_p))


;; #+RESULTS[084acf42d4fe771c97db9cf39e92c75383662d30]:
;; \begin{equation}
;; \sqrt{{{x}_{1}}^{2} + 2 {x}_{1} {x}_{p} + {{x}_{p}}^{2} + {{y}_{1}}^{2}} + \sqrt{{{x}_{1}}^{2} - 2 {x}_{1} {x}_{2} + 2 {x}_{1} {x}_{p} + {{x}_{2}}^{2} - 2 {x}_{2} {x}_{p} + {{x}_{p}}^{2} + {{y}_{2}}^{2}}
;; \end{equation}

;; To find the $x_p$ that minimizes the total distance,

;; - take the derivative with respect to $x_p$,
;; - set it equal to 0 and
;; - solve for $x_p$.

;; The derivative will look cleaner in code if we keep the components of the sum
;; separate and prevent Scheme from "simplifying". Redefine the function to return
;; a tuple:


(define ((total-distance* x1 y1 x2 y2) xp)
  (up (sqrt (+ (square (+ x1 xp))
               (square y1)))
      (sqrt (+ (square (- x2 (+ x1 xp)))
               (square y2)))))


;; #+RESULTS:
;; : #| total-distance* |#

;; Here are the sum components:


(->tex-equation
 ((total-distance* 0 'y_1 'x_2 'y_2) 'x_p))


;; #+RESULTS[8080e49ee342b7a2a69c9c84337c37bc473a3c58]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ \sqrt{{{x}_{p}}^{2} + {{y}_{1}}^{2}}} \cr \cr \displaystyle{ \sqrt{{{x}_{2}}^{2} - 2 {x}_{2} {x}_{p} + {{x}_{p}}^{2} + {{y}_{2}}^{2}}}\end{pmatrix}
;; \end{equation}

;; Taking a derivative is easy with =scmutils=. Just wrap the function in =D=:


(let* ((distance-fn (total-distance* 0 'y_1 'x_2 'y_2))
       (derivative (D distance-fn)))
  (->tex-equation
   (derivative 'x_p)))


;; #+RESULTS[5bbf36ca4a362ee2f2d2423071a6f818c8c93cab]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ {{{x}_{p}}\over {\sqrt{{{x}_{p}}^{2} + {{y}_{1}}^{2}}}}} \cr \cr \displaystyle{ {{ - {x}_{2} + {x}_{p}}\over {\sqrt{{{x}_{2}}^{2} - 2 {x}_{2} {x}_{p} + {{x}_{p}}^{2} + {{y}_{2}}^{2}}}}}\end{pmatrix}
;; \end{equation}

;; The first component is the base of base $x_p$ of the left triangle over the
;; total length. This ratio is equal to $\cos \theta_1$:

;; #+DOWNLOADED: screenshot @ 2020-06-10 10:36:53
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png]]

;; The bottom component is $-\cos \theta_2$, or ${- (x_2 - x_p)}$ over the length
;; of the right segment. Add these terms together, set them equal to 0 and
;; rearrange:

;; \begin{equation}
;; \label{eq:reflect-laws}
;; \cos \theta_1 = \cos \theta_2 \implies \theta_1 = \theta_2
;; \end{equation}

;; This description in terms of the two incident angles isn't so obvious from the
;; Scheme code. Still, you can use Scheme to check this result.

;; If the two angles are equal, then the left and right triangles are similar, and
;; the ratio of each base to height is equal:

;; \begin{equation}
;; \label{eq:reflect-ratio}
;; {x_p \over y_1} = {{x_2 - x_p} \over y_2}
;; \end{equation}

;; Solve for $x_p$ and rearrange:

;; \begin{equation}
;; \label{eq:reflect-ratio2}
;; x_p = {{y_1 x_2} \over {y_1 + y_2}}
;; \end{equation}

;; Plug this in to the derivative of the original =total-distance= function, and we
;; find that the derivative equals 0, as expected:


(let* ((distance-fn (total-distance 0 'y_1 'x_2 'y_2))
       (derivative (D distance-fn)))
  (->tex-equation
   (derivative (/ (* 'y_1 'x_2) (+ 'y_1 'y_2)))))
