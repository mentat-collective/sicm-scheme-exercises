;; Exercise 1.3: Fermat optics
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-3.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; Law of Reflection

;; The [[https://en.wikipedia.org/wiki/Reflection_(physics)#Laws_of_reflection][law of reflection]] is described in the footnote:

;; #+begin_quote
;; For reflection the angle of incidence is equal to the angle of reflection.
;; #+end_quote

;; We have to show that if we consider all possible paths from a given starting
;; point to a given endpoint, the path of minimum time will give us the law of
;; reflection.

;; Here's the setup:

;; #+DOWNLOADED: screenshot @ 2020-06-10 10:31:24
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_10-31-24_screenshot.png]]

;; If we force the light to bounce off of a mirror, then we have to figure out
;; where it will hit, where $x_p$ is, to minimize the time between the start and
;; end points.

;; There are two ways to solve this problem. The first is to remember this fact
;; from the problem definition:

;; #+begin_quote
;; Light travels in a straight line in any particular medium with a velocity that
;; depends upon the medium.
;; #+end_quote

;; There's no medium change, so if there were no mirror in its path, the light bean
;; would continue in a straight line. Instead of figuring out what the beam will do
;; when it hits the mirror, reflect the endpoint across the mirror and draw a
;; straight line between the start and "end":

;; #+DOWNLOADED: screenshot @ 2020-06-10 10:36:53
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png]]

;; The angle that the beam makes with the plane of the mirror is the same on both
;; sides of the mirror.

;; Now reflect the the "end" point and the segment of the beam that's crossed the
;; mirror back up across the mirror, and you'll find that $\theta_1 = \theta_2$.
;; (Pardon my not-to-scale drawing.)

;; We can also solve this with calculus. Because we don't change media, the speed
;; of light $c$ is constant, so minimizing the total distance is equivalent to
;; minimizing the time.

;; Set $x_1 = 0$ for convenience, and write the total distance the light travels as
;; a function of $x_p$:

;; \begin{equation}
;; \label{eq:reflectfn}
;; f(x_p) = {\sqrt{y_1^2 + x_p^2} + \sqrt{(x_2 - x_p)^2 + y_2^2}
;; \end{equation}

;; We can also define this function in Scheme.


(define ((total-distance x1 y1 x2 y2) xp)
  (+ (sqrt (+ (square (+ x1 xp))
              (square y1)))
     (sqrt (+ (square (- x2 (+ x1 xp)))
              (square y2)))))


;; #+RESULTS:
;; : #| total-distance |#

;; Here's the function again, with general $t_1$:


(->tex-equation
 ((total-distance 'x_1 'y_1 'x_2 'y_2) 'x_p))


;; #+RESULTS[084acf42d4fe771c97db9cf39e92c75383662d30]:
;; \begin{equation}
;; \sqrt{{{x}_{1}}^{2} + 2 {x}_{1} {x}_{p} + {{x}_{p}}^{2} + {{y}_{1}}^{2}} + \sqrt{{{x}_{1}}^{2} - 2 {x}_{1} {x}_{2} + 2 {x}_{1} {x}_{p} + {{x}_{2}}^{2} - 2 {x}_{2} {x}_{p} + {{x}_{p}}^{2} + {{y}_{2}}^{2}}
;; \end{equation}

;; Now we need to take the derivative with respect to $x_p$, set it equal to 0 and
;; solve for $x_p$. That will give us the stationary point of the total distance.

;; The derivative will look cleaner if we keep the components of the sum separate.
;; Redefine the function to return a tuple:


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

;; The structure of the problem makes the next step a little annoying. If you look
;; at the first component, you might recognize the $\cos \theta_1$, the base $x_p$
;; of the left triangle over the total length:

;; #+DOWNLOADED: screenshot @ 2020-06-10 10:36:53
;; #+attr_org: :width 400px
;; #+attr_html: :width 80% :align center
;; #+attr_latex: :width 8cm
;; [[file:images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png]]

;; The bottom component is $-\cos \theta_2$, or ${- (x_2 - x_p)}$ over the length
;; of the right segment. Add these together and set them equal to 0, and you find
;; that

;; \begin{equation}
;; \label{eq:reflect-laws}
;; \cos \theta_1 = \cos \theta_2 \implies \theta_1 = \theta_2
;; \end{equation}

;; This isn't so obvious from the scheme code, but you can use Scheme to check this
;; result. If the two angles are equal, then the left and right triangles are
;; similar, and the ratio of the base to height is equal:

;; \begin{equation}
;; \label{eq:reflect-ratio}
;; {x_p \over y_1} = {{x_2 - x_p} \over y_2}
;; \end{equation}

;; Solve for $x_p$ and rearrange:

;; \begin{equation}
;; \label{eq:reflect-ratio2}
;; x_p = {{y_1 x_2} \over {y_1 + y_2}}
;; \end{equation}

;; Plug this in to the derivative of the original =total-distance= function, and
;; we get that the derivative equals 0, as expected:


(let* ((distance-fn (total-distance 0 'y_1 'x_2 'y_2))
       (derivative (D distance-fn)))
  (->tex-equation
   (derivative (/ (* 'y_1 'x_2) (+ 'y_1 'y_2)))))
