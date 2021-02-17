;; Exercise 1.22: Driven pendulum
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_22.cljc :comments org
;; :END:


(ns ch1.ex1-22
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
;; Part B: Lagrangian

;; Now write the Lagrangian for the driven pendulum in rectangular coordinates. The
;; constraint force takes the same shape as in exercise 1.21:


(defn U-constraint [q0 q1 F l]
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))


;; #+RESULTS:
;; : #'ch1.ex1-22/U-constraint

;; The Lagrangian is similar, but only involves a single particle -- the pendulum
;; bob. We can generate the constraint force by directly building the support's
;; coordinates, rather than extracting them from the local tuple.

;; #+name: L-driven-free

(defn extract-particle [pieces]
  (fn [[t q v] i]
    (let [indices (take pieces
                        (iterate
                         inc (* i pieces)))
          extract (fn [tuple]
                    (mapv (fn [i]
                            (ref tuple i))
                          indices))]
      (up t (extract q) (extract v)))))

(defn KE-particle [m v]
  (* (/ 1 2) m (square v)))

(defn L-driven-free [m l y U]
  (fn [local]
    (let [extract (extract-particle 2)
          [_ q qdot] (extract local 0)
          F (ref (coordinate local) 2)]
      (- (KE-particle m qdot)
         (U q)
         (U-constraint (up 0 (y (state->t local)))
                       q
                       F
                       l)))))


;; #+RESULTS: L-driven-free
;; | #'ch1.ex1-22/extract-particle |
;; | #'ch1.ex1-22/KE-particle      |
;; | #'ch1.ex1-22/L-driven-free    |

;; Here is the now-familiar equation for a uniform gravitational potential, acting
;; on the $y$ coordinate:

;; #+name: U-gravity

(defn U-gravity [g m]
  (fn [[_ y]]
    (* m g y)))


;; #+RESULTS: U-gravity
;; : #'ch1.ex1-22/U-gravity

;; Now use the new Lagrangian to generate equations of motion for the three
;; coordinates $x$, $y$ and $F$:


(let [q (up (literal-function 'x)
            (literal-function 'y)
            (literal-function 'F))
      U (U-gravity 'g 'm)
      y (literal-function 'y_s)
      L (L-driven-free 'm 'l y U)
      f ((Lagrange-equations L) q)]
  (->tex-equation
   (f 't)))


;; #+RESULTS[2cd131df0455b4e3fb3acb962dea15a93d575c8b]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{\frac{l\,m\,{D}^{2}x\left(t\right) + F\left(t\right)\,x\left(t\right)}{l}} \cr \cr \displaystyle{\frac{g\,l\,m + l\,m\,{D}^{2}y\left(t\right) + F\left(t\right)\,y\left(t\right) - F\left(t\right)\,y_s\left(t\right)}{l}} \cr \cr \displaystyle{\frac{- {l}^{2} + {\left(x\left(t\right)\right)}^{2} + {\left(y\left(t\right)\right)}^{2} -2\,y\left(t\right)\,y_s\left(t\right) + {\left(y_s\left(t\right)\right)}^{2}}{2\,l}}\end{bmatrix}\n\end{equation}
;; :end:

;; The first two equations of motion match the equations we derived in part A,
;; using Newton's equations. The third states that

;; \begin{equation}
;; l^2 = x(t)^2 + (y_s(t) - y(t))^&2
;; \end{equation}

;; Verified, with some extra terms to force the simplification:


(let [q (up (literal-function 'x)
            (literal-function 'y)
            (literal-function 'F))
      U (U-gravity 'g 'm)
      y (literal-function 'y_s)
      L (L-driven-free 'm 'l y U)
      f ((Lagrange-equations L) q)
      eq (ref (f 't) 2)]
  (->tex-equation
   (- eq
      (/ (* (/ 1 2)
            (- (+ (square ((literal-function 'x) 't))
                  (square ((- y (literal-function 'y)) 't)))
               (square 'l)))
         'l))))
;; Part C: Coordinate Change

;; Now we want to verify that we get the same Lagrangian and equations of motion as
;; in 1.88 from the book. We also want to analyze the constraint forces. To do this
;; we need to introduce a coordinate change.

;; To analyze the constraint forces, we have to do the same trick as in exercise
;; 1.21 and use a coordinate $c(t) = l$. The new coordinates are $(\theta, c, F)$:

;; #+name: driven-polar->rect

(defn driven-polar->rect [y]
  (fn [[t [theta c F]]]
    (up (* c (sin theta))
        (- (y t) (* c (cos theta)))
        F)))


;; #+RESULTS: driven-polar->rect
;; : #'ch1.ex1-22/driven-polar->rect

;; Compose the coordinate change with the rectangular Lagrangian:

;; #+name: L-driven-pend

(defn L-driven-pend [m l y U]
  (compose (L-driven-free m l y U)
           (F->C (driven-polar->rect y))))


;; #+RESULTS: L-driven-pend
;; : #'ch1.ex1-22/L-driven-pend

;; Examine the Lagrangian itself, after the coordinate transformation. (Notice that
;; we're using a constant function for $c(t)$ that always returns $l$.)


(let [q (up (literal-function 'theta)
            (fn [_] 'l)
            (literal-function 'F))
      y (literal-function 'y_s)
      L (L-driven-pend 'm 'l y (U-gravity 'g 'm))
      f (compose L (Gamma q))]
  (->tex-equation
   (f 't)))


;; #+RESULTS[25db361716cdd35170096bc46f904682cd966e63]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,{l}^{2}\,m\,{\left(D\theta\left(t\right)\right)}^{2} + l\,m\,D\theta\left(t\right)\,\sin\left(\theta\left(t\right)\right)\,Dy_s\left(t\right) + g\,l\,m\,\cos\left(\theta\left(t\right)\right) - g\,m\,y_s\left(t\right) + \frac{1}{2}\,m\,{\left(Dy_s\left(t\right)\right)}^{2}\n\end{equation}
;; :end:

;; Looks just like equation 1.88.

;; Next, examine the Lagrange equations, using the same substitution of $c(t) = l$:


(let [q (up (literal-function 'theta)
            (fn [_] 'l)
            (literal-function 'F))
      y (literal-function 'y_s)
      L (L-driven-pend 'm 'l y (U-gravity 'g 'm))
      f ((Lagrange-equations L) q)]
  (->tex-equation
   (f 't)))
