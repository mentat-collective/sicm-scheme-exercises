;; Exercise 9.1 Chain Rule
;;      :PROPERTIES:
;;      :header-args+: :tangle src/ch9/ex9-1.cljc :comments org
;;      :END:

;; You're supposed to do these by hand, so I'll do that in the textbook. But here,
;; let's redo them on the machine.


(ns ch1.ex9-1
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)

;; Compute $\partial_0 F(x, y)$ and $\partial_1 F(x, y)$

;; First, let's define the functions we need.


(defn F [x y]
  (* (square x)
     (cube y)))

(defn G [x y]
  (up (F x y) y))

(defn H [x y]
  (F (F x y) y))



;; #+RESULTS:
;; | #'ch1.ex9-1/F |
;; | #'ch1.ex9-1/G |
;; | #'ch1.ex9-1/H |

;;  You can do this with explicit partials:


(let [f (down ((partial 0) F) ((partial 1) F))]
  (->tex-equation
   (f 'x 'y)))



;; #+RESULTS[001293adfac6814bb5fa4a4ab4ceb7075b07b1ea]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{2\,x\,{y}^{3}} \cr \cr \displaystyle{3\,{x}^{2}\,{y}^{2}}\end{bmatrix}\n\end{equation}
;; :end:

;; Or with the $D$ symbol:


(->tex-equation
 ((D F) 'x 'y))



;; #+RESULTS[8e59ef099a4c489230981a00528990c427fb1b3b]:
;; :results:
;; \begin{equation}\n\begin{bmatrix}\displaystyle{2\,x\,{y}^{3}} \cr \cr \displaystyle{3\,{x}^{2}\,{y}^{2}}\end{bmatrix}\n\end{equation}
;; :end:

;; Or, we could show that they're equivalent this way:


(let [f (down ((partial 0) F) ((partial 1) F))]
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
