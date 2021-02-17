;; Section 1.4: Computing Actions
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/sec1_4.cljc :comments org
;; :END:

;; I don't plan on doing this for every section in the book, but section 1.4 is the
;; first place where we're introduced to Scheme, so I followed along and made a few
;; notes.


(ns ch1.ex1-4
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)


;; #+RESULTS:

;; This is the first demo of how any of this stuff works, starting on page 15.
;; Here's our first Lagrangian, super simple.

;; #+name: L-free-particle

(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* (/ 1 2) mass (square v)))))


;; #+RESULTS: L-free-particle
;; : #'ch1.ex1-4/L-free-particle

;; =L-free-particle= is a function that takes some =mass= and returns a /new/
;; function. The new function takes an instance of a "local tuple" and returns the
;; value of the "Lagrangian". This is the function that you query at every point
;; along some evolving path in configuration space. For realizable physical paths,
;; the integral of this function should by minimized, or stationary.

;; Why? That's what we're trying to develop here.

;; Suppose we let $q$ denote a coordinate path function that maps time to position
;; components:


(def q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))


;; #+RESULTS:
;; : #'ch1.ex1-4/q

;; $\Gamma$ is a function that takes a coordinate path and returns a function of
;; time that gives the local tuple.

;; The value $\Gamma$ returns is called the "local tuple":


(->tex-equation
 ((Gamma q) 't))


;; #+RESULTS[fe26f06b665fa857fea5b87c1cab9ddf3a5c0565]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{t} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{x\left(t\right)} \cr \cr \displaystyle{y\left(t\right)} \cr \cr \displaystyle{z\left(t\right)}\end{pmatrix}} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{Dx\left(t\right)} \cr \cr \displaystyle{Dy\left(t\right)} \cr \cr \displaystyle{Dz\left(t\right)}\end{pmatrix}}\end{pmatrix}\n\end{equation}
;; :end:

;; This is just $(t, q(t), (Dq)(t), ....)$ Where $D$ is the derivative. (Preview:
;; can a component of the coordinate path depend on the others? YES, and that would
;; impose constraints beyond the degrees of freedom you'd guess by just counting
;; the coordinates.)

;; Composing the Lagrangian with $\Gamma[q]$ gives you a function that computes the
;; Lagrangian at some instant:


(->tex-equation
 ((compose (L-free-particle 'm) (Gamma q)) 't))


;; #+RESULTS[809182521389a716dc6c5b0cb89c98149cb5a09e]:
;; :results:
;; \begin{equation}\n\frac{1}{2}\,m\,{\left(Dx\left(t\right)\right)}^{2} + \frac{1}{2}\,m\,{\left(Dy\left(t\right)\right)}^{2} + \frac{1}{2}\,m\,{\left(Dz\left(t\right)\right)}^{2}\n\end{equation}
;; :end:

;; This particular formula is written in terms of $x, y, z$ coordinates, but that
;; only came from the definition of $q$. As we'll see later, you could write a
;; coordinate transformation from some other totally different style of coordinates
;; (called "generalized coordinates") and the Lagrangian would look different, but
;; return the same value.

;; This function calculates the action $S[q](t_1, t_2)$:


(ns-unmap *ns* 'Lagrangian-action)

(defn Lagrangian-action [L q t1 t2]
  (definite-integral (compose L (Gamma q)) t1 t2))


;; #+RESULTS:
;; : #'ch1.ex1-4/Lagrangian-action

;; Here's an example path that a particle might take, moving along a straight line
;; as a function of $t$.

;; #+name: test-path

(defn test-path [t]
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))


;; #+RESULTS: test-path
;; : #'ch1.ex1-4/test-path

;; Calculate the action for a particle of mass 3, between $t_1 = 0$ and $t_2 = 10$:


(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
