;; Section 1.4: Computing Actions
;; :PROPERTIES:
;; :header-args+: :tangle ch1/sec1-4.scm :comments org
;; :END:

;; I don't plan on doing this for every section in the book, but section 1.4 is the
;; first place where we're introduced to Scheme, so I followed along and made a few
;; notes.


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

;; This is the first demo of how any of this stuff works, starting on page 15.
;; Here's our first Lagrangian, super simple.

;; #+name: L-free-particle

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))


;; #+RESULTS: L-free-particle
;; : #| L-free-particle |#

;; =L-free-particle= is a function that takes some =mass= and returns a /new/
;; function. The new function takes an instance of a "local tuple" and returns the
;; value of the "Lagrangian". This is the function that you query at every point
;; along some evolving path in configuration space. For realizable physical paths,
;; the integral of this function should by minimized, or stationary.

;; Why? That's what we're trying to develop here.

;; Suppose we let $q$ denote a coordinate path function that maps time to position
;; components:


(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))


;; #+RESULTS:
;; : #| q |#

;; $\Gamma$ is a function that takes a coordinate path and returns a function of
;; time that gives the local tuple.

;; The value $\Gamma$ returns is called the "local tuple":


(->tex-equation
 ((Gamma q) 't))


;; #+RESULTS[1f4aaac455bf48bd20965b4268009969bd7fd58e]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ x\left( t \right)} \cr \cr \displaystyle{ y\left( t \right)} \cr \cr \displaystyle{ z\left( t \right)}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ Dx\left( t \right)} \cr \cr \displaystyle{ Dy\left( t \right)} \cr \cr \displaystyle{ Dz\left( t \right)}\end{pmatrix}}\end{pmatrix}
;; \end{equation}

;; This is just $(t, q(t), (Dq)(t), ....)$ Where $D$ is the derivative. (Preview:
;; can a component of the coordinate path depend on the others? YES, and that would
;; impose constraints beyond the degrees of freedom you'd guess by just counting
;; the coordinates.)

;; Composing the Langrangian with $\Gamma[q]$ gives you a function that computes the
;; Lagrangian at some instant:


(->tex-equation
 ((compose (L-free-particle 'm) (Gamma q)) 't))


;; #+RESULTS[49b01dd30b3d679e70016f72a5e51c78ecbf6c38]:
;; \begin{equation}
;; {{1}\over {2}} m {\left( Dz\left( t \right) \right)}^{2} + {{1}\over {2}} m {\left( Dy\left( t \right) \right)}^{2} + {{1}\over {2}} m {\left( Dx\left( t \right) \right)}^{2}
;; \end{equation}

;; This particular formula is written in terms of $x, y, z$ coordinates, but that
;; only came from the definition of $q$. As we'll see later, you could write a
;; coordinate transformation from some other totally different style of coordinates
;; (called "generalized coordinates") and the Lagrangian would look different, but
;; return the same value.

;; This function calculates the action $S[q](t_1, t_2)$:


(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))


;; #+RESULTS:
;; : #| Lagrangian-action |#

;; Here's an example path that a particle might take, moving along a straight line
;; as a function of $t$.

;; #+name: test-path

(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))


;; #+RESULTS: test-path
;; : #| test-path |#

;; Calculate the action for a particle of mass 3, between $t_1 = 0$ and $t_2 = 10$:


(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
