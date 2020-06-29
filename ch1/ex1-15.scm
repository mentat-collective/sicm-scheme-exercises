;; Exercise 1.15: Equivalence
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-15.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; Scheme Tools

;; Equation (1.77) in the book describes how to implement $C$ given some arbitrary
;; $F$. Looking ahead slightly, this is implemented as =F->C= on page 46.

;; The following function is a slight redefinition that allows us to use an $F$
;; that takes an explicit $(t, x')$, instead of the entire local tuple:


(define ((F->C* F) local)
  (let ((t (time local))
        (x (coordinate local))
        (v (velocity local)))
    (up t
        (F t x)
        (+ (((partial 0) F) t x)
           (* (((partial 1) F) t x)
              v)))))


;; #+RESULTS[8ea33eda9107b7b0d6ce890f316eb453b2d96fca]:
;; : #| F->C* |#

;; Next we define $F$, $C$ and $L$ as described above, as well as =qprime=, a
;; function that can represent our unprimed coordinate path function.

;; The types here all imply that the path has one real coordinate. I did this to
;; make the types easier to understand; the derivation applies equally well to
;; paths with many dimensions.


(define F
  (literal-function 'F (-> (X Real Real) Real)))

(define C (F->C* F))

(define L
  (literal-function 'L (-> (UP Real Real Real) Real)))

(define qprime
  (literal-function 'qprime))


;; #+RESULTS:
;; : #| F |#
;; :
;; : #| C |#
;; :
;; : #| L |#
;; :
;; : #| qprime |#

;; When we apply $C$ to the primed local tuple, do we get the transformed tuple
;; that we expect from 1.77 in the book?


(->tex-equation
 ((compose C (Gamma qprime)) 't))


;; #+RESULTS[ecef7fe872de385af827c689dbd0db76aa5319f0]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix}
;; \end{equation}

;; This looks correct. We can also transform the path before passing it to
;; $\Gamma$:


(define ((to-q F qp) t)
  (F t (qp t)))


;; #+RESULTS:
;; : #| to-q |#

;; Subtract the two forms to see that they're equivalent:


(->tex-equations
 ((- (compose C (Gamma qprime))
     (Gamma (to-q F qprime)))
  't))


;; #+RESULTS[10aea4a5b833aa00e0a1c63049d844dc37528289]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{pmatrix}
;; \end{equation}

;; Now that we know $C$ is correct we can define $q$, the unprimed coordinate path
;; function, and =Lprime=:


(define q (to-q F qprime))
(define Lprime (compose L C))
;; Derivation

;; Begin by calculating the components of the Lagrange equations in equation
;; \eqref{eq:lagrange-prime}. Examine the $\partial_2L'$ term first.

;; As we discussed above, function composition is associative, so:

;; \begin{equation}
;; \label{eq:c-l}
;; (L \circ C) \circ \Gamma[q'] = L' \circ \Gamma[q'] \implies L' = L \circ C
;; \end{equation}

;; Substituting $L'$ from \eqref{eq:c-l} and using the chain rule:

;; \begin{equation}
;;   \partial_2L' = \partial_2(L \circ C) = ((DL) \circ C) \partial_2 C
;; \end{equation}

;; I found the next step troubling until I became more comfortable with the
;; functional notation.

;; $C$ is a function that transforms a local tuple. It takes 3 arguments (a tuple
;; with 3 elements, technically) and returns 3 arguments. $\partial_2 C$ is an
;; up-tuple with 3 entries. Each entry describes the derivative each component of
;; $C$'s output with respect to the velocity component of the local tuple.

;; $L$ is a function that transforms the 3-element local to a scalar output. $DL$
;; is a down-tuple with 3 entries. Each entry describes the derivative of the
;; single output with respect to each entry of the local tuple.

;; The tuple algebra described in Chapter 9 defines multiplication between an up
;; and down tuple as a dot product, or a "contraction" in the book's language. This
;; means that we can expand out the product above:

;; \begin{equation}
;;   (DL \circ C)\partial_2 C = (\partial_0L \circ C)(I_0 \circ \partial_2 C) + (\partial_1L \circ C)(I_1 \circ \partial_2 C) + (\partial_2L \circ C)(I_2 \circ \partial_2 C)
;; \end{equation}

;; $I_0$, $I_1$ and $I_2$ are "selectors" that return that single element of the
;; local tuple.

;; Example the value of $\partial_2C$ using our Scheme utilities:


(->tex-equation
 (((partial 2) C) (up 't 'xprime 'vprime))
 "eq:p2c")


;; #+RESULTS[2ad70f16eca982284368a2f70f31de3b2de41025]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ {\partial}_{1}F\left( t, {x}^\prime \right)}\end{pmatrix}
;; \label{eq:p2c}
;; \end{equation}

;; The first two components are 0, leaving us with:

;; \begin{equation}
;;   \partial_2 L' = (DL \circ C)\partial_2 C = (\partial_2L \circ C)(I_2 \circ \partial_2 C)
;; \end{equation}

;; Compose this quantity with $\Gamma[q']$ and distribute the composition into the
;; product. Remember that $C \circ \Gamma[q'] = \Gamma[q]$:

;; \begin{equation}
;;   \begin{aligned}
;;     \partial_2L' \circ \Gamma[q'] & = (\partial_2L \circ C)(I_2 \circ \partial_2 C) \circ \Gamma[q'] \cr
;;     & = (\partial_2L \circ C \circ \Gamma[q'])(I_2 \circ \partial_2 C \circ \Gamma[q']) \cr
;;     & = (\partial_2L \circ \Gamma[q])(I_2 \circ \partial_2 C \circ \Gamma[q'])
;;   \end{aligned}
;; \end{equation}

;; Take the derivative (with respect to time, remember, from the types):

;; \begin{equation}
;;   D(\partial_2L' \circ \Gamma[q']) = D\left[(\partial_2L \circ \Gamma[q])(I_2 \circ \partial_2 C \circ \Gamma[q'])\right]
;; \end{equation}

;; Substitute the second term using \eqref{eq:p2c}:

;; \begin{equation}
;;   D(\partial_2L' \circ \Gamma[q']) = D\left[(\partial_2L \circ \Gamma[q])\partial_1F(t, q'(t))\right]
;; \end{equation}

;; Expand using the product rule:

;; \begin{equation}
;; \label{eq:ex1-15-dp2l}
;;   D(\partial_2L' \circ \Gamma[q']) = \left[ D(\partial_2L \circ \Gamma[q]) \right]\partial_1F(t, q'(t)) + (\partial_2L \circ \Gamma[q])D\left[ \partial_1F(t, q'(t)) \right]
;; \end{equation}

;; A term from the unprimed Lagrange's equation is peeking. Notice this, but don't
;; make the substitution just yet.

;; Next, expand the $\partial_1 L'$ term:

;; \begin{equation}
;;   \partial_1L' = \partial_1(L \circ C) = ((DL) \circ C) \partial_1 C
;; \end{equation}

;; Calculate $\partial_1C$ using our Scheme utilities:


(->tex-equation
 (((partial 1) C) (up 't 'xprime 'vprime)))
;; Scheme Derivation

;; Can we use Scheme to pursue the same derivation? If we can write the
;; relationships of the derivation in code, then we'll have a sort of computerized
;; proof that the primed Lagrange equations are valid.

;; First, consider $\partial_1 L' \circ \Gamma[q']$:


(->tex-equation
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))


;; #+RESULTS[5eac70f3edfcf5feeb3a2eb9b98d89646f21ade3]:
;; \begin{equation}
;; D{q}^\prime\left( t \right) {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {{\partial}_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) + {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( {\partial}_{0} {\partial}_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; This is completely insane, and already unhelpful. The argument to $L$, we know,
;; is actually $\Gamma[q]$. Make a function that will replace the tuple with that
;; reference:


(define (->eq expr)
  (write-string
   (replace-all (->tex-equation* expr)
                (->tex* ((Gamma q) 't))
                "\\circ \\Gamma[q]")))


;; #+RESULTS:
;; : #| ->eq |#

;; Try again:


(->eq
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))


;; #+RESULTS[ee6c612cadd91730dd0142a4dd7476fda80d6e07]:
;; \begin{equation}
;; D{q}^\prime\left( t \right) {\partial}_{2}L\left( \circ \Gamma[q] \right) {{\partial}_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}L\left( \circ \Gamma[q] \right) + {\partial}_{2}L\left( \circ \Gamma[q] \right) \left( {\partial}_{0} {\partial}_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; Ignore the parentheses around $\circ \Gamma[q]$ and this looks better.

;; The $\partial_1 L \circ \Gamma[q]$ term of the unprimed Lagrange equations is
;; nestled inside the expansion above, multiplied by a factor $\partial_1F(t,
;; q'(t))$:


(let* ((factor (((partial 1) F) 't (qprime 't))))
  (->eq
   ((* factor (compose ((partial 1) L) (Gamma q)))
    't)))


;; #+RESULTS[31b49f8349da58b715e99a7cbcdf983dc4e12d1e]:
;; \begin{equation}
;; {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}L\left( \circ \Gamma[q] \right)
;; \end{equation}

;; Next, consider the $D(\partial_2 L' \circ \Gamma[q'])$ term:


(->eq
 ((D (compose ((partial 2) Lprime) (Gamma qprime)))
  't))


;; #+RESULTS[9813f05b0fc4d5e7ad9ce035bca6e494cb087d84]:
;; \begin{equation}
;; {\left( D{q}^\prime\left( t \right) \right)}^{2} {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {{\partial}_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + D{q}^\prime\left( t \right) {\left( {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} \left( {\partial}_{1} {\partial}_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + 2 D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}_{0} {\partial}_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\left( {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} {D}^{2}{q}^\prime\left( t \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}_{1} {\partial}_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + D{q}^\prime\left( t \right) {\partial}_{2}L\left( \circ \Gamma[q] \right) {{\partial}_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) {{\partial}_{0}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}_{0} {\partial}_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}_{2}L\left( \circ \Gamma[q] \right) \left( {\partial}_{0} {\partial}_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}


;; This, again, is total madness. We really want some way to control how Scheme
;; expands terms.

;; But we know what we're looking for. Expand out the matching term of the unprimed
;; Lagrange equations:


(->eq
 ((D (compose ((partial 2) L) (Gamma q)))
  't))


;; #+RESULTS[9937f7c80dcdc8744c8f2a1a57505172c55bee17]:
;; \begin{equation}
;; {\left( D{q}^\prime\left( t \right) \right)}^{2} {{\partial}_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}_{1} {\partial}_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + 2 D{q}^\prime\left( t \right) \left( {\partial}_{0} {\partial}_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {D}^{2}{q}^\prime\left( t \right) {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}_{1} {\partial}_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + {{\partial}_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) {{\partial}_{0}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( {\partial}_{0} {\partial}_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right)
;; \end{equation}

;; Staring at these two equations, it becomes clear that the first contains the
;; second, multiplied by $\partial_1F(t, q'(t))$, the same factor that appeared in
;; the expansion of the $\partial_1 L \circ \Gamma[q]$ term.

;; Try writing out the primed Lagrange equations, and subtracting the unprimed
;; Lagrange equations, scaled by this factor:


(let* ((primed-lagrange
        (- (D (compose ((partial 2) Lprime) (Gamma qprime)))
           (compose ((partial 1) Lprime) (Gamma qprime))))

       (lagrange
        (- (D (compose ((partial 2) L) (Gamma q)))
           (compose ((partial 1) L) (Gamma q))))

       (factor
        (compose coordinate ((partial 1) C) (Gamma qprime))))
  (->tex-equation
   ((- primed-lagrange (* factor lagrange))
    't)))
