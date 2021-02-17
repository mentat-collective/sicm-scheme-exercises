;; Exercise 1.15: Equivalence
;; :PROPERTIES:
;; :header-args+: :tangle src/ch1/ex1_15.cljc :comments org
;; :END:


(ns ch1.ex1-15
  (:refer-clojure :exclude [+ - * / compare zero? ref partial])
  (:require [clojure.string :as string]
            [sicmutils.env :as e #?@(:cljs [:include-macros true])]))

(e/bootstrap-repl!)
;; Scheme Tools

;; Equation (1.77) in the book describes how to implement $C$ given some arbitrary
;; $F$. Looking ahead slightly, this is implemented as =F->C= on page 46.

;; The following function is a slight redefinition that allows us to use an $F$
;; that takes an explicit $(t, x')$, instead of the entire local tuple:


(defn F->C* [F]
  (fn [[t x v]]
    (up t
        (F t x)
        (+ (((partial 0) F) t x)
           (* (((partial 1) F) t x)
              v)))))


;; #+RESULTS[8ea33eda9107b7b0d6ce890f316eb453b2d96fca]:
;; : #'ch1.ex1-15/F->C*

;; Next we define $F$, $C$ and $L$ as described above, as well as =qprime=, a
;; function that can represent our unprimed coordinate path function.

;; The types here all imply that the path has one real coordinate. I did this to
;; make the types easier to understand; the derivation applies equally well to
;; paths with many dimensions.


(def F
  (literal-function 'F (-> (X Real Real) Real)))

(def C (F->C* F))

(def L
  (literal-function 'L (-> (UP Real Real Real) Real)))

(def qprime
  (literal-function 'qprime))


;; #+RESULTS:
;; | #'ch1.ex1-15/F      |
;; | #'ch1.ex1-15/C      |
;; | #'ch1.ex1-15/L      |
;; | #'ch1.ex1-15/qprime |

;; When we apply $C$ to the primed local tuple, do we get the transformed tuple
;; that we expect from 1.77 in the book?


(->tex-equation
 ((compose C (Gamma qprime)) 't))


;; #+RESULTS[2b72ef2c7d9041ccdec7885dcc252c5362969303]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{t} \cr \cr \displaystyle{F\left(t, q^\prime\left(t\right)\right)} \cr \cr \displaystyle{\partial_1F\left(t, q^\prime\left(t\right)\right)\,Dq^\prime\left(t\right) + \partial_0F\left(t, q^\prime\left(t\right)\right)}\end{pmatrix}\n\end{equation}
;; :end:

;; This looks correct. We can also transform the path before passing it to
;; $\Gamma$:


(defn to-q [F qp]
  (fn [t]
    (F t (qp t))))


;; #+RESULTS:
;; : #'ch1.ex1-15/to-q

;; Subtract the two forms to see that they're equivalent:


(->tex-equation
 ((- (compose C (Gamma qprime))
     (Gamma (to-q F qprime)))
  't))


;; #+RESULTS[2d8902854d6900eaf19d58d1b5ab9fb60bd831b4]:
;; :results:
;; \begin{equation}\n\begin{pmatrix}\displaystyle{0} \cr \cr \displaystyle{0} \cr \cr \displaystyle{0}\end{pmatrix}\n\end{equation}
;; :end:

;; Now that we know $C$ is correct we can define $q$, the unprimed coordinate path
;; function, and =Lprime=:


(def q (to-q F qprime))
(def Lprime (compose L C))
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
 :label "eq:p2c")


;; #+RESULTS[f3ea66e9d3343c80892faaabe92b413b063971f7]:
;; :results:
;; \begin{equation}\n\label{eq:p2c}\n\begin{pmatrix}\displaystyle{0} \cr \cr \displaystyle{0} \cr \cr \displaystyle{\partial_1F\left(t, x^\prime\right)}\end{pmatrix}\n\end{equation}
;; :end:

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
;; \label{eq:ex1_15-dp2l}
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


;; #+RESULTS[a14cf0bec5b2aeb15eec4de504e22659b40491c0]:
;; :results:
;; \begin{equation}\nDq^\prime\left(t\right)\,\partial_2L\left(\begin{pmatrix}\displaystyle{t} \cr \cr \displaystyle{F\left(t, q^\prime\left(t\right)\right)} \cr \cr \displaystyle{\partial_1F\left(t, q^\prime\left(t\right)\right)\,Dq^\prime\left(t\right) + \partial_0F\left(t, q^\prime\left(t\right)\right)}\end{pmatrix}\right)\,{\partial_1}^{2}F\left(t, q^\prime\left(t\right)\right) + \partial_1F\left(t, q^\prime\left(t\right)\right)\,\partial_1L\left(\begin{pmatrix}\displaystyle{t} \cr \cr \displaystyle{F\left(t, q^\prime\left(t\right)\right)} \cr \cr \displaystyle{\partial_1F\left(t, q^\prime\left(t\right)\right)\,Dq^\prime\left(t\right) + \partial_0F\left(t, q^\prime\left(t\right)\right)}\end{pmatrix}\right) + \partial_2L\left(\begin{pmatrix}\displaystyle{t} \cr \cr \displaystyle{F\left(t, q^\prime\left(t\right)\right)} \cr \cr \displaystyle{\partial_1F\left(t, q^\prime\left(t\right)\right)\,Dq^\prime\left(t\right) + \partial_0F\left(t, q^\prime\left(t\right)\right)}\end{pmatrix}\right)\,\left(\partial_0\,\partial_1\right)\left(F\right)\left(t, q^\prime\left(t\right)\right)\n\end{equation}
;; :end:


;; This is completely insane, and already unhelpful. The argument to $L$, we know,
;; is actually $\Gamma[q]$. Make a function that will replace the tuple with that
;; reference:


(defn ->eq [expr]
  (string/replace
   (->tex-equation expr)
   (->TeX (simplify ((Gamma q) 't)))
   "\\Gamma[q]\\left(t\\right)"))


;; #+RESULTS:
;; : #'ch1.ex1-15/->eq

;; Try again:


(->eq
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))


;; #+RESULTS[faf6828253b9f230c0a91ac86f744888da752daf]:
;; :results:
;; \begin{equation}\nDq^\prime\left(t\right)\,\partial_2L\left(\Gamma[q]\left(t\right)\right)\,{\partial_1}^{2}F\left(t, q^\prime\left(t\right)\right) + \partial_1F\left(t, q^\prime\left(t\right)\right)\,\partial_1L\left(\Gamma[q]\left(t\right)\right) + \partial_2L\left(\Gamma[q]\left(t\right)\right)\,\left(\partial_0\,\partial_1\right)\left(F\right)\left(t, q^\prime\left(t\right)\right)\n\end{equation}
;; :end:

;; The $\partial_1 L(\Gamma[q](t))$ term of the unprimed Lagrange equations is
;; nestled inside the expansion above, multiplied by a factor $\partial_1F(t,
;; q'(t))$:


(let [factor (((partial 1) F) 't (qprime 't))]
  (->eq
   ((* factor (compose ((partial 1) L) (Gamma q)))
    't)))


;; #+RESULTS[4d38189f9b821ce2a0fb1b2731fd8eff80329182]:
;; :results:
;; \begin{equation}\n\partial_1F\left(t, q^\prime\left(t\right)\right)\,\partial_1L\left(\Gamma[q]\left(t\right)\right)\n\end{equation}
;; :end:

;; Next, consider the $D(\partial_2 L' \circ \Gamma[q'])$ term:


(->eq
 ((D (compose ((partial 2) Lprime) (Gamma qprime)))
  't))


;; #+RESULTS[3f03581cccc9ece6ad0694aaa2c6404e4445ed97]:
;; :results:
;; \begin{equation}\n\partial_1F\left(t, q^\prime\left(t\right)\right)\,{\left(Dq^\prime\left(t\right)\right)}^{2}\,{\partial_1}^{2}F\left(t, q^\prime\left(t\right)\right)\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right) + {\left(\partial_1F\left(t, q^\prime\left(t\right)\right)\right)}^{2}\,Dq^\prime\left(t\right)\,\left(\partial_1\,\partial_2\right)\left(L\right)\left(\Gamma[q]\left(t\right)\right) + {\left(\partial_1F\left(t, q^\prime\left(t\right)\right)\right)}^{2}\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right)\,{D}^{2}q^\prime\left(t\right) + 2\,\partial_1F\left(t, q^\prime\left(t\right)\right)\,Dq^\prime\left(t\right)\,\left(\partial_0\,\partial_1\right)\left(F\right)\left(t, q^\prime\left(t\right)\right)\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right) + \partial_0F\left(t, q^\prime\left(t\right)\right)\,\partial_1F\left(t, q^\prime\left(t\right)\right)\,\left(\partial_1\,\partial_2\right)\left(L\right)\left(\Gamma[q]\left(t\right)\right) + \partial_1F\left(t, q^\prime\left(t\right)\right)\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right)\,{\partial_0}^{2}F\left(t, q^\prime\left(t\right)\right) + Dq^\prime\left(t\right)\,\partial_2L\left(\Gamma[q]\left(t\right)\right)\,{\partial_1}^{2}F\left(t, q^\prime\left(t\right)\right) + \partial_1F\left(t, q^\prime\left(t\right)\right)\,\left(\partial_0\,\partial_2\right)\left(L\right)\left(\Gamma[q]\left(t\right)\right) + \partial_2L\left(\Gamma[q]\left(t\right)\right)\,\left(\partial_0\,\partial_1\right)\left(F\right)\left(t, q^\prime\left(t\right)\right)\n\end{equation}
;; :end:

;; This, again, is total madness. We really want some way to control how Scheme
;; expands terms.

;; But we know what we're looking for. Expand out the matching term of the unprimed
;; Lagrange equations:


(->eq
 ((D (compose ((partial 2) L) (Gamma q)))
  't))


;; #+RESULTS[fe49bac2a687e3d14b31a41abfbb0ada1cbd5b03]:
;; :results:
;; \begin{equation}\n{\left(Dq^\prime\left(t\right)\right)}^{2}\,{\partial_1}^{2}F\left(t, q^\prime\left(t\right)\right)\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right) + \partial_1F\left(t, q^\prime\left(t\right)\right)\,Dq^\prime\left(t\right)\,\left(\partial_1\,\partial_2\right)\left(L\right)\left(\Gamma[q]\left(t\right)\right) + \partial_1F\left(t, q^\prime\left(t\right)\right)\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right)\,{D}^{2}q^\prime\left(t\right) + 2\,Dq^\prime\left(t\right)\,\left(\partial_0\,\partial_1\right)\left(F\right)\left(t, q^\prime\left(t\right)\right)\,{\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right) + \partial_0F\left(t, q^\prime\left(t\right)\right)\,\left(\partial_1\,\partial_2\right)\left(L\right)\left(\Gamma[q]\left(t\right)\right) + {\partial_2}^{2}L\left(\Gamma[q]\left(t\right)\right)\,{\partial_0}^{2}F\left(t, q^\prime\left(t\right)\right) + \left(\partial_0\,\partial_2\right)\left(L\right)\left(\Gamma[q]\left(t\right)\right)\n\end{equation}
;; :end:

;; Staring at these two equations, it becomes clear that the first contains the
;; second, multiplied by $\partial_1F(t, q'(t))$, the same factor that appeared in
;; the expansion of the $\partial_1 L \circ \Gamma[q]$ term.

;; Try writing out the primed Lagrange equations, and subtracting the unprimed
;; Lagrange equations, scaled by this factor:


(let [primed-lagrange
      (- (D (compose ((partial 2) Lprime) (Gamma qprime)))
         (compose ((partial 1) Lprime) (Gamma qprime)))

      lagrange
      (- (D (compose ((partial 2) L) (Gamma q)))
         (compose ((partial 1) L) (Gamma q)))

      factor
      (compose coordinate ((partial 1) C) (Gamma qprime))]
  (->tex-equation
   ((- primed-lagrange (* factor lagrange))
    't)))
