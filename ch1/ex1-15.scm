;; Code Preliminaries


(load "ch1/utils.scm")


;; #+RESULTS:
;; : ;Loading "ch1/utils.scm"...
;; : ;  Loading "ch1/exdisplay.scm"... done
;; : ;... done
;; : #| check-f |#

;; I didn't get terribly far converting this to code, but I'll leave what I have
;; here in case some intrepid soul wants to carry the torch. This can help us a
;; bit, so let me set up some structure.

;; Goal: Figure out how to write $C$ given $F$, so that the computer can take
;; derivatives for us. Ideally I'd push the entire proof through, but I got stuck.

;; Redefine the =F->C= function on page 46, so that $F$ takes an explicit $t$ and
;; $x'$ argument instead of having to rely on the tuple.


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

;; Functions:


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

;; And now, finally, we can talk about C. This matches what we have in the book.


(->tex-equation
 ((compose C (Gamma qprime)) 't))


;; #+RESULTS[ecef7fe872de385af827c689dbd0db76aa5319f0]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix}
;; \end{equation}


;; We can also do this by converting the path itself:


(define ((to-q F qp) t)
  (F t (qp t)))


;; #+RESULTS:
;; : #| to-q |#

;; It's the same:


(->tex-equation
 ((- (compose C (Gamma qprime))
     (Gamma (to-q F qprime)))
  't))


;; #+RESULTS[10aea4a5b833aa00e0a1c63049d844dc37528289]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{pmatrix}
;; \end{equation}

;; So we can define a few more things. Talk about these in a bit.


(define q (to-q F qprime))
(define Lprime (compose L C))
;; Code Version

;; Let's expand the terms of the Lagrange equations.


(->tex-equation
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))


;; #+RESULTS[5eac70f3edfcf5feeb3a2eb9b98d89646f21ade3]:
;; \begin{equation}
;; D{q}^\prime\left( t \right) {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) + {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; The Lagrange equation term is in there with a factor on it. Here's the term I need to subtract:


(let* ((factor
        (compose coordinate ((partial 1) C) (Gamma qprime)))      )
  (->tex-equation
   ((* factor (compose ((partial 1) L) (Gamma q)))
    't)))


;; #+RESULTS[881bca125c154512d159981aae326e02b633fb8d]:
;; \begin{equation}
;; {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right)
;; \end{equation}

;; So now we have the whole thing and the term to substract off. This is the extra:


(let* ((factor
        (compose coordinate ((partial 1) C) (Gamma qprime))))
  (->tex-equation
   ((- (compose ((partial 1) Lprime) (Gamma qprime))
       (* factor (compose ((partial 1) L) (Gamma q))))
    't)))


;; #+RESULTS[ab5e1b69d3d762f9eb3a7f477664ce4681321e66]:
;; \begin{equation}
;; D{q}^\prime\left( t \right) {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; Let's do the other side. Nightmare!


(->tex-equation
 ((D (compose ((partial 2) Lprime) (Gamma qprime)))
  't))


;; #+RESULTS[8f6c2926e2d5df141a3985bb361708120ec851f2]:
;; \begin{equation}
;; {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( D{q}^\prime\left( t \right) \right)}^{2} {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {D}^{2}{q}^\prime\left( t \right) {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + 2 {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\left( {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 0 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + D{q}^\prime\left( t \right) {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 0 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; I see Lagrange's equation peeking out, with a big factor attached to it.


(let* ((factor
        (compose velocity ((partial 2) C) (Gamma qprime))))
  (->tex-equation
   ((* factor (D (compose ((partial 2) L) (Gamma q))))
    't)))


;; #+RESULTS[a499b82f2449993cdf822f3c0f67685b380e4304]:
;; \begin{equation}
;; {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( D{q}^\prime\left( t \right) \right)}^{2} {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {D}^{2}{q}^\prime\left( t \right) {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + 2 {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\left( {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 0 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 0 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; About the factors, this is interesting:


(->tex-equation
 ((- (compose velocity ((partial 2) C) (Gamma qprime))
     (compose coordinate ((partial 1) C) (Gamma qprime)))
  't))


;; #+RESULTS[d1d70fdc52328f43b5e17b5b4503e83236322684]:
;; \begin{equation}
;; 0
;; \end{equation}

;; So the factors are identical. Why? Think this through.

;; If you pull the factor out, you see that you've generated new Lagrange
;; equations, scaled by a factor.


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
;; Derivation

;; The goal here is to show that if the Lagrange equations hold in the original
;; coordinate system:

;; \begin{equation}
;; \label{eq:1-15-lagrange}
;; D(\partial_2L \circ \Gamma[q]) - (\partial_1L \circ \Gamma[q]) = 0
;; \end{equation}

;; Then they hold in the primed coordinate system:

;; \begin{equation}
;; \label{eq:lagrange-prime}
;; D(\partial_2L' \circ \Gamma[q']) - (\partial_1L' \circ \Gamma[q']) = 0
;; \end{equation}

;; Approach: let's start pushing the Lagrange equations through, and see if we
;; recognize any spot where we can use our assumption.

;; We start by calculating the components of the Lagrange equations in equation
;; \eqref{eq:lagrange-prime}. We'll handle the $\partial_2L'$ term first.

;; Composition is associative, so

;; \begin{equation}
;; \label{eq:c-l}
;; (L \circ C) \circ \Gamma[q'] = L' \circ \Gamma[q'] \implies L' = L \circ C
;; \end{equation}

;; Substituting the right side of \eqref{eq:c-l} and using the chain rule:

;; \begin{equation}
;;   \partial_2L' = \partial_2(L \circ C) = ((DL) \circ C) \partial_2 C
;; \end{equation}

;; NOTE describe the shape of what's going on here, how we have these up and down
;; tuple shapes. That means we can expand this out:

;; \begin{equation}
;;   (DL \circ C)\partial_2 C = (\partial_0L \circ C)(I_0 \circ \partial_2 C) + (\partial_1L \circ C)(I_1 \circ \partial_2 C) + (\partial_2L \circ C)(I_2 \circ \partial_2 C)
;; \end{equation}

;; It's less confusing if you leave functional notation for a moment and imagine
;; application to some argument $x$. $\partial_2C(x)$ is an up-tuple with 3
;; elements $(\partial_2C(x)_0, \partial_2C(x)_1, \partial_2C(x)_2)$ and $DL(C(x))$
;; is a down-tuple, so they contract like this:


;; \begin{equation}
;;   DL(C(x)) \cdot \partial_2 C(x) = \partial_0L(C(x))\partial_2 C(x)_0 + \partial_1L(C(x))\partial_2 C(x)_1 + \partial_2L(C(x))\partial_2 C(x)_2
;; \end{equation}

;; Now look at the $\partial_2C$ term using our code from before:


(let ((C (F->C* F*)))
  (->tex-equation
   (((partial 2) C) (up 't 'xprime 'vprime))
   "eq:p2c"))


;; #+RESULTS[e53134b42f7b5b852bde3c4cdcf4f1ee489192a2]:
;; \begin{equation}
;; \begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ {\partial}_{1}F\left( t, {x}^\prime \right)}\end{pmatrix}
;; \label{eq:p2c}
;; \end{equation}

;; Add the path $\Gamma[q']$ back in and distribute, remembering that $C \circ \Gamma[q'] = \Gamma[q]$:

;; \begin{equation}
;;   \begin{aligned}
;;     \partial_2L' \circ \Gamma[q'] & = (\partial_2L \circ C)(I_2 \circ \partial_2 C) \circ \Gamma[q'] \cr
;;     & = (\partial_2L \circ C \circ \Gamma[q'])(I_2 \circ \partial_2 C \circ \Gamma[q']) \cr
;;     & = (\partial_2L \circ \Gamma[q])(I_2 \circ \partial_2 C \circ \Gamma[q'])
;;   \end{aligned}
;; \end{equation}

;; Now take the time derivative:

;; \begin{equation}
;;   D(\partial_2L' \circ \Gamma[q']) = D\left[(\partial_2L \circ \Gamma[q])(I_2 \circ \partial_2 C \circ \Gamma[q'])\right]
;; \end{equation}

;; Use the product rule:

;; \begin{equation}
;;   D(\partial_2L' \circ \Gamma[q']) = \left[ D(\partial_2L \circ \Gamma[q]) \right](I_2 \circ \partial_2 C \circ \Gamma[q']) + (\partial_2L \circ \Gamma[q])D\left[ (I_2 \circ \partial_2 C \circ \Gamma[q']) \right]
;; \end{equation}

;; substitute using \eqref{eq:p2c}. When we compose with a path we get this
;; function of $t$:


(let ((C (F->C* F*)))
  (->tex-equation
   (ref ((compose ((partial 2) C) (Gamma qprime)) 't) 2)))


;; #+RESULTS[d9a914ea8ba205691e7831814d452efe00a4a327]:
;; \begin{equation}
;; {\partial}_{1}F\left( t, {q}^\prime\left( t \right) \right)
;; \end{equation}

;; Substitute:

;; \begin{equation}
;; \label{eq:1-15-p2l}
;;   D(\partial_2L' \circ \Gamma[q']) = D(\partial_2L \circ \Gamma[q])\partial_1F(t, q'(t)) + (\partial_2L \circ \Gamma[q]) D(\partial_1F(t, q'(t)))
;; \end{equation}

;; We can see Lagrange's equation from our assumption peeking. Before we tackle
;; that, let's do the other side:

;; \begin{equation}
;;   \partial_1L' = \partial_1(L \circ C) = ((DL) \circ C) \partial_1 C
;; \end{equation}

;; Use our function above to take $\partial_1C$:


(let ((C (F->C* F*)))
  (->tex-equation
   (((partial 1) C) (up 't 'xprime 'vprime))))
