  - [Exercise 9.1 Chain Rule](#sec-1)
  - [Exercise 9.2: Computing Derivatives](#sec-2)

Notation Appendix. This is all about getting cozy with scheme, and with the various idiosyncracies of the tuple and functional notation.

# Exercise 9.1 Chain Rule<a id="sec-1"></a>

You're supposed to do these by hand, so I'll do that in the textbook. But here, let's redo them on the machine.

1.  Compute \\(\partial\_0 F(x, y)\\) and \\(\partial\_1 F(x, y)\\)

    First, let's define the functions we need.

    ```clojure
    (defn F [x y]
      (* (square x)
         (cube y)))

    (defn G [x y]
      (up (F x y) y))

    (defn H [x y]
      (F (F x y) y))
    ```

    You can do this with explicit partials:

    ```clojure
    (let [f (down ((partial 0) F) ((partial 1) F))]
      (->tex-equation
       (f 'x 'y)))
    ```

    \begin{equation}\n\begin{bmatrix}\displaystyle{2\,x\,{y}^{3}} \cr \cr \displaystyle{3\,{x}^{2}\,{y}^{2}}\end{bmatrix}\n\end{equation}

    Or with the \\(D\\) symbol:

    ```clojure
    (->tex-equation
     ((D F) 'x 'y))
    ```

    \begin{equation}\n\begin{bmatrix}\displaystyle{2\,x\,{y}^{3}} \cr \cr \displaystyle{3\,{x}^{2}\,{y}^{2}}\end{bmatrix}\n\end{equation}

    Or, we could show that they're equivalent this way:

    ```clojure
    (let [f (down ((partial 0) F) ((partial 1) F))]
      (->tex-equation
       (- ((D F) 'x 'y)
          (f 'x 'y))))
    ```

    \begin{equation}\n\begin{bmatrix}\displaystyle{0} \cr \cr \displaystyle{0}\end{bmatrix}\n\end{equation}

2.  Compute \\(\partial\_0 F(F(x, y), y)\\) and \\(\partial\_1 F(F(x, y), y)\\)

    \\(H\\) is already that composition, so:

    ```clojure
    (->tex-equation
     ((D H) 'x 'y))
    ```

    \begin{equation}\n\begin{bmatrix}\displaystyle{4\,{x}^{3}\,{y}^{9}} \cr \cr \displaystyle{9\,{x}^{4}\,{y}^{8}}\end{bmatrix}\n\end{equation}

3.  Compute \\(\partial\_0 G(x, y)\\) and \\(\partial\_1 G(x, y)\\)

    ```clojure
    (->tex-equation
     ((D G) 'x 'y))
    ```

    \begin{equation}\n\begin{bmatrix}\displaystyle{\begin{pmatrix}\displaystyle{2\,x\,{y}^{3}} \cr \cr \displaystyle{0}\end{pmatrix}} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{3\,{x}^{2}\,{y}^{2}} \cr \cr \displaystyle{1}\end{pmatrix}}\end{bmatrix}\n\end{equation}

4.  Compute \\(DF(a, b)\\), \\(DG(3, 5)\\) and \\(DH(3a^2, 5b^3)\\)

    ```clojure
    (->tex-equation
     (up ((D F) 'a 'b)
         ((D G) 3 5)
         ((D H) (* 3 (square 'a)) (* 5 (cube 'b)))))
    ```

    \begin{equation}\n\begin{pmatrix}\displaystyle{\begin{bmatrix}\displaystyle{2\,a\,{b}^{3}} \cr \cr \displaystyle{3\,{a}^{2}\,{b}^{2}}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{\begin{pmatrix}\displaystyle{750} \cr \cr \displaystyle{0}\end{pmatrix}} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{675} \cr \cr \displaystyle{1}\end{pmatrix}}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{210937500\,{a}^{6}\,{b}^{27}} \cr \cr \displaystyle{284765625\,{a}^{8}\,{b}^{24}}\end{bmatrix}}\end{pmatrix}\n\end{equation}

# Exercise 9.2: Computing Derivatives<a id="sec-2"></a>

A further exercise is to try defining the functions so that they use explicit tuples, so you can compose them:

```clojure
(defn F* [[x y]]
  (* (square x) (cube y)))

(defn G* [[_ y :as v]]
  (up (F* v) y))

(def H* (compose F* G*))
```

to be really pro, I'd make a function that takes these as arguments and prints a nice formatted exercise output. Let's do the final exercise, for fun:

```clojure
(->tex-equation
 (up ((D F*) (up 'a 'b))
     ((D G*) (up 3 5))
     ((D H*) (up (* 3 (square 'a)) (* 5 (cube 'b))))))
```

\begin{equation}\n\begin{pmatrix}\displaystyle{\begin{bmatrix}\displaystyle{2\,a\,{b}^{3}} \cr \cr \displaystyle{3\,{a}^{2}\,{b}^{2}}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{\begin{pmatrix}\displaystyle{750} \cr \cr \displaystyle{0}\end{pmatrix}} \cr \cr \displaystyle{\begin{pmatrix}\displaystyle{675} \cr \cr \displaystyle{1}\end{pmatrix}}\end{bmatrix}} \cr \cr \displaystyle{\begin{bmatrix}\displaystyle{210937500\,{a}^{6}\,{b}^{27}} \cr \cr \displaystyle{284765625\,{a}^{8}\,{b}^{24}}\end{bmatrix}}\end{pmatrix}\n\end{equation}
