    - [Equations](#sec-1)

This is an example of how we might structure an org-mode file that can export out to Github flavored Markdown, or to a PDF.

First, let's get some code loaded up and written. Here's a block that converts polar coordinates to rectangular coordinates.

```clojure
(defn p->r [local]
  (let [[r phi] (coordinate local)
        x (* r (cos phi))
        y (* r (sin phi))]
    (up x y)))
```

This is some good stuff.

```clojure
(ns ch1.demo
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [sicmutils.env :as e #?@(:cljs [:include-macros true])]
            [sicmutils.expression.render :as render]
            [taoensso.timbre :refer [set-level!]]))

(e/bootstrap-repl!)
(set-level! :fatal)

(defn ->tex-equation* [e]
  (let [eq (render/->TeX (simplify e))]
    (str "\\begin{equation}\n"
         eq
         "\n\\end{equation}")))

(defn ->tex-equation [e]
  (println
   (->tex-equation* e)))

(defn p->r [local]
  (let [[r phi] (coordinate local)
        x (* r (cos phi))
        y (* r (sin phi))]
    (up x y)))

(defn spherical->rect [[_ [r theta phi]]]
  (up (* r (sin theta) (cos phi))
      (* r (sin theta) (sin phi))
      (* r (cos theta))))
```

And another, that gets us from spherical to rectangular.

```clojure
(defn spherical->rect [[_ [r theta phi]]]
  (up (* r (sin theta) (cos phi))
      (* r (sin theta) (sin phi))
      (* r (cos theta))))
```

    ;Loading "src/ch1/utils.cljc"... done
    #| "" |#

This block will generate a LaTeX version of the code I've supplied:

```clojure
(->tex-equation
 ((+ (literal-function 'c)
     (D (literal-function 'z)))
  't)
 :label "eq:masterpiece")
```

class clojure.lang.ArityException

You can even reference these with equation numbers, like Equation \eqref{eq:masterpiece} above.

```clojure
(up 1 2 't)
```

|    |    |    |    |
|--- |--- |--- |--- |
| up | 1 | 2 | t |

# Equations<a id="sec-1"></a>

Here's (a test) of \\(a = bc\\) and more \\[ \alpha\_t \\] equations:

And again this is a thing:

\\[ e^{i\pi} = -1 \\]

\\[ \int\_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2} \\]

\\(\vec{x} \dot (\vec{x} \times \vec{v}) = \vec{v} \dot (\vec{x} \times \vec{v}) = 0\\)

\\(\vec{x} \cdot (\vec{x} \times \vec{v}) = \vec{v} \dot (\vec{x} \times \vec{b}) = 0\\)
