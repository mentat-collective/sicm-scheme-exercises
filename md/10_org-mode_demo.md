    - [Equations](#sec-1)

This is an example of how we might structure an org-mode file that can export out to Github flavored Markdown, or to a PDF.

First, let's get some code loaded up and written. Here's a block that converts polar coordinates to rectangular coordinates.

```scheme
(define (p->r local)
  (let* ((polar-tuple (coordinate local))
         (r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))
```

This is some good stuff.

```scheme
(load "ch1/utils.scm")

(define (p->r local)
  (let* ((polar-tuple (coordinate local))
         (r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))

(define (spherical->rect local)
  (let* ((spherical-tuple (coordinate local))
         (r (ref spherical-tuple 0))
         (theta (ref spherical-tuple 1))
         (phi (ref spherical-tuple 2)))
    (up (* r (sin theta) (cos phi))
        (* r (sin theta) (sin phi))
        (* r (cos theta)))))
```

And another, that gets us from spherical to rectangular.

```scheme
(define (spherical->rect local)
  (let* ((spherical-tuple (coordinate local))
         (r (ref spherical-tuple 0))
         (theta (ref spherical-tuple 1))
         (phi (ref spherical-tuple 2)))
    (up (* r (sin theta) (cos phi))
        (* r (sin theta) (sin phi))
        (* r (cos theta)))))
```

    ;Loading "ch1/utils.scm"... done
    #| "" |#

This block will generate a LaTeX version of the code I've supplied:

```scheme
(->tex-equation
 ((+ (literal-function 'c)
     (D (literal-function 'z)))
  't)
 "eq:masterpiece")
```

\begin{equation}
c\left( t \right) + Dz\left( t \right)
\label{eq:masterpiece}
\end{equation}

You can even reference these with equation numbers, like Equation \eqref{eq:masterpiece} above.

```scheme
(up 1 2 't)
```

    #|
    (up 1 2 t)
    |#

# Equations<a id="sec-1"></a>

Here's (a test) of \\(a = bc\\) and more \\[ \alpha\_t \\] equations:

And again this is a thing:

\\[ e^{i\pi} = -1 \\]

\\[ \int\_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2} \\]

\\(\vec{x} \dot (\vec{x} \times \vec{v}) = \vec{v} \dot (\vec{x} \times \vec{v}) = 0\\)

\\(\vec{x} \cdot (\vec{x} \times \vec{v}) = \vec{v} \dot (\vec{x} \times \vec{b}) = 0\\)
