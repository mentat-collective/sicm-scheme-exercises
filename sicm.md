- [Structure and Interpretation of Classical Mechanics](#sec-1)
  - [Chapter 1](#sec-1-1)
    - [Equations](#sec-1-1-1)
    - [Concluding.](#sec-1-1-2)


# Structure and Interpretation of Classical Mechanics<a id="sec-1"></a>

## Chapter 1<a id="sec-1-1"></a>

This is some nice code! Testing it out!

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

```scheme
(->tex-equation
 ((+ (literal-function 'c)
     (D (literal-function 'z)))
  't)
 "eq:masterpiece")
```

Can you see \eqref{eq:masterpiece} above?

```scheme
(up 1 2 't)
```

    #|
    (up 1 2 t)
    |#

### Equations<a id="sec-1-1-1"></a>

Here's a test of \(a = bc\) and more \[ \alpha_t \] equations:

\(and again\) this is a thing.

\[ e^{i\pi} = -1 \]

\[ \int_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2} \]

### Concluding.<a id="sec-1-1-2"></a>

This is the rest of the goods.
