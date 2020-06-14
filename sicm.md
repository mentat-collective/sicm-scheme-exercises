- [Preface](#sec-)
- [Lagrangian Mechanics](#sec-1)
  - [Exercise 1.1: Degrees of Freedom and 1.2: Generalized Coordinates](#sec-1-1)
  - [Exercise 1.3: Fermat optics](#sec-1-2)
    - [Law of Reflection](#sec-1-2-1)
    - [Law of Refraction](#sec-1-2-2)
  - [Section 1.4: Computing Actions](#sec-1-3)
  - [Exercise 1.4: Lagrangian actions](#sec-1-4)
  - [Paths of Minimum Action](#sec-1-5)
    - [Finding trajectories that minimize the action](#sec-1-5-1)
  - [Exercise 1.5: Solution process](#sec-1-6)
  - [Exercise 1.6: Minimizing action](#sec-1-7)
    - [Executions](#sec-1-7-1)
  - [Exercise 1.7: Properties of \\(\delta\\)](#sec-1-8)
    - [Variation Product Rule](#sec-1-8-1)
    - [Variation Sum Rule](#sec-1-8-2)
    - [Variation Scalar Multiplication](#sec-1-8-3)
    - [Chain Rule for Variations](#sec-1-8-4)
    - [\\(\delta\_\eta\\) commutes with \\(D\\)](#sec-1-8-5)
  - [Exercise 1.8: Implementation of \\(\delta\\)](#sec-1-9)
    - [Part A: Implement \\(\delta\_\eta\\)](#sec-1-9-1)
    - [Part B: Check \\(\delta\_\eta\\)'s properties](#sec-1-9-2)
  - [Exercise 1.9: Lagrange's equations](#sec-1-10)
  - [Exercise 1.10: Higher-derivative Lagrangians](#sec-1-11)
    - [Higher dimensions](#sec-1-11-1)
  - [Exercise 1.11: Kepler's third law](#sec-1-12)
  - [Exercise 1.12: Lagrange's equations (code)](#sec-1-13)
    - [Part A: Ideal Planar Pendulum](#sec-1-13-1)
    - [Part B: 2D Potential](#sec-1-13-2)
    - [Part C: Particle on a Sphere](#sec-1-13-3)
  - [Exercise 1.13: Higher-derivative Lagrangians (code)](#sec-1-14)
    - [Part A: Acceleration-dependent Lagrangian Implementation](#sec-1-14-1)
    - [Part B: Applying HO-Lagrangians](#sec-1-14-2)
    - [Part C: Generalized Lagrange Equations](#sec-1-14-3)
  - [Exercise 1.14: Coordinate-independence of Lagrange equations](#sec-1-15)
  - [Exercise 1.15: Equivalence](#sec-1-16)
    - [Overview](#sec-1-16-1)
    - [Coordinate Transformations](#sec-1-16-2)
    - [Code Preliminaries](#sec-1-16-3)
    - [Code Version](#sec-1-16-4)
    - [Derivation](#sec-1-16-5)
  - [Exercise 1.16: Central force motion](#sec-1-17)
  - [Exercise 1.17: Bead on a helical wire](#sec-1-18)
  - [Exercise 1.18: Bead on a triaxial surface](#sec-1-19)
  - [Exercise 1.19: Two-bar linkage](#sec-1-20)
  - [Exercise 1.20: Sliding pendulum](#sec-1-21)
  - [Exercise 1.21: A dumbbell](#sec-1-22)
  - [Exercise 1.22: Driven pendulum](#sec-1-23)
  - [Exercise 1.23: Fill in the details](#sec-1-24)
  - [Exercise 1.24: Constraint forces](#sec-1-25)
  - [Exercise 1.25: Foucalt pendulum Lagrangian](#sec-1-26)
  - [Exercise 1.26: Properties of \\(D\_t\\)](#sec-1-27)
  - [Exercise 1.27: Lagrange equations for total time derivatives](#sec-1-28)
  - [Exercise 1.28: Total Time Derivatives](#sec-1-29)
    - [part A](#sec-1-29-1)
    - [Part B](#sec-1-29-2)
    - [Part C](#sec-1-29-3)
    - [Part D](#sec-1-29-4)
    - [Part E](#sec-1-29-5)
    - [Part F](#sec-1-29-6)
  - [Exercise 1.29: Galilean Invariance](#sec-1-30)
  - [Exercise 1.30: Orbits in a central potential](#sec-1-31)
  - [Exercise 1.31: Foucault pendulum evolution](#sec-1-32)
  - [Exercise 1.32: Time-dependent constraints](#sec-1-33)
  - [Exercise 1.33: Falling off a log](#sec-1-34)
  - [Exercise 1.34: Driven spherical pendulum](#sec-1-35)
  - [Exercise 1.35: Restricted equations of motion](#sec-1-36)
  - [Exercise 1.36: Noether integral](#sec-1-37)
  - [Exercise 1.37: Velocity transformation](#sec-1-38)
  - [Exercise 1.38: Properties of \\(E\\)](#sec-1-39)
  - [Exercise 1.39: Combining Lagrangians](#sec-1-40)
  - [Exercise 1.40: Bead on a triaxial surface](#sec-1-41)
  - [Exercise 1.41: Motion of a tiny golf ball](#sec-1-42)
  - [Exercise 1.42: Augmented Lagrangian](#sec-1-43)
  - [Exercise 1.43: A numerical investigation](#sec-1-44)
  - [Exercise 1.44: Double pendulum behavior](#sec-1-45)
- [Rigid Bodies](#sec-2)
  - [Exercise 2.1](#sec-2-1)
  - [Exercise 2.2](#sec-2-2)
  - [Exercise 2.3](#sec-2-3)
  - [Exercise 2.4](#sec-2-4)
  - [Exercise 2.5](#sec-2-5)
  - [Exercise 2.6](#sec-2-6)
  - [Exercise 2.7](#sec-2-7)
  - [Exercise 2.8](#sec-2-8)
  - [Exercise 2.9](#sec-2-9)
  - [Exercise 2.10](#sec-2-10)
  - [Exercise 2.11](#sec-2-11)
  - [Exercise 2.12](#sec-2-12)
  - [Exercise 2.13](#sec-2-13)
  - [Exercise 2.14](#sec-2-14)
  - [Exercise 2.15](#sec-2-15)
  - [Exercise 2.16](#sec-2-16)
  - [Exercise 2.17](#sec-2-17)
  - [Exercise 2.18](#sec-2-18)
  - [Exercise 2.19](#sec-2-19)
  - [Exercise 2.20](#sec-2-20)
- [Hamiltonian Mechanics](#sec-3)
  - [Exercise 3.1](#sec-3-1)
  - [Exercise 3.2](#sec-3-2)
  - [Exercise 3.3](#sec-3-3)
  - [Exercise 3.4](#sec-3-4)
  - [Exercise 3.5](#sec-3-5)
  - [Exercise 3.6](#sec-3-6)
  - [Exercise 3.7](#sec-3-7)
  - [Exercise 3.8](#sec-3-8)
  - [Exercise 3.9](#sec-3-9)
  - [Exercise 3.10](#sec-3-10)
  - [Exercise 3.11](#sec-3-11)
  - [Exercise 3.12](#sec-3-12)
  - [Exercise 3.13](#sec-3-13)
  - [Exercise 3.14](#sec-3-14)
  - [Exercise 3.15](#sec-3-15)
  - [Exercise 3.16](#sec-3-16)
- [Phase Space Structure](#sec-4)
  - [Exercise 4.0](#sec-4-1)
  - [Exercise 4.1](#sec-4-2)
  - [Exercise 4.2](#sec-4-3)
  - [Exercise 4.3](#sec-4-4)
  - [Exercise 4.4](#sec-4-5)
  - [Exercise 4.5](#sec-4-6)
  - [Exercise 4.6](#sec-4-7)
  - [Exercise 4.7](#sec-4-8)
  - [Exercise 4.8](#sec-4-9)
  - [Exercise 4.9](#sec-4-10)
  - [Exercise 4.10](#sec-4-11)
- [Canonical Transformations](#sec-5)
  - [Exercise 5.1](#sec-5-1)
  - [Exercise 5.2](#sec-5-2)
  - [Exercise 5.3](#sec-5-3)
  - [Exercise 5.4](#sec-5-4)
  - [Exercise 5.5](#sec-5-5)
  - [Exercise 5.6](#sec-5-6)
  - [Exercise 5.7](#sec-5-7)
  - [Exercise 5.8](#sec-5-8)
  - [Exercise 5.9](#sec-5-9)
  - [Exercise 5.10](#sec-5-10)
  - [Exercise 5.11](#sec-5-11)
  - [Exercise 5.12](#sec-5-12)
  - [Exercise 5.13](#sec-5-13)
  - [Exercise 5.14](#sec-5-14)
  - [Exercise 5.15](#sec-5-15)
  - [Exercise 5.16](#sec-5-16)
  - [Exercise 5.17](#sec-5-17)
  - [Exercise 5.18](#sec-5-18)
  - [Exercise 5.19](#sec-5-19)
  - [Exercise 5.20](#sec-5-20)
- [Canonical Evolution](#sec-6)
  - [Exercise 6.1](#sec-6-1)
  - [Exercise 6.2](#sec-6-2)
  - [Exercise 6.3](#sec-6-3)
  - [Exercise 6.4](#sec-6-4)
  - [Exercise 6.5](#sec-6-5)
  - [Exercise 6.6](#sec-6-6)
  - [Exercise 6.7](#sec-6-7)
  - [Exercise 6.8](#sec-6-8)
  - [Exercise 6.9](#sec-6-9)
  - [Exercise 6.10](#sec-6-10)
  - [Exercise 6.11](#sec-6-11)
  - [Exercise 6.12](#sec-6-12)
- [Canonical Perturbation Theory](#sec-7)
  - [Exercise 7.1](#sec-7-1)
  - [Exercise 7.2](#sec-7-2)
  - [Exercise 7.3](#sec-7-3)
  - [Exercise 7.4](#sec-7-4)
  - [Exercise 7.5](#sec-7-5)
- [Our Notation](#sec-8)
  - [Exercise 9.1 Chain Rule](#sec-8-1)
  - [Exercise 9.2: Computing Derivatives](#sec-8-2)
- [Org-Mode Demo](#sec-9)
    - [Equations](#sec-9-0-1)

Welcome to my tour of Structure and Interpretation of Classical Mechanics. I'm working on this book to develop my sense of the best way to do research in public; this book is heavy on math, programming and visualization, and should stress the normal tools.

I'm attempting to take notes in on org-mode file, and generate all my code from there.

I don't think I have the heart, or the time, to really do high-class notes of every single section; but I am going to do each of the exercises, and explore some of the code-based concepts in each section.

# Preface<a id="sec-"></a>

The preface is already intriguing. A tour through the new notation, plus some discussion of why a programming language is the best route in to this stuff. Both of these are extremely powerful ideas, and why I was pulled to this book in the first place.

The functional notation is:

\begin{equation}
  D (\partial\_2 L \circ \Gamma[q]) - (\partial\_1 L \circ \Gamma[q]) = 0
\end{equation}

Compare that to the traditional notation:

\begin{equation}
  \frac{d}{dt} \frac{\partial L}{\partial \dot q^i} -\frac{\partial L}{\partial q^i}= 0
\end{equation}

They have a nice riff on how this is totally ambiguous. \\(\Gamma\\) is not a great way to go.

# Lagrangian Mechanics<a id="sec-1"></a>

## Exercise 1.1: Degrees of Freedom and 1.2: Generalized Coordinates<a id="sec-1-1"></a>

> For each of the mechanical systems described below, give the number of degrees of freedom of the configuration space. ([SICM, ex1](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-1))

[Exercise 1.2](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-2) asks about the generalized coordinates of each, so I'll note those here too.

1.  Three juggling pins.

    ****18 degrees of freedom**** for three juggling pins - 3 position, 3 angles for each. OR ****15**** total, if you assume that the pin is symmetric; then you just need two degrees of tilt.

2.  A spherical pendulum consisting of a point mass (the pendulum bob) hanging from a rigid massless rod attached to a fixed support point. The pendulum bob may move in any direction subject to the constraint imposed by the rigid rod. The point mass is subject to the uniform force of gravity.

    Spherical pendulum,hanging from a fixed support has ****two degrees of freedom****, one for each angle.

3.  Spherical DOUBLE pendulum has ****four degrees of freedom****; two angles from previous, plus two more angles for the second pendulum off of the first.

    A spherical double pendulum, consisting of one point mass hanging from a rigid massless rod attached to a second point mass hanging from a second massless rod attached to a fixed support point. The point masses are subject to the uniform force of gravity.

4.  A point mass sliding without friction on a rigid curved wire.

    ****1 degree of freedom****, distance along the wire.

5.  A top consisting of a rigid axisymmetric body with one point on the symmetry axis of the body attached to a fixed support, subject to a uniform gravitational force.

    Axisymmetric top has ****TWO degrees of freedom****, for the angles off of vertical. But then we can't talk about its speed, so that's a little strange. I guess it has an angular momentum as a property, but that's not a coordinate.

6.  The same as e, but not axisymmetric.

    We now have ****THREE degrees of freedom****, one more for the angle of the top's rotation.

## Exercise 1.3: Fermat optics<a id="sec-1-2"></a>

This problem has us exploring some consequences for optics of the principle of least time. [Exercise 1.3](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-3) states:

> Fermat observed that the laws of reflection and refraction could be accounted for by the following facts: Light travels in a straight line in any particular medium with a velocity that depends upon the medium. The path taken by a ray from a source to a destination through any sequence of media is a path of least total time, compared to neighboring paths. Show that these facts imply the laws of reflection and refraction.

### Law of Reflection<a id="sec-1-2-1"></a>

The [law of reflection](https://en.wikipedia.org/wiki/Reflection_(physics)#Laws_of_reflection) is described in the footnote:

> For reflection the angle of incidence is equal to the angle of reflection.

Here's the setup. The horizontal line is a mirror. The law states that \\(\theta\_1 = \theta\_2\\).

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_10-31-24_screenshot.png)

We have to show that if we consider all possible paths from a given starting point to a given endpoint, the path of minimum time will give us the law of reflection.

The *actual* path of minimum time is the straight line that avoids the mirror, of course. If we force the light to bounce off of the mirror, then we have to figure out where it will hit, where \\(x\_p\\) is, to minimize the time between the start and end points.

There are two ways to solve this problem. We can use geometry and visual intuition, or we can use calculus.

1.  Geometry

    First, recall this fact from the problem text:

    > Light travels in a straight line in any particular medium with a velocity that depends upon the medium.

    There's no medium change, so if there were no mirror in its path, the light beam would continue in a straight line. Instead of figuring out what the beam will do when it hits the mirror, reflect the endpoint across the mirror and draw a straight line between the start and "end" points:

    ![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png)

    The angle that the beam makes with the plane of the mirror is the same on both sides of the mirror.

    Now reflect the the "end" point and the segment of the beam that's crossed the mirror back up. By symmetry, \\(\theta\_1 = \theta\_2\\), and we've proved the law of reflection.

2.  Calculus

    We can also solve this with calculus. Because the beam doesn't change media, its speed \\(v\\) stays constant, so minimizing the total distance \\(d\\) is equivalent to minimizing the time \\(t = {d \over v}\\).

    Set \\(x\_1 = 0\\) for convenience, and write the total distance the light travels as a function of \\(x\_p\\):

    \begin{equation}
    d(x\_p) = \sqrt{y\_1^2 + x\_p^2} + \sqrt{(x\_2 - x\_p)^2 + y\_2^2}
    \end{equation}

    For practice, we can also define this function in Scheme.

    ```scheme
    (define ((total-distance x1 y1 x2 y2) xp)
      (+ (sqrt (+ (square (+ x1 xp))
                  (square y1)))
         (sqrt (+ (square (- x2 (+ x1 xp)))
                  (square y2)))))
    ```

    Here's the function again, generated from code, with general \\(t\_1\\):

    ```scheme
    (->tex-equation
     ((total-distance 'x_1 'y_1 'x_2 'y_2) 'x_p))
    ```

    \begin{equation}
    \sqrt{{{x}\_{1}}^{2} + 2 {x}\_{1} {x}\_{p} + {{x}\_{p}}^{2} + {{y}\_{1}}^{2}} + \sqrt{{{x}\_{1}}^{2} - 2 {x}\_{1} {x}\_{2} + 2 {x}\_{1} {x}\_{p} + {{x}\_{2}}^{2} - 2 {x}\_{2} {x}\_{p} + {{x}\_{p}}^{2} + {{y}\_{2}}^{2}}
    \end{equation}

    To find the \\(x\_p\\) that minimizes the total distance,

    -   take the derivative with respect to \\(x\_p\\),
    -   set it equal to 0 and
    -   solve for \\(x\_p\\).

    The derivative will look cleaner in code if we keep the components of the sum separate and prevent Scheme from "simplifying". Redefine the function to return a tuple:

    ```scheme
    (define ((total-distance* x1 y1 x2 y2) xp)
      (up (sqrt (+ (square (+ x1 xp))
                   (square y1)))
          (sqrt (+ (square (- x2 (+ x1 xp)))
                   (square y2)))))
    ```

    Here are the sum components:

    ```scheme
    (->tex-equation
     ((total-distance* 0 'y_1 'x_2 'y_2) 'x_p))
    ```

    \begin{equation}
    \begin{pmatrix} \displaystyle{ \sqrt{{{x}\_{p}}^{2} + {{y}\_{1}}^{2}}} \cr \cr \displaystyle{ \sqrt{{{x}\_{2}}^{2} - 2 {x}\_{2} {x}\_{p} + {{x}\_{p}}^{2} + {{y}\_{2}}^{2}}}\end{pmatrix}
    \end{equation}

    Taking a derivative is easy with `scmutils`. Just wrap the function in `D`:

    ```scheme
    (let* ((distance-fn (total-distance* 0 'y_1 'x_2 'y_2))
           (derivative (D distance-fn)))
      (->tex-equation
       (derivative 'x_p)))
    ```

    \begin{equation}
    \begin{pmatrix} \displaystyle{ {{{x}\_{p}}\over {\sqrt{{{x}\_{p}}^{2} + {{y}\_{1}}^{2}}}}} \cr \cr \displaystyle{ {{ - {x}\_{2} + {x}\_{p}}\over {\sqrt{{{x}\_{2}}^{2} - 2 {x}\_{2} {x}\_{p} + {{x}\_{p}}^{2} + {{y}\_{2}}^{2}}}}}\end{pmatrix}
    \end{equation}

    The first component is the base of base \\(x\_p\\) of the left triangle over the total length. This ratio is equal to \\(\cos \theta\_1\\):

    ![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png)

    The bottom component is \\(-\cos \theta\_2\\), or \\({- (x\_2 - x\_p)}\\) over the length of the right segment. Add these terms together, set them equal to 0 and rearrange:

    \begin{equation}
    \label{eq:reflect-laws}
    \cos \theta\_1 = \cos \theta\_2 \implies \theta\_1 = \theta\_2
    \end{equation}

    This description in terms of the two incident angles isn't so obvious from the Scheme code. Still, you can use Scheme to check this result.

    If the two angles are equal, then the left and right triangles are similar, and the ratio of each base to height is equal:

    \begin{equation}
    \label{eq:reflect-ratio}
    {x\_p \over y\_1} = {{x\_2 - x\_p} \over y\_2}
    \end{equation}

    Solve for \\(x\_p\\) and rearrange:

    \begin{equation}
    \label{eq:reflect-ratio2}
    x\_p = {{y\_1 x\_2} \over {y\_1 + y\_2}}
    \end{equation}

    Plug this in to the derivative of the original `total-distance` function, and we find that the derivative equals 0, as expected:

    ```scheme
    (let* ((distance-fn (total-distance 0 'y_1 'x_2 'y_2))
           (derivative (D distance-fn)))
      (->tex-equation
       (derivative (/ (* 'y_1 'x_2) (+ 'y_1 'y_2)))))
    ```

    \begin{equation}
    0
    \end{equation}

    If a beam of light travels in a way that minimizes total distance (and therefore time in a constant medium), then it will reflect off of a mirror with the same angle at which it arrived. The law of reflection holds.

### Law of Refraction<a id="sec-1-2-2"></a>

The law of refraction is also called [Snell's law](https://en.wikipedia.org/wiki/Snell%27s_law). Here's the description from the footnote:

> Refraction is described by Snell's law: when light passes from one medium to another, the ratio of the sines of the angles made to the normal to the interface is the inverse of the ratio of the refractive indices of the media. The refractive index is the ratio of the speed of light in the vacuum to the speed of light in the medium.

First we'll tackle this with calculus.

1.  Calculus

    The setup here is slightly different. We have a light beam traveling from one medium to another and changing speeds at a boundary located \\(a\\) to the right of the starting point. The goal is to figure out the point where the light will hit the boundary, if we assume that the light will take the path of least time.

    ![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_12-03-11_screenshot.png)

    The refractive index \\(n\_i = {c \over v\_i}\\), the speed of light \\(c\\) in a vacuum over the speed in the material. Rearranging, \\(v\_i = {c \over n\_i}\\).

    Time is distance over speed, so the total time that the beam spends between the start and end points as a function of \\(y\_p\\), the point of contact with the boundary, is:

    \begin{equation}
      \begin{aligned}
        t(y\_p) & = {c \sqrt{a^2 + y\_p^2}\over v\_1} + {c \sqrt{(x\_2 - x\_p)^2 + y\_2^2} \over v\_2} \cr
        & = {n\_1 \over c} \sqrt{a^2 + y\_p^2} + {n\_2 \over c} \sqrt{(x\_2 - x\_p)^2 + y\_2^2}
      \end{aligned}
    \end{equation}

    Take the derivative:

    \begin{equation}
      Dt(y\_p) = {1 \over c} \left({n\_1 y\_p \over \sqrt{a^2 + y\_p^2}} - {n\_2 (x\_2 - x\_p) \over \sqrt{(x\_2 - x\_p)^2 + y\_2^2}}\right)
    \end{equation}

    Set the derivative equal to 0 and split terms:

    \begin{equation}
    \label{eq:almost-snell}
      {n\_1 y\_p \over \sqrt{a^2 + y\_p^2}} = {n\_2 (x\_2 - x\_p) \over \sqrt{(x\_2 - x\_p)^2 + y\_2^2}}
    \end{equation}

    Similar to the law of reflection's result, each term (up to its \\(n\_i\\) multiple) is equal to the height of the left or right triangle over the length of the beam's path on the left or right of the boundary.

    Equation \eqref{eq:almost-snell} simplifies to:

    \begin{equation}
      n\_1 \sin \theta\_1 = n\_2 \sin \theta\_2
    \end{equation}

    Rearranging yields Snell's law:

    \begin{equation}
    {n\_1 \over n\_2} = {\sin \theta\_2 \over \sin \theta\_1}
    \end{equation}

2.  Geometry

    I won't recreate this here, but the [Feynman Lectures on Physics](https://www.feynmanlectures.caltech.edu/I_26.html), in [Lecture 26](https://www.feynmanlectures.caltech.edu/I_26.html), has a fantastic discussion about, and derivation of, the law of refraction using no calculus, just geometry. I highly recommend you check out that lecture. Feynman lays out a number of examples of how the principle of least time is not just a restatement of the optical rules we already knew.

    You can use the idea to guess what shape of mirror you'd want to build to focus many light rays on a single point (a parabola), or how you might force all light rays coming out of a single point to meet up again at another point (build a converging lens).

    This whole area of optics and least time has obsessed scientists for hundreds of years. Spend a few minutes [poking around](https://www.feynmanlectures.caltech.edu/I_26.html) and see what you find.

## Section 1.4: Computing Actions<a id="sec-1-3"></a>

This is the first demo of how any of this stuff works, starting on page 15. Here's our first Lagrangian, super simple.

```scheme
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))
```

Suppose we let \\(q\\) denote a coordinate path function that maps time to position components:

```scheme
(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))
```

\\(\Gamma\\) is a function that takes a coordinate path and returns a function of time that gives the local tuple.

The value \\(\Gamma\\) returns is called the "local tuple":

```scheme
(->tex-equation
 ((Gamma q) 't))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ x\left( t \right)} \cr \cr \displaystyle{ y\left( t \right)} \cr \cr \displaystyle{ z\left( t \right)}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ Dx\left( t \right)} \cr \cr \displaystyle{ Dy\left( t \right)} \cr \cr \displaystyle{ Dz\left( t \right)}\end{pmatrix}}\end{pmatrix}
\end{equation}

This is just \\((t, q(t), (Dq)(t), ....)\\) Where \\(D\\) is the derivative. (Preview: can a component of the coordinate path depend on the others? YES, and that would impose constraints beyond the degrees of freedom you'd guess by just counting the coordinates.)

Composing the Langrangian with \\(\Gamma\\) gives you a function that computes the Lagrangian at some instant:

```scheme
(->tex-equation
 ((compose (L-free-particle 'm) (Gamma q)) 't))
```

\begin{equation}
{{1}\over {2}} m {\left( Dz\left( t \right) \right)}^{2} + {{1}\over {2}} m {\left( Dy\left( t \right) \right)}^{2} + {{1}\over {2}} m {\left( Dx\left( t \right) \right)}^{2}
\end{equation}

This particular formula is written in terms of \\(x, y, z\\) coordinates, but that only came from the definition of \\(q\\). As we'll see later, you could write a coordinate transformation from some other totally different style of coordinates (called "generalized coordinates") and the Lagrangian would look different, but return the same value.

This function calculates the action \\(S[q](t\_1, t\_2)\\):

```scheme
(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))
```

Here's an example path that a particle might take, moving along a straight line as a function of \\(t\\).

```scheme
(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))
```

    #| test-path |#

Calculate the action for a particle of mass 3, between \\(t\_1 = 0\\) and \\(t\_2 = 10\\):

```scheme
(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
```

    #| 435. |#

This happens to be the minimal action, since the path we provided was a uniform path and the Lagrangian was for a free particle. If we'd provided a different path, we would still get an action. Just not a stationary action. Infinitesimal wiggles would change the action.

## Exercise 1.4: Lagrangian actions<a id="sec-1-4"></a>

> For a free particle an appropriate Lagrangian is

\begin{equation}
\label{eq:14lagrangian}
L(t, x, v) = {1 \over 2}mv^2
\end{equation}

> Suppose that x is the constant-velocity straight-line path of a free particle, such that \\(x\_a = x(t\_a)\\) and \\(x\_b = x(t\_b)\\). Show that the action on the solution path is

\begin{equation}
\label{eq:14result}
{m \over 2}{{(x\_b - x\_a)^2} \over {t\_b - t\_a}}
\end{equation}

The velocity is constant between the two points, so it must be equal to the difference in position over the difference in time:

\begin{equation}
\label{eq:constant-v}
v = {{x(t\_b) - x(t\_a)} \over {t\_b - t\_a}} = {{x\_b - x\_a} \over {t\_b - t\_a}}
\end{equation}

The action is equal to:

\begin{equation}
  \label{eq:2}
  \begin{aligned}
    S[q](t\_a, t\_b) & = \int\_{t\_a}^{t\_b} L(t, x, v) dx \cr
    & = \int\_{t\_a}^{t\_b} {1 \over 2}mv(t)^2 dx \cr
    & = {m \over 2}{v(t)^2 t} \Bigr|\_{t\_a}^{t\_b} \cr
    & = {m \over 2}{v(t\_b)^2 t\_b - v(t\_a)^2 t\_a}
  \end{aligned}
\end{equation}

The velocity is constant, so substitute in equation \eqref{eq:constant-v} and simplify:

\begin{equation}
  \label{eq:4}
  \begin{aligned}
    S[q](t\_a, t\_b) & = {m \over 2}{({{x\_b - x\_a} \over {t\_b - t\_a}})^2 (t\_b - t\_a)} \cr
    & = {m \over 2}{(x\_b - x\_a)^2 \over {t\_b - t\_a}}
  \end{aligned}
\end{equation}

As expected.

## Paths of Minimum Action<a id="sec-1-5"></a>

This section in the textbook implements path variation, so we can see the action change (and increase!) off of the optimal path.

`make-eta` returns a function that equals 0 at \\(t\_1\\) and \\(t\_2\\):

```scheme
(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))
```

Next, define a function that calculates the Lagrangian for a free particle, like before, but adds in the path variation multiplied by some small scaling factor \\(\epsilon\\).

```scheme
(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* epsilon eta))
                       t1
                       t2)))
```

The action for a small variation of \\(v(t) = (\sin(t), \cos(t), t^2)\\) is larger (top entry) vs the non-varied path (bottom entry), as expected.

```scheme
(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(let ((action-fn (varied-free-particle-action 3.0 test-path
                                              (up sin cos square)
                                              0.0 10.0)))
  (->tex-equation
   (up (action-fn 0.001)
       (action-fn 0))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 436.2912142857153} \cr \cr \displaystyle{ 435.}\end{pmatrix}
\end{equation}

What value of \\(\epsilon\\) minimizes the action for the test path?

Search over -2.0 to 1.0:

```scheme
(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (->tex-equation
   (minimize action-fn -2.0 1.0)))
```

\begin{equation}
5.134781488891349e-15\left( 435., 5 \right)
\end{equation}

The result shows that the minimum action occurs at \\(\epsilon = 0\\), up to numerical precision.

### Finding trajectories that minimize the action<a id="sec-1-5-1"></a>

Is it possible to use this principle to actually *find* a path, instead of simply checking it?

First we need a function that builds a path. This version generates a path of individual points, bracketed by the supplied start and end points \\((t\_0, q\_0)\\) and \\((t\_1, q\_1)\\). \\(qs\\) is a list of intermediate points.

```scheme
(define (make-path t0 q0 t1 q1 qs)
  (let ((n (length qs)))
    (let ((ts (linear-interpolants t0 t1 n)))
      (Lagrange-interpolation-function
       (append (list q0) qs (list q1))
       (append (list t0) ts (list t1))))))
```

This function sort-of-composes `make-path` and `Lagrangian-action`:

```scheme
(define ((parametric-path-action L t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action L path t0 t1)))
```

Finally, a function that generates a path that minimizes the action:

```scheme
(define (find-path L t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action L t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))
```

Let's test it out with a Lagrangian for a one dimensional harmonic oscillator with spring constant \\(k\\):

```scheme
(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))
```

    #| L-harmonic |#

```scheme
(define win2 (frame 0.0 :pi/2 0 1))

(define harmonic-path
  (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 3))

(plot-function win2 harmonic-path 0 :pi (/ :pi 100))
```

The path looks like a harmonic oscillator that starts high and bounces down, after \\(\pi \over 2\\) seconds, down to 0.

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_14-24-14_screenshot.png)

## Exercise 1.5: Solution process<a id="sec-1-6"></a>

The goal of this exercise is really just to watch the minimization process that they've given us.

> We can watch the progress of the minimization by modifying the procedure parametric-path-action to plot the path each time the action is computed.

The functions they've provided define a window, and then a version of `parametric-path-action` that updates the graph as it minimizes:

```scheme
(define win2 (frame 0.0 :pi/2 0.0 1.2))

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1)
         intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
    ;; display path
    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
    ;; compute action
    (Lagrangian-action Lagrangian path t0 t1)))
```

This final command runs the minimization and updates the graph as it goes.

```scheme
(find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 2)
```

The minimization looks like this:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-05-29_10-12-19_AJBpDgU.gif)

## Exercise 1.6: Minimizing action<a id="sec-1-7"></a>

The problem asks:

> Suppose we try to obtain a path by minimizing an action for an impossible problem. For example, suppose we have a free particle and we impose endpoint conditions on the velocities as well as the positions that are inconsistent with the particle being free. Does the formalism protect itself from such an unpleasant attack? You may find it illuminating to program it and see what happens.

I spent a good amount of time thinking about this one. When I attacked this book five years ago I found it very confusing. It makes more sense now that I've moved farther in the book and understand what it's asking us to do.

Let's say you take, as the authors suggest, some path, and impose velocity constraints on the endpoints in addition to the position constraints.

Usually, you constrain the coordinates at each endpoint and force a path that minimizes the action between two times. So what does it mean to impose velocity conditions?

The key is to realize that on the computer, you're forcing a path to be composed of a bunch of discrete points. If you can force a point into the path that is NOT controlled by the optimizer, then you can force a velocity at some point in the path that makes no sense for minimal action.

Let's define a new version of `parametric-path-action` that also takes an offset for the initial and final points. We'll force the first and last intermediate point to be equal to the start and end points, plus the offsets.

Then, we can try to find an action-minimizing path, but force the optimizer to deal with not just our endpoint conditions, but these two extra points as well. Forcing two points on each end will force an initial velocity condition.

Here's the implementation:

```scheme

(define (((parametric-path-action* win)
          Lagrangian t0 q0 offset0 t1 q1 offset1)
         intermediate-qs)
  (let ((intermediate-qs* (append (list (+ q0 offset0))
                                  intermediate-qs
                                  (list (+ q1 offset1)))))
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs*)))
      ;; display path
      (graphics-clear win)
      (plot-function win path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1))))
```

You could try a similar trick by modifying the first and last entries of `intermediate-qs` instead of appending a point, but I suspect that the optimizer would be able to figure out how to offset your offset.

Next, a version of `find-path` that passes the offsets through to the new `parametric-path-action*`:

```scheme
(define ((find-path* win) L t0 q0 offset0 t1 q1 offset1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let* ((action (parametric-path-action* win))
           (minimizing-qs
            (multidimensional-minimize
             (action L t0 q0 offset0 t1 q1 offset1)
             initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))
```

And finally, a function that can execute runs of our formalism-killing experiment.

```scheme
(define (one-six offset0 offset1 n)
  (let* ((tmax 10)
         (win (frame -1 (+ tmax 1) -0.2 (+ 1.2 offset0 offset1)))
         (find (find-path* win))
         (L (L-free-particle 3.0))
         (path (find L
                     0. 1. offset0
                     tmax 0. offset1
                     n)))
    (Lagrangian-action L path 0 tmax)))
```

`one-six` takes two offsets and runs the minimization routine against `L-free-particle`, moving from position 1 to 0 over 10 seconds. `n` controls the number of interpolation points that the system will use.

Internally, remember, `parametric-path-action*` will append two extra fixed offset points to the `n` intermediate points that the optimizer gets to control.

### Executions<a id="sec-1-7-1"></a>

Let's run the code with 0 offsets and 3 interpolation points. Note that this should *still* distort the path, since we now have two fixed points at the start and end. This is effectively imposing a 0 velocity constraint at the beginning and end.

Here's the code, and its output:

```scheme
(one-six 0 0 3)
```

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_15-10-46_ex1_6_nooffset.gif)

The path ends up looking almost sinusoidal, and takes a while to converge. This is the best polynomial that the system can come up with that matches the 7 points (3 interpolated, 2 offsets, 1 start and 1 end).

Here's a small positive velocity imposed at the beginning:

```scheme
(one-six 0.2 0 3)
```

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_15-10-53_ex1_6_02offset.gif)

The system takes longer to converge. Here's a larger impulse of 0.5:

```scheme
(one-six 0.5 0 3)
```

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_15-11-10_ex1_6_05offset.gif)

And a moderate negative velocity, just for fun:

```scheme
(one-six -0.5 0 3)
```

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_15-11-27_ex1_6_neg5offset.gif)

The process <span class="underline"><span class="underline">does</span></span> converge, but this is only because we only used 3 intermediate points. If you bump up to 10 points, with this code:

```scheme
(one-six 20 0 3)
```

The optimization freezes.

What is going on here? Why does the minimizer converge?

With velocity constraints imposed, we're no longer minimizing the action with respect to some Lagrangian. We're minimizing the action given two constraints. You have the Lagrangian, and then the warring goal of the polynomial interpolation forcing a certain shape on the path. At some point, the minimizer breaks; internally it ends up pinned between two tugging constraints.

If you make the impulse too big or force too many intermediate points, then the war is too hardcore and the process never converges. But it's important to note here the details of the optimizer. This detail doesn't break reality.

If you *do* need to impose velocity conditions, it turns out you can use a Lagrangian that takes acceleration into account. This is discussed in Exercise 1.10.

## Exercise 1.7: Properties of \\(\delta\\)<a id="sec-1-8"></a>

This exercise asks us to prove various products of the variation operator \\(\delta\_\eta\\). This is a sort of higher-order derivative operator. Apply it to a higher order function \\(f\\), and you'll get a function back that returns the *sensitivity* of \\(f\\) to fluctuations in its input path function. (Confusing? Check out [the textbook](https://tgvaughan.github.io/sicm/chapter001.html#h1-6a).

### Variation Product Rule<a id="sec-1-8-1"></a>

The product rule for variations states that:

\begin{equation}
\label{eq:var-prod}
\delta\_\eta (f g)[q] = \delta\_\eta f[q] g[q] + f[q] \delta\_\eta g[q]
\end{equation}

Write out the left side explicitly, using the definition of \\(\delta\_\eta\\):

\begin{equation}
\label{eq:var-prod-proof}
  \delta\_\eta (f g)[q] = \lim\_{\epsilon \to 0} \left( {f[q + \epsilon\eta]g[q + \epsilon\eta] - f[q]g[q]} \over \epsilon \right)
\end{equation}

Make the inspired move to add and subtract \\(f[q] g[q + \epsilon \eta]\\) inside the limit, rearrange and factor out the terms that have appeared in common. (Stare at this for a moment to make sure the steps are clear.)

\begin{equation}
\label{eq:var-prod-proof2}
\delta\_\eta (f g)[q] = \lim\_{\epsilon \to 0} \left( {g[q + \epsilon\eta](f[q + \epsilon\eta] - f[q])} \over \epsilon \right) + f[q] \lim\_{\epsilon \to 0} \left( {(g[q + \epsilon \eta] - g[q])} \over \epsilon \right)
\end{equation}

You might recognize that we've now isolated terms that look like \\(\delta\_\eta f[q]\\) and \\(\delta\_\eta g[q]\\), as \\(\epsilon\\) approaches 0. Notice that as this happens, \\(g[q + \epsilon\eta] \to g[q]\\), and the whole expression evaluates to the product rule we were seeking:

\begin{equation}
\label{eq:var-prod2}
\delta\_\eta (f g)[q] = \delta\_\eta f[q]\,g[q] + f[q]\,\delta\_\eta g[q]
\end{equation}

### Variation Sum Rule<a id="sec-1-8-2"></a>

The sum rule is easier. Our goal is:

\begin{equation}
\label{eq:var-sum}
\delta\_\eta (f + g)[q] = \delta\_\eta f[q] + \delta\_\eta g[q]
\end{equation}

Expand out the definition of the variation operator, regroup terms, allow \\(\epsilon \to 0\\) and notice that we've recovered our goal.

\begin{equation}
\label{eq:var-sum-proof}
\begin{aligned}
  \delta\_\eta (f + g)[q] & = \lim\_{\epsilon \to 0} \left( {(f[q + \epsilon\eta] + g[q + \epsilon\eta]) - (f[q] + g[q])} \over \epsilon \right) \cr
  & = \lim\_{\epsilon \to 0} \left( {f[q + \epsilon\eta] - f[q]} \over \epsilon \right) + \lim\_{\epsilon \to 0} \left( {g[q + \epsilon\eta] - g[q]} \over \epsilon \right) \cr
  & = \delta\_\eta f[q] + \delta\_\eta g[q]
\end{aligned}
\end{equation}

Done!

### Variation Scalar Multiplication<a id="sec-1-8-3"></a>

We want to show that \\(\delta\_\eta\\) preserves multiplication by a scalar \\(c\\):

\begin{equation}
\label{eq:var-scalar}
\delta\_\eta (c g)[q] = c \delta\_\eta g[q]
\end{equation}

Expand out the definition of the variation operator:

\begin{equation}
\label{eq:var-scalar-proof}
\begin{aligned}
  \delta\_\eta (c g)[q] & = \lim\_{\epsilon \to 0} \left( {c f[q + \epsilon\eta] - c f[q]} \over \epsilon \right) \cr
  & = c \lim\_{\epsilon \to 0} \left( {f[q + \epsilon\eta] - f[q]} \over \epsilon \right) \cr
  & = c \delta\_\eta f[q]
\end{aligned}
\end{equation}

Done, since the limit operator preserves scalar multiplication.

### Chain Rule for Variations<a id="sec-1-8-4"></a>

The chain rule for variations states that:

\begin{equation}
\label{eq:var-chain}
\delta\_\eta h[q] = (DF \circ g[q])\, \delta\_\eta g[q] \textrm{ with } h[q] = F \circ g[q]
\end{equation}

Expand this out using the definition of \\(\delta\_\eta\\):

\begin{equation}
\label{eq:var-chain-proof}
  \delta\_\eta (F \circ g[q]) = \lim\_{\epsilon \to 0} \left( {(F \circ g[q + \epsilon\eta]) - (F \circ g[q])} \over \epsilon \right)
\end{equation}

Now multiply the term inside the limit by \\(1 = {{g[q + \epsilon\eta] - g[q]} \over {g[q + \epsilon\eta] - g[q]}}\\) and factor out the new, more recognizable product that forms:

\begin{equation}
\label{eq:var-chain-proof2}
\begin{aligned}
  \delta\_\eta (F \circ g[q]) & = \lim\_{\epsilon \to 0} \left( {((F \circ g[q + \epsilon\eta]) - (F \circ g[q]))({g[q + \epsilon\eta] - g[q]})} \over {({g[q + \epsilon\eta] - g[q]}) \epsilon} \right) \cr
  & = \lim\_{\epsilon \to 0} \left( {(F \circ g[q + \epsilon\eta]) - (F \circ g[q])} \over {g[q + \epsilon\eta] - g[q]} \right) \delta\_\eta g[q]
\end{aligned}
\end{equation}

The remaining term inside the limit has the form of a derivative of some function \\(f\\) evaluated at a point \\(a\\).

\begin{equation}
\label{eq:var-chain-proof3}
Df(a) = \lim\_{b \to a} \left( {f(b) - f(a)} \over {b - a} \right)
\end{equation}

Where \\(b = g[q + \epsilon \eta]\\) and \\(a = g[q]\\). As \\(\epsilon \to 0\\), \\(F \circ g[q + \epsilon \eta] \to F \circ g[q]\\). We know this because we showed that \\(\delta\_\eta g[q]\\) exists and factored it out.

Remember that that this is all function algebra, so composition here is analogous to function application; so \\(F\\) is indeed the \\(f\\) in equation \eqref{eq:var-chain-proof3}, and the remaining term collapses to \\(DF\\) evaluated at \\(a = g[q]\\):

\begin{equation}
\label{eq:var-chain-proof4}
  \delta\_\eta (F \circ g[q]) = (DF \circ g[q])\, \delta\_\eta g[q]
\end{equation}

### \\(\delta\_\eta\\) commutes with \\(D\\)<a id="sec-1-8-5"></a>

We need to show the derivative can commute with a normal derivative of the function that \\(f\\) returns after it's passed a path:

\begin{equation}
\label{eq:var-commute}
D \delta\_\eta f[q] = \delta\_\eta g[q] \textrm{ with } g[q] = D(f[q])
\end{equation}

Expand the left side by the definition of \\(\delta\_\eta\\):

\begin{equation}
\label{eq:var-commute-proof}
  D (\delta\_\eta f[q]) = D \lim\_{\epsilon \to 0} \left( {(f[q + \epsilon\eta]) - (f[q])} \over \epsilon \right)
\end{equation}

The derivative \\(D\\) is a linear operator, so we can move it in to the limit and distribute it over subtraction:

\begin{equation}
\label{eq:var-commute-proof2}
\begin{aligned}
  D (\delta\_\eta f[q]) & = \lim\_{\epsilon \to 0} \left( {D(f[q + \epsilon\eta]) - D(f[q])} \over \epsilon \right) \cr
  & = \delta\_\eta(D(f[q]))
\end{aligned}
\end{equation}

Our goal is achieved.

## Exercise 1.8: Implementation of \\(\delta\\)<a id="sec-1-9"></a>

### Part A: Implement \\(\delta\_\eta\\)<a id="sec-1-9-1"></a>

The goal here is to implement \\(\delta\_\eta\\) as a procedure. Explicitly:

> Suppose we have a procedure `f` that implements a path-dependent function: for path `q` and time `t` it has the value `((f q) t)`. The procedure delta computes the variation \\(\delta\_\eta f[q](t)\\) as the value of the expression `((((delta eta) f) q) t)`. Complete the definition of `delta`:

After laboriously proving all of the properties above, the actual implementation feels so simple.

The key is equation 1.22 in the book:

\begin{equation}
\label{eq:1-22}
\delta\_\eta f[q] = \lim\_{\epsilon \to 0} \left( {g(\epsilon) - g(0)} \over \epsilon \right) = Dg(0)
\end{equation}

Given \\(g(\epsilon) = f[q + \epsilon \eta]\\). Through the magic of automatic differentiation we can simply write:

```scheme
(define (((delta eta) f) q)
  (let ((g (lambda (eps)
             (f (+ q (* eps eta))))))
    ((D g) 0)))
```

It's almost spooky, that \\(D\\) can somehow figure out what to do here.

### Part B: Check \\(\delta\_\eta\\)'s properties<a id="sec-1-9-2"></a>

Part B's problem description gave us a path-dependent function similar to this one:

```scheme
(define ((fn sym) q)
  (let* ((Local (UP Real (UP* Real) (UP* Real)))
         (F (literal-function sym (-> Local Real))))
    (compose F (Gamma q))))
```

I've modified it slightly to take in a symbol, since we'll need to generate multiple functions for a few of the rules.

\\(fn\\) takes a symbol like \\(F\\) and a path function - a function from \\(t\\) to any number of coordinates (see the `UP*`?) - and returns a generic expression for a path dependent function \\(F\\) that acts via \\(F \circ \Gamma[q]\\). \\(F\\) might be a Lagrangian, for example.

The textbook also gives us this function from \\(t \to (x, y)\\) to test out the properties above. I've added an \\(\eta\\) of the same type signature that we can use to add variation to the path.

```scheme
(define q (literal-function 'q (-> Real (UP Real Real))))
(define eta (literal-function 'q (-> Real (UP Real Real))))
```

1.  Variation Product Rule

    Equation \eqref{eq:var-prod} states the product rule for variations. Here it is in code. I've implemented the right and left sides and subtracted them. As expected, the result is 0:

    ```scheme
    (let* ((f (fn 'f))
           (g (fn 'g))
           (de (delta eta)))
      (let ((left ((de (* f g)) q))
            (right (+ (* (g q) ((de f) q))
                      (* (f q) ((de g) q)))))
        (->tex-equation
         ((- left right) 't))))
    ```

    \begin{equation}
    0
    \end{equation}

2.  Variation Sum Rule

    The sum rule is similar. Here's the Scheme implementation of equation \eqref{eq:var-sum}:

    ```scheme
    (let* ((f (fn 'f))
           (g (fn 'g))
           (de (delta eta)))
      (let ((left ((de (+ f g)) q))
            (right (+ ((de f) q)
                      ((de g) q))))
        (->tex-equation
         ((- left right) 't))))
    ```

    \begin{equation}
    0
    \end{equation}

3.  Variation Scalar Multiplication

    Here's equation \eqref{eq:var-scalar} in code. The sides are equal, so their difference is 0:

    ```scheme
    (let* ((g (fn 'g))
           (de (delta eta)))
      (let ((left ((de (* 'c g)) q))
            (right (* 'c ((de g) q))))
        (->tex-equation
         ((- left right) 't))))
    ```

    \begin{equation}
    0
    \end{equation}

4.  Chain Rule for Variations

    To compute the chain rule we'll need a version of `fn` that takes the derivative of the inner function:

    ```scheme
    (define ((Dfn sym) q)
      (let* ((Local (UP Real (UP* Real) (UP* Real)))
             (F (literal-function sym (-> Local Real))))
        (compose (D F) (Gamma q))))
    ```

    For the Scheme implementation, remember that both `fn` and `Dfn` have \\(\Gamma\\) baked in. The \\(g\\) in equation \eqref{eq:var-chain} is hardcoded to \\(\Gamma\\) in the function below.

    Here's a check that the two sides of equation \eqref{eq:var-chain} are equal:

    ```scheme
    (let* ((h (fn 'F))
           (dh (Dfn 'F))
           (de (delta eta)))
      (let ((left (de h))
            (right (* dh (de Gamma))))
        (->tex-equation
         (((- left right) q) 't))))
    ```

    \begin{equation}
    0
    \end{equation}

5.  \\(\delta\_\eta\\) commutes with \\(D\\)

    Our final test. Here's equation \eqref{eq:var-commute} in code, showing that the derivative commutes with the variation operator:

    ```scheme
    (let* ((f (fn 'f))
           (g (compose D f))
           (de (delta eta)))
      (let ((left (D ((de f) q)))
            (right ((de g) q)))
        (->tex-equation
         ((- left right) 't))))
    ```

    \begin{equation}
    0
    \end{equation}

## Exercise 1.9: Lagrange's equations<a id="sec-1-10"></a>

This exercise has us deriving Lagrange's equations in steps for three different systems.

You should do this on paper, then go look at Exercise 1.12 for the Scheme code that implements each Lagrangian and shows the steps required to get to Lagrange's equations.

## Exercise 1.10: Higher-derivative Lagrangians<a id="sec-1-11"></a>

Exercise:

> Derive Lagrange's equations for Lagrangians that depend on accelerations. In particular, show that the Lagrange equations for Lagrangians of the form \\(L(t, q, \dot{q}, \ddot{q})\\) with \\(\ddot{q}\\) terms are
>
> \begin{equation}
>   D^2(\partial\_3L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]) + (\partial\_1L \circ \Gamma[q]) = 0
> \end{equation}

Start with the action:

\begin{equation}
  \begin{aligned}
    S[q](t\_a, t\_b) & = \int\_{t\_a}^{t\_b} L(t, x(t), v(t), a(t)) dx \cr
    & = \int\_{t\_a}^{t\_b} (L \circ \Gamma[q])
  \end{aligned}
\end{equation}

apply the variation operator, \\(\delta\_\eta\\):

\begin{equation}
  \delta\_\eta S[q](t\_a, t\_b) = \int\_{t\_a}^{t\_b} \delta\_\eta (L \circ \Gamma[q])
\end{equation}

Expand the right side out the chain rule, equation \eqref{eq:var-chain}:

\begin{equation}
  \int\_{t\_a}^{t\_b} \delta\_\eta (L \circ \Gamma[q])  =\int\_{t\_a}^{t\_b} ((DL) \circ \Gamma[q]) \delta\_\eta\Gamma[q]
\end{equation}

From equations 1.20 and 1.21 in the book, we know that

\begin{equation}
\delta\_\eta\Gamma[q] = (0, \eta, D\eta, D^2\eta, \ldots)
\end{equation}

Expand the chain rule up to the $n$th derivative of the coordinate:

\begin{equation}
  \begin{aligned}
    \int\_{t\_a}^{t\_b} & ((DL) \circ \Gamma[q]) \delta\_\eta\Gamma[q] = \cr
    & \int\_{t\_a}^{t\_b} 0 + (\partial\_1L \circ \Gamma[q])\eta + (\partial\_2L \circ \Gamma[q])D\eta + \ldots + (\partial\_{n + 1}L \circ \Gamma[q])D^n\eta
  \end{aligned}
\end{equation}

Our goal now is to isolate the Lagrange equations. Focus on the \\(\partial\_2 L\\) term:

\begin{equation}
  \int\_{t\_a}^{t\_b} (\partial\_2L \circ \Gamma[q])D\eta
\end{equation}

Integrate by parts:

\begin{equation}
  \int\_{t\_a}^{t\_b} (\partial\_2L \circ \Gamma[q])D\eta = (\partial\_2L \circ \Gamma[q])\eta \Bigr|\_{t\_a}^{t\_b} - \int\_{t\_a}^{t\_b} D(\partial\_2L \circ \Gamma[q])\eta
\end{equation}

The first of the two terms disappears, since, by definition, \\(\eta(t\_a) = \eta(t\_b) = 0\\), leaving us with:

\begin{equation}
  \int\_{t\_a}^{t\_b} (\partial\_2L \circ \Gamma[q])D\eta = \int\_{t\_a}^{t\_b} D(\partial\_2L \circ \Gamma[q])\eta
\end{equation}

And reducing the full equation to:

\begin{equation}
  \begin{aligned}
    \int\_{t\_a}^{t\_b} & ((DL) \circ \Gamma[q]) \delta\_\eta\Gamma[q] = \cr
    & \int\_{t\_a}^{t\_b} ((\partial\_1L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]))\eta + (\partial\_3L \circ \Gamma[q])D^2\eta \cr
    & + \ldots + (\partial\_{n + 1}L \circ \Gamma[q])D^n\eta
  \end{aligned}
\end{equation}

The original Lagrange equations are peeking at us.

We got there by isolating a term multiplied by \\(\eta\\). To do this with the next term, integrate by parts twice:

\begin{equation}
  \begin{aligned}
    \int\_{t\_a}^{t\_b} (\partial\_3L \circ \Gamma[q])D^2\eta & = (\partial\_3L \circ \Gamma[q])D\eta \Bigr|\_{t\_a}^{t\_b} - \int\_{t\_a}^{t\_b} D(\partial\_2L \circ \Gamma[q])D\eta \cr
    & = (\partial\_3L \circ \Gamma[q])D\eta \Bigr|\_{t\_a}^{t\_b} - D(\partial\_2L \circ \Gamma[q])\eta \Bigr|\_{t\_a}^{t\_b} + \int\_{t\_a}^{t\_b} D^2(\partial\_2L \circ \Gamma[q])\eta
  \end{aligned}
\end{equation}

The second of the two definite evaluations disappears, since, as before, \\(\eta(t\_a) = \eta(t\_b) = 0\\). The first of the definite evaluations suggests that we need a new constraint to achieve higher-derivative Lagrange equations.

If \\(D\eta(t\_a) = D\eta(t\_b) = 0\\), then the first definite evaluation disappears as well. So, for a Lagrangian that considers acceleration, we have to impose the constraint that that the endpoint velocities stay fixed. The term collapses to:

\begin{equation}
  \begin{aligned}
    \int\_{t\_a}^{t\_b} (\partial\_3L \circ \Gamma[q])D^2\eta & = (\partial\_3L \circ \Gamma[q])D\eta \Bigr|\_{t\_a}^{t\_b} - D(\partial\_2L \circ \Gamma[q])\eta \Bigr|\_{t\_a}^{t\_b} + \int\_{t\_a}^{t\_b} D^2(\partial\_2L \circ \Gamma[q])\eta \cr
    & = \int\_{t\_a}^{t\_b} D^2(\partial\_2L \circ \Gamma[q])\eta
  \end{aligned}
\end{equation}

Fold this back in to the full equation:

\begin{equation}
  \begin{aligned}
    \int\_{t\_a}^{t\_b} & ((DL) \circ \Gamma[q]) \delta\_\eta\Gamma[q] = \cr
    & \int\_{t\_a}^{t\_b} ((\partial\_1L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]) + D^2(\partial\_3L \circ \Gamma[q]))\eta \cr
    & + \ldots + (\partial\_{n + 1}L \circ \Gamma[q])D^n\eta
  \end{aligned}
\end{equation}

For a Lagrangian of the form \\(L(t, q, \dot{q}, \ddot{q})\\), the remaining terms disappear, leaving us with

\begin{equation}
  \begin{aligned}
    \delta\_\eta S[q](t\_a, t\_b) & = \int\_{t\_a}^{t\_b} ((DL) \circ \Gamma[q]) \delta\_\eta\Gamma[q] \cr
    & = \int\_{t\_a}^{t\_b} ((\partial\_1L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]) + D^2(\partial\_3L \circ \Gamma[q]))\eta
  \end{aligned}
\end{equation}

The goal was to find conditions under which the action is stationary, ie, \\(\delta\_\eta S[q](t\_a, t\_b) = 0\\). For arbitrary \\(\eta\\) with fixed endpoints, this can only occur if the non-\\(\eta\\) inside the integral is 0:

\begin{equation}
(\partial\_1L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]) + D^2(\partial\_3L \circ \Gamma[q]) = 0
\end{equation}

This is the result we were seeking.

### Higher dimensions<a id="sec-1-11-1"></a>

To keep going, we have to integrate by parts once more for each new term of the local tuple that the Lagrangian depends on. For each new term we'd find a new constraint:

\begin{equation}
D^{n-1}\eta(t\_a) = D^{n-1}\eta(t\_b) = 0
\end{equation}

And a new term in Lagrange's equations:

\begin{equation}
  (-1)^{n} D^{n}(\partial\_{n+1}L \circ \Gamma[q])
\end{equation}

The fully general Lagrange's equations are, for a Lagrangian that depends on the local tuple up to the $n$th derivative of \\(q\\), are:

\begin{equation}
  0 = \sum\_{i = 0}^n(-1)^i D^{i}(\partial\_{i+1}L \circ \Gamma[q])
\end{equation}

Constrained by, for all \\(i\\) from 0 to \\(n-1\\):

\begin{equation}
D^i\eta(t\_a) = D^i\eta(t\_b) = 0
\end{equation}

Equivalently, the constraint is that all derivatives of \\(q\\) from \\(i\\) to \\(n-1\\) must remain constant at \\(t\_a\\) and \\(t\_b\\).

Exercise 1.13 implements this in Scheme.

## Exercise 1.11: Kepler's third law<a id="sec-1-12"></a>

Lagrangian from the problem:

```scheme
(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))     (phi (ref q 1))
                            (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot) (square (* r phidot))) )
         (V r)))))
```

Helper function, plus a Scheme version of the reduced mass definition:

```scheme
(define ((gravitational-energy G m1 m2) r)
  (- (/ (* G m1 m2) r)))

(define (reduced-mass m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))
```

The Lagrangian is written in terms of some angle \\(\phi\\) and \\(r\\), the distance between the two particles. \\(q\\) defines a circular path:

```scheme
(define ((q r omega) t)
  (let ((phi (* omega t)))
    (up r phi)))
```

Let's write the Lagrange equations, given \\(r = a\\) and \\(\omega = n\\):

```scheme
(let ((eqfn (Lagrange-equations
             (L-central-polar (reduced-mass 'm1 'm2)
                              (gravitational-energy 'G 'm1 'm2)))))
  (->tex-equation
   ((eqfn (q 'a 'n)) 't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ {{ - {a}^{3} \cdot m1 \cdot m2 \cdot {n}^{2} + G {m1}^{2} \cdot m2 + G \cdot m1 \cdot {m2}^{2}}\over {{a}^{2} \cdot m1 + {a}^{2} \cdot m2}}} \cr \cr \displaystyle{ 0}\end{bmatrix}
\end{equation}

If you fiddle with this you should see that you can factor out the reduced mass and an \\(a^2\\) term. The whole equations is zero, so it's fine to multiply the residual by any non-zero factor you like.

```scheme
(let ((eqfn (Lagrange-equations
             (L-central-polar (reduced-mass 'm1 'm2)
                              (gravitational-energy 'G 'm1 'm2)))))
  (->tex-equation
   (* ((eqfn (q 'a 'n)) 't)
      (/ (square 'a)
         (reduced-mass 'm1 'm2)))))
```

\begin{equation}
\begin{bmatrix} \displaystyle{  - {a}^{3} {n}^{2} + G \cdot m1 + G \cdot m2} \cr \cr \displaystyle{ 0}\end{bmatrix}
\end{equation}

And, boom, there in the first position we see Kepler's third law:

\begin{equation}
\label{eq:kepler3}
n^2a^3 = G(m\_1 + m\_2)
\end{equation}

## Exercise 1.12: Lagrange's equations (code)<a id="sec-1-13"></a>

This exercise has us writing code for the three systems described in [Exercise 1.9](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-9). Before we start, here's a function that will display an up-tuple of:

-   \\(\partial\_1 L \circ \Gamma[q]\\), the generalized force
-   \\(\partial\_2 L \circ \Gamma[q]\\), the generalized momenta
-   \\(D(\partial\_2 L \circ \Gamma[q])\\), the derivative of our momenta
-   The Lagrange equations for the system.

```scheme
(define (lagrange-equation-steps L q)
  (let* ((p1 (compose ((partial 1) L) (Gamma q)))
         (p2 (compose ((partial 2) L) (Gamma q)))
         (dp2 (D p2)))
    (->tex-equation
     ((up p1 p2 dp2 (- dp2 p1))
      't))))
```

We'll call this for each system to show the steps required to derive Lagrange's equations.

### Part A: Ideal Planar Pendulum<a id="sec-1-13-1"></a>

From the book:

> An ideal planar pendulum consists of a bob of mass \\(m\\) connected to a pivot by a massless rod of length \\(l\\) subject to uniform gravitational acceleration \\(g\\). A Lagrangian is \\(L(t, \theta, \dot{\theta}) = {1 \over 2} ml^2\dot{\theta}^2 + mgl\cos \theta\\). The formal parameters of \\(L\\) are \\(t\\), \\(\theta\\), and \\(\dot{\theta}\\); \\(\theta\\) measures the angle of the pendulum rod to a plumb line and \\(\dot{\theta}\\) is the angular velocity of the rod.

Here is the Lagrangian described above:

```scheme
(define ((L-pendulum m g l) local)
  (let ((theta (coordinate local))
        (theta_dot (velocity local)))
    (- (* 1/2 m (square l) (square theta_dot))
       (* m g l (cos theta)))))
```

And the steps that lead us to Lagrange's equations:

```scheme
(lagrange-equation-steps
 (L-pendulum 'm 'g 'l)
 (literal-function 'theta))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ g l m \sin\left( \theta\left( t \right) \right)} \cr \cr \displaystyle{ {l}^{2} m D\theta\left( t \right)} \cr \cr \displaystyle{ {l}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{  - g l m \sin\left( \theta\left( t \right) \right) + {l}^{2} m {D}^{2}\theta\left( t \right)}\end{pmatrix}
\end{equation}

The final Lagrange equation needs some hand simplification. Divide out \\(g\\) and \\(l\\), and include a factor of \\(-1\\) to make things look nice:

```scheme
(let* ((L (L-pendulum 'm 'g 'l))
       (theta (literal-function 'theta))
       (eq ((Lagrange-equations L) theta)))
  (->tex-equation
   ((/ eq (* -1 'm 'l))
    't)))
```

\begin{equation}
g \sin\left( \theta\left( t \right) \right) - l {D}^{2}\theta\left( t \right)
\end{equation}

Much better.

### Part B: 2D Potential<a id="sec-1-13-2"></a>

From the book:

> A particle of mass \\(m\\) moves in a two-dimensional potential \\(V(x, y) = {(x^2 + y^2) \over 2} + x^2 y - {y^3 \over 3}\\), where \\(x\\) and \\(y\\) are rectangular coordinates of the particle. A Lagrangian is \\(L(t;x, y; v\_x, v\_y) = {1 \over 2} m (v\_x^2 + v\_y^2) - V(x, y)\\).

Define the potential \\(V\\) and the Lagrangian separately. The Lagrangian here is actually written in a form general enough that it would work for any number of dimensions in rectangular space, given some potential \\(V\\). This is something to note for later, when we discuss coordinate transformations.

```scheme
(define (V q)
  (let ((x (ref q 0))
        (y (ref q 1)))
    (- (+ (/ (+ (square x)
                (square y))
             2)
          (* (square x) y))
       (/ (cube y) 3))))

(define (((L-2d-potential m) V) local)
  (- (* 1/2 m (square (velocity local)))
     (V (coordinate local))))
```

Next, the derivation of the Lagrange equations:

```scheme
(lagrange-equation-steps
 ((L-2d-potential 'm) V)
 (up (literal-function 'x)
     (literal-function 'y)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{  - 2 y\left( t \right) x\left( t \right) - x\left( t \right)} \cr \cr \displaystyle{ {\left( y\left( t \right) \right)}^{2} - {\left( x\left( t \right) \right)}^{2} - y\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ m Dx\left( t \right)} \cr \cr \displaystyle{ m Dy\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ m {D}^{2}x\left( t \right)} \cr \cr \displaystyle{ m {D}^{2}y\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ m {D}^{2}x\left( t \right) + 2 y\left( t \right) x\left( t \right) + x\left( t \right)} \cr \cr \displaystyle{ m {D}^{2}y\left( t \right) - {\left( y\left( t \right) \right)}^{2} + {\left( x\left( t \right) \right)}^{2} + y\left( t \right)}\end{bmatrix}}\end{pmatrix}
\end{equation}

The final down-tuple gives us the Lagrange equations for \\(x\\) and \\(y\\) respectively.

### Part C: Particle on a Sphere<a id="sec-1-13-3"></a>

From the book:

> A Lagrangian for a particle of mass \\(m\\) constrained to move on a sphere of radius \\(R\\) is \\(L(t; \theta, \phi; \alpha, \beta) = {1 \over 2} m R^2(\alpha^2+(\beta \sin\theta)^2)\\). The angle \\(\theta\\) is the colatitude of the particle and \\(\phi\\) is the longitude; the rate of change of the colatitude is \\(\alpha\\) and the rate of change of the longitude is \\(\beta\\).

Here is the Lagrangian:

```scheme
(define ((L-sphere m R) local)
  (let* ((q (coordinate local))
         (qdot (velocity local))
         (theta (ref q 0))
         (alpha (ref qdot 0))
         (beta (ref qdot 1)))
    (* 1/2 m (square R)
       (+ (square alpha)
          (square (* beta (sin theta)))))))
```

The final Lagrange equations have a few terms that we can cancel out. Scheme doesn't know that these are meant to be residuals, so it won't cancel out factors that we can see by eye are missing. Here is the full derivation:

```scheme
(lagrange-equation-steps
 (L-sphere 'm 'R)
 (up (literal-function 'theta)
     (literal-function 'phi)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2}} \cr \cr \displaystyle{ 0}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m D\theta\left( t \right)} \cr \cr \displaystyle{ {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} D\phi\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {R}^{2} m \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\phi\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{  - {R}^{2} m \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + {R}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {R}^{2} m \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\phi\left( t \right)}\end{bmatrix}}\end{pmatrix}
\end{equation}

Next, get the Lagrange equations in hand and manually simplify each equation by dividing out, respectively, \\(mR^2\\) and \\(mR^2 \sin \theta\\):

```scheme
(let* ((L (L-sphere 'm 'R))
       (theta (literal-function 'theta))
       (q (up theta (literal-function 'phi)))
       (le ((Lagrange-equations L) q)))
  (let ((eq1 (ref le 0))
        (eq2 (ref le 1)))
    (->tex-equation
     ((up (/ eq1 (* 'm (square 'R)))
          (/ eq2 (* (sin theta) 'm (square 'R))))
      't))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{  - \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + \sin\left( \theta\left( t \right) \right) {D}^{2}\phi\left( t \right)}\end{pmatrix}
\end{equation}

Looking good. These are the residuals for \\(\theta\\) and \\(\phi\\), respectively.

## Exercise 1.13: Higher-derivative Lagrangians (code)<a id="sec-1-14"></a>

### Part A: Acceleration-dependent Lagrangian Implementation<a id="sec-1-14-1"></a>

Here we go:

```scheme
(define ((Lagrange-equations3 L) q)
  (let ((state-path (Gamma q 4)))
    (+ ((square D) (compose ((partial 3) L) state-path))
       (- (D (compose ((partial 2) L) state-path)))
       (compose ((partial 1) L) state-path))))
```

### Part B: Applying HO-Lagrangians<a id="sec-1-14-2"></a>

Lagrangian from the problem:

```scheme
(define ((L-1-13 m k) local)
  (let ((x (coordinate local))
        (a (acceleration local)))
    (- (* -1/2 m x a)
       (* 1/2 k (square x)))))
```

Testing, with a factor of \\(-1\\) to make it look nice:

```scheme
(->tex-equation
 (- (((Lagrange-equations3 (L-1-13 'm 'k))
      (literal-function 'x)) 't)))
```

\begin{equation}
k x\left( t \right) + m {D}^{2}x\left( t \right)
\end{equation}

### Part C: Generalized Lagrange Equations<a id="sec-1-14-3"></a>

The more general version:

```scheme
;; Returns n copies of elems appended to each other.
(define (cycle n elems)
  (apply append (make-list n elems)))

;; Returns n total elements generated from an infinite cycle of elems.
(define (alternating n elems)
  (let* ((l (length elems))
         (times (quotient (+ n (-1+ l)) l)))
    (list-head (cycle times elems) n)))

(define ((Lagrange-equations* L n) q)
  (let ((state-path (Gamma q (1+ n))))
    (define (term i)
      ((expt D i)
       (compose ((partial (1+ i)) L) state-path)))
    (let ((terms (map term (iota n))))
      (fold-left (lambda (acc pair)
                   (+ acc (apply * pair)))
                 0
                 (zip (alternating n '(1 -1))
                      (reverse terms))))))
```

Check that we get the same result:

```scheme
(->tex-equation
 (- (((Lagrange-equations* (L-1-13 'm 'k) 3)
      (literal-function 'x)) 't)))
```

\begin{equation}
k x\left( t \right) + m {D}^{2}x\left( t \right)
\end{equation}

This looks like a harmonic oscillator.

## Exercise 1.14: Coordinate-independence of Lagrange equations<a id="sec-1-15"></a>

This exercise is building up to showing that the coordinate transformation is *always* associative; you can always generate a new Lagrangian by composing it with a coordinate transformation.

We're trying to show that the rectangular equations of motion are the same as the polar. Here are the two Lagrangians from the book:

```scheme
(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))

(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
                        (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot)
               (square (* r phidot))) )
         (U r)))))
```

Here are the rectangular equations of motion:

```scheme
(->tex-equation
 (((Lagrange-equations
    (L-central-rectangular 'm (literal-function 'U)))
   (up (literal-function 'x)
       (literal-function 'y)))
  't)
 "eq:rect-equations")
```

\begin{equation}
\begin{bmatrix} \displaystyle{ {{m {D}^{2}x\left( t \right) \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} + x\left( t \right) DU\left( \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} \right)}\over {\sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}}}}} \cr \cr \displaystyle{ {{m \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} {D}^{2}y\left( t \right) + y\left( t \right) DU\left( \sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}} \right)}\over {\sqrt{{\left( x\left( t \right) \right)}^{2} + {\left( y\left( t \right) \right)}^{2}}}}}\end{bmatrix}
\label{eq:rect-equations}
\end{equation}

And here are the polar equations:

```scheme
(->tex-equation
  (((Lagrange-equations
      (L-central-polar 'm (literal-function 'U)))
    (up (literal-function 'r)
        (literal-function 'phi)))
   't))
```

\begin{equation}
\begin{bmatrix} \displaystyle{  - m r\left( t \right) {\left( D\phi\left( t \right) \right)}^{2} + m {D}^{2}r\left( t \right) + DU\left( r\left( t \right) \right)} \cr \cr \displaystyle{ m {D}^{2}\phi\left( t \right) {\left( r\left( t \right) \right)}^{2} + 2 m r\left( t \right) D\phi\left( t \right) Dr\left( t \right)}\end{bmatrix}
\end{equation}

The goal is to show that, if you can write down coordinate transformations for the coordinates, velocities and accelerations, the equations of motions will turn out to be equivalent.

They want you to do this on paper. We'll show how to accomplish this by writing down expressions for the rectangular coordinates in terms of polar coordinates and substituting those in to the rectangular equations of motion. We should find that the polar equations pop out.

Take the coordinate transformation described in 1.64 in the book:

\begin{equation}
  \begin{split}
    x &= r \cos \phi \cr
    y &= r \sin \phi
  \end{split}
\end{equation}

Note that \\(x\\), \\(y\\), \\(r\\) and \\(\phi\\) are functions of \\(t\\). Take the derivative of each equation (Use the product and chain rules!) to obtain expressions for the rectangular velocities in terms of the polar coordinates, just like equation 1.66 in the book:

\begin{equation}
  \begin{split}
    Dx(t) &= Dr(t) \cos \phi(t) - r(t) D\phi(t) \sin \phi(t) \cr
    Dy(t) &= Dr(t) \sin \phi(t) + r(t) D\phi(t) \cos \phi(t)
  \end{split}
\end{equation}

The rectangular equations of motion have second derivatives, so we need to keep going. This is too devastating to imagine doing by hand. Let's move to Scheme.

Write the coordinate transformation for polar coordinates to rectangular:

```scheme
(define (p->r local)
  (let* ((polar-tuple (coordinate local))
         (r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))
```

Then use `F->C`, first described on page 46. This is a function that takes some function like `p->r` that converts coordinates, and returns a *new* function that can convert an entire local tuple from one coordinate system to another.

The version that the book presents on page 46 can only transform the velocity component, but `scmutils` contains a more general version that will convert as many path elements as you pass to it.

Here are the rectangular positions, velocities and accelerations, written in polar coordinates:

```scheme
(let ((convert-path (F->C p->r))
      (polar-path (up 't
                      (up 'r 'phi)
                      (up 'rdot 'phidot)
                      (up 'rdotdot 'phidotdot))))
  (->tex-equation
   (convert-path polar-path)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ r \cos\left( \phi \right)} \cr \cr \displaystyle{ r \sin\left( \phi \right)}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{  - \dot{\phi} r \sin\left( \phi \right) + \dot{r} \cos\left( \phi \right)} \cr \cr \displaystyle{ \dot{\phi} r \cos\left( \phi \right) + \dot{r} \sin\left( \phi \right)}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{  - {\dot{\phi}}^{2} r \cos\left( \phi \right) - 2 \dot{\phi} \dot{r} \sin\left( \phi \right) - \ddot{\phi} r \sin\left( \phi \right) + \ddot{r} \cos\left( \phi \right)} \cr \cr \displaystyle{  - {\dot{\phi}}^{2} r \sin\left( \phi \right) + 2 \dot{\phi} \dot{r} \cos\left( \phi \right) + \ddot{\phi} r \cos\left( \phi \right) + \ddot{r} \sin\left( \phi \right)}\end{pmatrix}}\end{pmatrix}
\end{equation}

Ordinarily, it would be too heartbreaking to substitute these in to the rectangular equations of motion&#x2026; but we have Scheme to help.

This function generates the equations of motion as a function of the local tuple:

```scheme
(define (rect-equations local)
  (let* ((q (coordinate local))
         (x (ref q 0))
         (y (ref q 1))

         (v (velocity local))
         (xdot (ref v 0))
         (ydot (ref v 1))

         (a (acceleration local))
         (xdotdot (ref a 0))
         (ydotdot (ref a 1))

         (U (literal-function 'U)))
    (up (/ (+ (* 'm xdotdot (sqrt (+ (square x) (square y))))
              (* x ((D U) (sqrt (+ (square x) (square y))))))
           (sqrt (+ (square x) (square y))))
        (/ (+ (* 'm ydotdot (sqrt (+ (square x) (square y))))
              (* y ((D U) (sqrt (+ (square x) (square y))))))
           (sqrt (+ (square x) (square y)))))))
```

And here are the equations of motion, written in terms of an explicit local tuple:

```scheme
(let ((rect-path (up 't
                     (up 'x 'y)
                     (up 'xdot 'ydot)
                     (up 'xdotdot 'ydotdot))))
  (->tex-equation
   (rect-equations rect-path)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ {{m \ddot{x} \sqrt{{x}^{2} + {y}^{2}} + x DU\left( \sqrt{{x}^{2} + {y}^{2}} \right)}\over {\sqrt{{x}^{2} + {y}^{2}}}}} \cr \cr \displaystyle{ {{m \ddot{y} \sqrt{{x}^{2} + {y}^{2}} + y DU\left( \sqrt{{x}^{2} + {y}^{2}} \right)}\over {\sqrt{{x}^{2} + {y}^{2}}}}}\end{pmatrix}
\end{equation}

Now use the `p->r` conversion to substitute each of the rectangular values above with their associated polar values:

```scheme
(let* ((convert-path (F->C p->r))
       (polar-path (up 't
                       (up 'r 'phi)
                       (up 'rdot 'phidot)
                       (up 'rdotdot 'phidotdot)))
       (local (convert-path polar-path)))
  (->tex-equation
   (rect-equations local)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{  - m {\dot{\phi}}^{2} r \cos\left( \phi \right) - 2 m \dot{\phi} \dot{r} \sin\left( \phi \right) - m \ddot{\phi} r \sin\left( \phi \right) + m \ddot{r} \cos\left( \phi \right) + DU\left( r \right) \cos\left( \phi \right)} \cr \cr \displaystyle{  - m {\dot{\phi}}^{2} r \sin\left( \phi \right) + 2 m \dot{\phi} \dot{r} \cos\left( \phi \right) + m \ddot{\phi} r \cos\left( \phi \right) + m \ddot{r} \sin\left( \phi \right) + DU\left( r \right) \sin\left( \phi \right)}\end{pmatrix}
\end{equation}

I stared at this for a while&#x2026; and eventually noticed this linear combination that gets me to the result.

```scheme
(let* ((convert-path (F->C p->r))
       (polar-path (up 't
                       (up 'r 'phi)
                       (up 'rdot 'phidot)
                       (up 'rdotdot 'phidotdot)))
       (local (convert-path polar-path))
       (eq (rect-equations local)))
  (->tex-equation
   (up (+ (* (cos 'phi) (ref eq 0))
          (* (sin 'phi) (ref eq 1)))
       (- (* 'r (cos 'phi) (ref eq 1))
          (* 'r (sin 'phi) (ref eq 0))))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{  - m {\dot{\phi}}^{2} r + m \ddot{r} + DU\left( r \right)} \cr \cr \displaystyle{ 2 m \dot{\phi} r \dot{r} + m \ddot{\phi} {r}^{2}}\end{pmatrix}
\end{equation}

## Exercise 1.15: Equivalence<a id="sec-1-16"></a>

NOTE - I have a strong suspicion here that we can show that what is actually going on is that we end up with a total time derivative that we can ignore. The final terms at the end that cancel&#x2026; why is it that they work out the way they do? It would be nice to try and use the time-derivative test machinery we built to take a look there.

This one was a serious doozy. I think that this exercise can be a great way to show off the computer algebra system, and show off the steps that I go through to make a proof.

But I also want to pull back and stare at the formula. What is going on? What is the meaning of the extra terms? If we can say, for example, that they're a total time derivative, looking at the future, that would be great. There has to be a reason that the Lagrangian doesn't change.

The same thing happens when you look at a new Lagrangian and see a "fictitious force term" for, say, centrifugal force. There is something going on here.

Okay, let's get into the derivation. State the problem, what we're trying to solve, then let's get into the derivation.

### Overview<a id="sec-1-16-1"></a>

Here's what's next:

-   Start by defining what's going on, what we're trying to do.
-   Lock down some of the compositions in code.
-   Show the by-hand version (remember to sub in the down tuple)
-   Show that we can confirm it all cancels out in code&#x2026; IF we could possibly see the factor.
-   Come up with a test case for the partial problem for Sussman.

### Coordinate Transformations<a id="sec-1-16-2"></a>

> Show by direct calculation that the Lagrange equations for \\(L'\\) are satisfied if the Lagrange equations for \\(L\\) are satisfied.

The coordinate transform looks like

\begin{equation}
x = F(t, x')
\end{equation}

Imagine that we're getting to rectangular from polar, say. And the coordinate transform can depend on time.

So let's say we have some Lagrangian \\(L\\) expressed in terms of the unprimed coordinates. We want a new Lagrangian, \\(L'\\), that we can use directly.

If the Lagrangian value CAN'T change, if it's not coordinate dependent, then this has to be true:

\begin{equation}
L' \circ \Gamma[q'] = L \circ \Gamma[q]
\end{equation}

Lots of discussion about how we need to find a \\(C\\) that can change the path:

\begin{equation}
L \circ \Gamma[q] = L \circ C \circ \Gamma[q']
\end{equation}

### Code Preliminaries<a id="sec-1-16-3"></a>

I didn't get terribly far converting this to code, but I'll leave what I have here in case some intrepid soul wants to carry the torch. This can help us a bit, so let me set up some structure.

Goal: Figure out how to write \\(C\\) given \\(F\\), so that the computer can take derivatives for us. Ideally I'd push the entire proof through, but I got stuck.

Redefine the `F->C` function on page 46, so that \\(F\\) takes an explicit \\(t\\) and \\(x'\\) argument instead of having to rely on the tuple.

```scheme
(define ((F->C* F) local)
  (let ((t (time local))
        (x (coordinate local))
        (v (velocity local)))
    (up t
        (F t x)
        (+ (((partial 0) F) t x)
           (* (((partial 1) F) t x)
              v)))))
```

Functions:

```scheme
(define F
  (literal-function 'F (-> (X Real Real) Real)))

(define C (F->C* F))

(define L
  (literal-function 'L (-> (UP Real Real Real) Real)))

(define qprime
  (literal-function 'qprime))
```

And now, finally, we can talk about C. This matches what we have in the book.

```scheme
(->tex-equation
 ((compose C (Gamma qprime)) 't))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix}
\end{equation}

We can also do this by converting the path itself:

```scheme
(define ((to-q F qp) t)
  (F t (qp t)))
```

It's the same:

```scheme
(->tex-equation
 ((- (compose C (Gamma qprime))
     (Gamma (to-q F qprime)))
  't))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{pmatrix}
\end{equation}

So we can define a few more things. Talk about these in a bit.

```scheme
(define q (to-q F qprime))
(define Lprime (compose L C))
```

### Code Version<a id="sec-1-16-4"></a>

Let's expand the terms of the Lagrange equations.

```scheme
(->tex-equation
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))
```

\begin{equation}
D{q}^\prime\left( t \right) {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) + {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

The Lagrange equation term is in there with a factor on it. Here's the term I need to subtract:

```scheme
(let* ((factor
        (compose coordinate ((partial 1) C) (Gamma qprime)))      )
  (->tex-equation
   ((* factor (compose ((partial 1) L) (Gamma q)))
    't)))
```

\begin{equation}
{\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right)
\end{equation}

So now we have the whole thing and the term to substract off. This is the extra:

```scheme
(let* ((factor
        (compose coordinate ((partial 1) C) (Gamma qprime))))
  (->tex-equation
   ((- (compose ((partial 1) Lprime) (Gamma qprime))
       (* factor (compose ((partial 1) L) (Gamma q))))
    't)))
```

\begin{equation}
D{q}^\prime\left( t \right) {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

Let's do the other side. Nightmare!

```scheme
(->tex-equation
 ((D (compose ((partial 2) Lprime) (Gamma qprime)))
  't))
```

\begin{equation}
{\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( D{q}^\prime\left( t \right) \right)}^{2} {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {D}^{2}{q}^\prime\left( t \right) {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + 2 {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\left( {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 0 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + D{q}^\prime\left( t \right) {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 0 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

I see Lagrange's equation peeking out, with a big factor attached to it.

```scheme
(let* ((factor
        (compose velocity ((partial 2) C) (Gamma qprime))))
  (->tex-equation
   ((* factor (D (compose ((partial 2) L) (Gamma q))))
    't)))
```

\begin{equation}
{\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( D{q}^\prime\left( t \right) \right)}^{2} {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\left( partial\left( 1 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {D}^{2}{q}^\prime\left( t \right) {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + 2 {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( partial\left( 0 \right) partial\left( 1 \right) \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) D{q}^\prime\left( t \right) {\left( {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} + {\left( partial\left( 2 \right) \right)}^{2}\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\left( partial\left( 0 \right) \right)}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 1 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + \left( partial\left( 0 \right) partial\left( 2 \right) \right)\left( L \right)\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

About the factors, this is interesting:

```scheme
(->tex-equation
 ((- (compose velocity ((partial 2) C) (Gamma qprime))
     (compose coordinate ((partial 1) C) (Gamma qprime)))
  't))
```

\begin{equation}
0
\end{equation}

So the factors are identical. Why? Think this through.

If you pull the factor out, you see that you've generated new Lagrange equations, scaled by a factor.

```scheme
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
```

\begin{equation}
0
\end{equation}

Done! Amazing, it totally works in code, with some inference help.

### Derivation<a id="sec-1-16-5"></a>

The goal here is to show that if the Lagrange equations hold in the original coordinate system:

\begin{equation}
\label{eq:1-15-lagrange}
D(\partial\_2L \circ \Gamma[q]) - (\partial\_1L \circ \Gamma[q]) = 0
\end{equation}

Then they hold in the primed coordinate system:

\begin{equation}
\label{eq:lagrange-prime}
D(\partial\_2L' \circ \Gamma[q']) - (\partial\_1L' \circ \Gamma[q']) = 0
\end{equation}

Approach: let's start pushing the Lagrange equations through, and see if we recognize any spot where we can use our assumption.

We start by calculating the components of the Lagrange equations in equation \eqref{eq:lagrange-prime}. We'll handle the \\(\partial\_2L'\\) term first.

Composition is associative, so

\begin{equation}
\label{eq:c-l}
(L \circ C) \circ \Gamma[q'] = L' \circ \Gamma[q'] \implies L' = L \circ C
\end{equation}

Substituting the right side of \eqref{eq:c-l} and using the chain rule:

\begin{equation}
  \partial\_2L' = \partial\_2(L \circ C) = ((DL) \circ C) \partial\_2 C
\end{equation}

NOTE describe the shape of what's going on here, how we have these up and down tuple shapes. That means we can expand this out:

\begin{equation}
  (DL \circ C)\partial\_2 C = (\partial\_0L \circ C)(I\_0 \circ \partial\_2 C) + (\partial\_1L \circ C)(I\_1 \circ \partial\_2 C) + (\partial\_2L \circ C)(I\_2 \circ \partial\_2 C)
\end{equation}

It's less confusing if you leave functional notation for a moment and imagine application to some argument \\(x\\). \\(\partial\_2C(x)\\) is an up-tuple with 3 elements \\((\partial\_2C(x)\_0, \partial\_2C(x)\_1, \partial\_2C(x)\_2)\\) and \\(DL(C(x))\\) is a down-tuple, so they contract like this:

\begin{equation}
  DL(C(x)) \cdot \partial\_2 C(x) = \partial\_0L(C(x))\partial\_2 C(x)\_0 + \partial\_1L(C(x))\partial\_2 C(x)\_1 + \partial\_2L(C(x))\partial\_2 C(x)\_2
\end{equation}

Now look at the \\(\partial\_2C\\) term using our code from before:

```scheme
(let ((C (F->C* F*)))
  (->tex-equation
   (((partial 2) C) (up 't 'xprime 'vprime))
   "eq:p2c"))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ {\partial}\_{1}F\left( t, {x}^\prime \right)}\end{pmatrix}
\label{eq:p2c}
\end{equation}

Add the path \\(\Gamma[q']\\) back in and distribute, remembering that \\(C \circ \Gamma[q'] = \Gamma[q]\\):

\begin{equation}
  \begin{aligned}
    \partial\_2L' \circ \Gamma[q'] & = (\partial\_2L \circ C)(I\_2 \circ \partial\_2 C) \circ \Gamma[q'] \cr
    & = (\partial\_2L \circ C \circ \Gamma[q'])(I\_2 \circ \partial\_2 C \circ \Gamma[q']) \cr
    & = (\partial\_2L \circ \Gamma[q])(I\_2 \circ \partial\_2 C \circ \Gamma[q'])
  \end{aligned}
\end{equation}

Now take the time derivative:

\begin{equation}
  D(\partial\_2L' \circ \Gamma[q']) = D\left[(\partial\_2L \circ \Gamma[q])(I\_2 \circ \partial\_2 C \circ \Gamma[q'])\right]
\end{equation}

Use the product rule:

\begin{equation}
  D(\partial\_2L' \circ \Gamma[q']) = \left[ D(\partial\_2L \circ \Gamma[q]) \right](I\_2 \circ \partial\_2 C \circ \Gamma[q']) + (\partial\_2L \circ \Gamma[q])D\left[ (I\_2 \circ \partial\_2 C \circ \Gamma[q']) \right]
\end{equation}

substitute using \eqref{eq:p2c}. When we compose with a path we get this function of \\(t\\):

```scheme
(let ((C (F->C* F*)))
  (->tex-equation
   (ref ((compose ((partial 2) C) (Gamma qprime)) 't) 2)))
```

\begin{equation}
{\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

Substitute:

\begin{equation}
\label{eq:1-15-p2l}
  D(\partial\_2L' \circ \Gamma[q']) = D(\partial\_2L \circ \Gamma[q])\partial\_1F(t, q'(t)) + (\partial\_2L \circ \Gamma[q]) D(\partial\_1F(t, q'(t)))
\end{equation}

We can see Lagrange's equation from our assumption peeking. Before we tackle that, let's do the other side:

\begin{equation}
  \partial\_1L' = \partial\_1(L \circ C) = ((DL) \circ C) \partial\_1 C
\end{equation}

Use our function above to take \\(\partial\_1C\\):

```scheme
(let ((C (F->C* F*)))
  (->tex-equation
   (((partial 1) C) (up 't 'xprime 'vprime))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ {\partial}\_{1}F\left( t, {x}^\prime \right)} \cr \cr \displaystyle{ {v}^\prime {\left( \partial\_1 \right)}^{2}F\left( t, {x}^\prime \right) + \left( \partial\_0 \partial\_1 \right)F\left( t, {x}^\prime \right)}\end{pmatrix}
\end{equation}

Expand the chain rule out and cancel, as before:

\begin{equation}
  \begin{aligned}
    \partial\_1L' & = ((DL) \circ C) \partial\_1 C \cr
    & = (\partial\_0L \circ C)(I\_0 \circ \partial\_1 C) + (\partial\_1L \circ C)(I\_1 \circ \partial\_1 C) + (\partial\_2L \circ C)(I\_2 \circ \partial\_1 C) \cr
    & = (\partial\_1L \circ C)(I\_1 \circ \partial\_1 C) + (\partial\_2L \circ C)(I\_2 \circ \partial\_1 C)
  \end{aligned}
\end{equation}

Add the path \\(\Gamma[q']\\) back in and distribute:

\begin{equation}
  \begin{aligned}
  \partial\_1L' \circ \Gamma[q'] & = (\partial\_1L \circ \Gamma[q])(I\_1 \circ \partial\_1 C \circ \Gamma[q']) + (\partial\_2L \circ \Gamma[q])(I\_2 \circ \partial\_1 C \circ \Gamma[q']) \cr
& = (\partial\_1L \circ \Gamma[q])(\partial\_1F(t, q'(t))) + (\partial\_2L \circ \Gamma[q])D(\partial\_1F(t, q'(t)))
  \end{aligned}
\end{equation}

We're almost there! Go back to \label{eq:1-15-p2l}, and use our assumption \eqref{eq:1-15-lagrange} that the original Lagrange equations hold:

\begin{equation}
  \begin{aligned}
  D(\partial\_2L' \circ \Gamma[q']) - \partial\_1L' \circ \Gamma[q'] & = D(\partial\_2L \circ \Gamma[q] - (\partial\_1L \circ \Gamma[q]))\partial\_1F(t, q'(t)) \cr
& + (\partial\_2L \circ \Gamma[q]) D(\partial\_1F(t, q'(t))) - (\partial\_2L \circ \Gamma[q])D(\partial\_1F(t, q'(t))) \cr
& = D(\partial\_2L \circ \Gamma[q]) - (\partial\_1L \circ \Gamma[q]))\partial\_1F(t, q'(t)) \cr
& = 0
  \end{aligned}
\end{equation}

And boom, we're done.

## Exercise 1.16: Central force motion<a id="sec-1-17"></a>

Messing around to make sure I understand what I'm seeing in the coordinate transforms on page 45.

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

Check polar:

```scheme
(show-expression
 ((F->C p->r)
  (up 't
      (up 'r 'phi)
      (up 'rdot 'phidot))))
```

spherical coordinate change, check velocities:

```scheme
(show-expression
 ((F->C spherical->rect)
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))

(show-expression
 (square (ref (ref ((F->C spherical->rect)
             (up 't
                 (up 'r 'theta 'phi)
                 (up 'rdot 'thetadot 'phidot))) 2) 0)))
```

get the Langrangian from page 41:

```scheme
(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))
```

BOOM, now we can compose these things!

```scheme
(define (L-central-polar m U)
  (compose (L-central-rectangular m U)
           (F->C p->r)))

(define (L-central-spherical m U)
  (compose (L-central-rectangular m U)
           (F->C spherical->rect)))
```

Confirm the polar coordinate version&#x2026;

```scheme
(show-expression
 ((L-central-polar 'm (literal-function 'U))
  (up 't
      (up 'r 'phi)
      (up 'rdot 'phidot))))
```

BOOM, much better than calculating by hand!

```scheme
(show-expression
 ((L-central-spherical 'm (literal-function 'U))
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))
```

rectangular, for fun:

```scheme
(show-expression
 ((L-central-rectangular 'm (literal-function 'U))
  (up 't
      (up 'x 'y 'z)
      (up 'xdot 'ydot 'zdot))))
```

From a note to Vinay. Langrangian coordinate transformation from spherical -> rectangular on paper, which of course is a total nightmare, writing vx<sup>2</sup> + vy<sup>2</sup> + vz<sup>2</sup> and simplifying. BUT then, of course, you write down the spherical => rectangular position change&#x2026;

the explicit link to function composition, and how the new lagrangian is (Lagrangian A + A<-B + B<-C)&#x2026; really drives home how invertible coordinate transforms can stack associatively through function composition. the lesson is, prove that the code works, then trust the program to go to crazy coordinate systems.

they add in a very simple-to-write coordinate transform that has one of the angles depend on t. and then compose that in, and boom, basically for free you're in rotating spherical coords.

## Exercise 1.17: Bead on a helical wire<a id="sec-1-18"></a>

## Exercise 1.18: Bead on a triaxial surface<a id="sec-1-19"></a>

## Exercise 1.19: Two-bar linkage<a id="sec-1-20"></a>

## Exercise 1.20: Sliding pendulum<a id="sec-1-21"></a>

## Exercise 1.21: A dumbbell<a id="sec-1-22"></a>

The uneven dumbbell.

NOTE for when I write this up. This exercise is quite careful to NOT change the dimension of the configuration space, when it does coordinate transformations. We show later that you can do that, but that's the reason, good to note, why you introduce a new variable \\(c\\) that's equal to the distance between the dumbbells.

```scheme
(load "ch1/utils.scm")
```

Takes in any number of up tuples and zips them into a new list of up-tuples by taking each element.

```scheme
(define (up-zip . ups)
  (apply vector-map up (map up->vector ups)))
```

I spent some time trying to make a nice API&#x2026; but without map, filter, reduce etc on tuples it is quite annoying. So let's go ad hoc first and see what happens.

```scheme
(define (KE-particle m v)
  (* 1/2 m (square v)))
```

```scheme
;; gets the particle itself
(define ((extract-particle pieces) local i)
  (let* ((q (coordinate local))
         (qdot (velocity local))
         (indices (apply up (iota pieces (* i pieces))))
         (extract (lambda (tuple)
                    (vector-map (lambda (i) (ref tuple i))
                                indices))))
    (up (time q)
        (extract q)
        (extract qdot))))

(define (constraint q0 q1 F l)
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))

(define ((L-free-constrained m0 m1 l) local)
  (let* ((extract (extract-particle 2))
         (p0 (extract local 0))
         (q_0 (coordinate p0))
         (qdot_0 (velocity p0))

         (p1 (extract local 1))
         (q_1 (coordinate p1))
         (qdot_1 (velocity p1))

         (F (ref (coordinate local) 4)))
    (- (+ (KE-particle m0 qdot_0)
          (KE-particle m1 qdot_1))
       (constraint q_0 q_1 F l))))

(define q-rect
  (up (literal-function 'x_0)
      (literal-function 'y_0)
      (literal-function 'x_1)
      (literal-function 'y_1)
      (literal-function 'F)))
```

This shows the lagrangian itself, which answers part b:

```scheme
(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f (compose L (Gamma q-rect))))
  (se (f 't)))
```

Here are the lagrange equations, confirming part b.

```scheme
(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q-rect)))
  (se (f 't)))
```

Part c - make a change of coordinates.

```scheme
(define ((cm-theta->rect m0 m1) local)
  (let* ((q (coordinate local))
         (x_cm (ref q 0))
         (y_cm (ref q 1))
         (theta (ref q 2))
         (c (ref q 3))
         (F (ref q 4))
         (total-mass (+ m0 m1))
         (m0-distance (* c (/ m1 total-mass)))
         (m1-distance (* c (/ m0 total-mass))))
    (up (- x_cm (* m0-distance (cos theta)))
        (- y_cm (* m0-distance (sin theta)))
        (+ x_cm (* m1-distance (cos theta)))
        (+ y_cm (* m1-distance (sin theta)))
        F)))

(se
 ((F->C (cm-theta->rect 'm_0 'm_1))
  (up 't
      (up 'x_cm 'y_cm 'theta 'c 'F)
      (up 'xdot_cm 'ydot_cm 'thetadot 'cdot 'Fdot))))

(define (L-free-constrained-new m0 m1 l)
  (compose (L-free-constrained m0 m1 l)
           (F->C (cm-theta->rect m0 m1))))
```

This shows the lagrangian itself, after the coordinate transformation:

```scheme
(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained-new 'm_0 'm_1 'l))
       (f (compose L (Gamma q))))
  (se (f 't)))
```

Here are the lagrange equations for part c.

```scheme
(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained-new 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (se (f 't)))
```

For part d, we can substitute the constant value of c to get simplified equations.

```scheme
(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (L (L-free-constrained-new 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (se (f 't)))
```

For part e, I wrote this in the notebook - it is effectively identical to the substitution that is happening on the computer, so I'm going to ignore this. You just get more cancellations.

## Exercise 1.22: Driven pendulum<a id="sec-1-23"></a>

## Exercise 1.23: Fill in the details<a id="sec-1-24"></a>

## Exercise 1.24: Constraint forces<a id="sec-1-25"></a>

## Exercise 1.25: Foucalt pendulum Lagrangian<a id="sec-1-26"></a>

## Exercise 1.26: Properties of \\(D\_t\\)<a id="sec-1-27"></a>

## Exercise 1.27: Lagrange equations for total time derivatives<a id="sec-1-28"></a>

## Exercise 1.28: Total Time Derivatives<a id="sec-1-29"></a>

```scheme
(load "ch1/utils.scm")
```

### part A<a id="sec-1-29-1"></a>

nice, easy to guess.

```scheme
(define ((FA m) local)
  (let ((x (coordinate local)))
    (* m x)))
```

Show the function of t, and confirm that both methods are equivalent.

```scheme
(check-f (FA 'm)
         (literal-function 'x))
```

### Part B<a id="sec-1-29-2"></a>

NOT a total time derivative.

Define G directly:

```scheme
(define ((GB m) local)
  (let* ((t (time local))
         (v_x (velocity local))
         (GB0 0)
         (GB1 (* m (cos t))))
    (+ GB0 (* GB1 v_x))))
```

And show the full G, for fun:

```scheme
(let ((f (compose (GB 'm) (Gamma (literal-function 'x)))))
  (se (f 't)))
```

It's easier to confirm that this is not a total time derivative by checking the partials.

```scheme
(define (GB-properties m)
  (let ((GB0 (lambda (local) 0))
        (GB1 (lambda (local)
               (* m (cos (time local))))))
    (G-properties GB0 GB1 (literal-function 'x))))
```

It's clear here that the second and third tuple entries aren't equal, so we don't have a total time derivative.

```scheme
(se (GB-properties 'm))
```

### Part C<a id="sec-1-29-3"></a>

no problem, we've got a total time derivative on our hands.

```scheme
(define (FC local)
  (let ((t (time local))
        (x (coordinate local)))
    (* x (cos t))))

(check-f FC (literal-function 'x))

(define GC-properties
  (let ((GC0 (lambda (local)
               (* -1
                  (coordinate local)
                  (sin (time local)))))
        (GC1 (lambda (local)
               (cos (time local)))))
    (G-properties GC0 GC1 (literal-function 'x))))
```

Boom, the second and third entries are equal, as we'd expect.

```scheme
(se GC-properties)
```

### Part D<a id="sec-1-29-4"></a>

This is NOT a total time derivative; you can tell by taking the partials of each side, G0 and G1, as we'll see here.

```scheme
(define GD-properties
  (let ((GD0 (lambda (local)
               (* (coordinate local)
                  (sin (time local)))))
        (GD1 (lambda (local)
               (cos (time local)))))
    (G-properties GD0 GD1 (literal-function 'x))))
```

The partials for each side don't match.

```scheme
(se GD-properties)
```

### Part E<a id="sec-1-29-5"></a>

This is strange to me, because I thought that this thing had to produce a tuple.

OH, but the secret is that Qdot is also a tuple, so you contract them together.

Here's the function F that we can use to derive it:

```scheme
(define (FE local)
  (let* ((t (time local))
         (q (coordinate local))
         (x (ref q 0))
         (y (ref q 1)))
    (* (+ (square x) (square y))
       (cos t))))
```

Boom, total time derivative!

```scheme
(check-f FE (up (literal-function 'x)
                (literal-function 'y)))
```

And let's show that we pass the tests by decomposing this into G0 and G1:

```scheme
(define GE-properties
  (let (
        ;; any piece of the function without a velocity multiplied.
        (GE0 (lambda (local)
               (let* ((t (time local))
                      (q (coordinate local))
                      (x (ref q 0))
                      (y (ref q 1)))
                 (* -1
                    (+ (square x) (square y))
                    (sin t)))))

        ;; The pieces multiplied by velocities, split into a down tuple of
        ;; components, one for each of the coordinate components.
        (GE1 (lambda (local)
               (let* ((t (time local))
                      (q (coordinate local))
                      (x (ref q 0))
                      (y (ref q 1)))
                 (down
                  (* 2 x (cos t))
                  (* 2 y (cos t)))))))
    (G-properties GE0 GE1 (up (literal-function 'x)
                              (literal-function 'y)))))
```

BOOM!

We've recovered F; the partials are equal, and the final matrix is symmetric.

```scheme
(se GE-properties)
```

### Part F<a id="sec-1-29-6"></a>

This one is interesting, since the second partial is a tuple. This is not so obvious to me, so first let's check the properties:

```scheme
(define GF-properties
  (let (
        ;; any piece of the function without a velocity multiplied.
        (GF0 (lambda (local)
               (let* ((t (time local))
                      (q (coordinate local))
                      (x (ref q 0))
                      (y (ref q 1)))
                 (* -1
                    (+ (square x) (square y))
                    (sin t)))))

        ;; The pieces multiplied by velocities, split into a down tuple of
        ;; components, one for each of the coordinate components.
        (GF1 (lambda (local)
               (let* ((t (time local))
                      (q (coordinate local))
                      (x (ref q 0))
                      (y (ref q 1)))
                 (down
                  (+ (cube y) (* 2 x (cos t)))
                  (+ x (* 2 y (cos t))))))))
    (G-properties GF0 GF1 (up (literal-function 'x)
                              (literal-function 'y)))))
```

AND it looks like we DO have a total time derivative, maybe. We certainly pass the first test here, since the second and third tuple entries are equal.

BUT we fail the second test; the hessian that we get from ((partial 1) G1) is not symmetric.

```scheme
(se GF-properties)
```

## Exercise 1.29: Galilean Invariance<a id="sec-1-30"></a>

I'll do this for a single particle, since it's annoying to get the sum going for many; and the lagrangian is additive, so no problem.

```scheme
(define (uniform-translate-shift->rect local)
  (let* ((t (time local))
         (q (coordinate local))
         (xprime (ref q 0))
         (delta_x (ref q 1))
         (delta_v (ref q 2)))
    (+ xprime delta_x (* t delta_v))))

(define (L-translate-shift m)
  (compose (L-free-particle m)
           (F->C uniform-translate-shift->rect)))
```

First, confirm that if we have a constant, we get what we expected from paper.

```scheme
(let* ((q (up (literal-function 'xprime)
              (lambda (t) 'Delta_x)
              (lambda (t) 'Delta_v)))
       (f (compose (L-translate-shift 'm) (Gamma q))))
  (->tex-equation (f 't)))
```

\\[\begin{equation} {{1}\over {2}} {{\Delta}\_{v}}^{2} m + {\Delta}\_{v} m D{x}^\prime\left( t \right) + {{1}\over {2}} m {\left( D{x}^\prime\left( t \right) \right)}^{2} \end{equation}\\]

We can change this a little to see the extra terms; substract off the free particle lagrangian, to see the extra stuff.

```scheme
(let* ((q (up (literal-function 'xprime)
              (lambda (t) 'Delta_x)
              (lambda (t) 'Delta_v)))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (compose L (Gamma q))))
  (->tex-equation (f 't)))
```

\\[\begin{equation} {{1}\over {2}} {{\Delta}\_{v}}^{2} m + {\Delta}\_{v} m D{x}^\prime\left( t \right) \end{equation}\\]

Here's the gnarly version with both entries as actual functions. Can this be a total time derivative? It CANNOT be, because we have a \\((D \Delta\_v(t))^2\\) term in there, and we know that total time derivatives have to be linear in the velocities. The function \\(F\\) would have had to have a velocity in it, which is not allowed.

```scheme
(let* ((q (up (literal-function 'xprime)
              (literal-function 'Delta_x)
              (literal-function 'Delta_v)))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (compose L (Gamma q))))
  (->tex-equation (f 't)))
```

\\[ \begin{equation} {{1}\over {2}} m {t}^{2} {\left( D{\Delta}\_{v}\left( t \right) \right)}^{2} + m t D{x}^\prime\left( t \right) D{\Delta}\_{v}\left( t \right) + m t D{\Delta}\_{v}\left( t \right) {\Delta}\_{v}\left( t \right) + m t D{\Delta}\_{v}\left( t \right) D{\Delta}\_{x}\left( t \right) + m D{x}^\prime\left( t \right) {\Delta}\_{v}\left( t \right) + m D{x}^\prime\left( t \right) D{\Delta}\_{x}\left( t \right) - {{1}\over {2}} m {\left( D{\Delta}\_{v}\left( t \right) \right)}^{2} + {{1}\over {2}} m {\left( {\Delta}\_{v}\left( t \right) \right)}^{2} + m {\Delta}\_{v}\left( t \right) D{\Delta}\_{x}\left( t \right) \end{equation} \\]

Let's simplify by making the \\(\Delta\_v\\) constant and see if there's anything so obvious about \\(\Delta\_x\\).

We know that we have a total derivative when \\(\Delta\_x\\) is constant, and we know that total time derivatives are linear, so let's substract off the total time derivative and see what happens:

```scheme
(let* ((q (lambda (dx)
            (up (literal-function 'xprime)
                dx
                (lambda (t) 'Delta_v))))
       (L (- (L-translate-shift 'm)
             (L-free-particle 'm)))
       (f (lambda (dx)
            (compose L (Gamma (q dx))))))
  (->tex-equation
   ((- (f (literal-function 'Delta_x))
       (f (lambda (t) 'Delta_x)))
    't)))
```

\\[\begin{equation} {\Delta}\_{v} m D{\Delta}\_{x}\left( t \right) + m D{x}^\prime\left( t \right) D{\Delta}\_{x}\left( t \right) \end{equation}\\]

Take a look. there is a quadratic velocity term in here! We have \\(D \Delta\_x(t) D x'(t)\\). This is not allowed in a total time derivative.

SO, only if the shift and uniform translation are constant do we not affect the Lagrangian value.

## Exercise 1.30: Orbits in a central potential<a id="sec-1-31"></a>

## Exercise 1.31: Foucault pendulum evolution<a id="sec-1-32"></a>

## Exercise 1.32: Time-dependent constraints<a id="sec-1-33"></a>

## Exercise 1.33: Falling off a log<a id="sec-1-34"></a>

## Exercise 1.34: Driven spherical pendulum<a id="sec-1-35"></a>

## Exercise 1.35: Restricted equations of motion<a id="sec-1-36"></a>

## Exercise 1.36: Noether integral<a id="sec-1-37"></a>

## Exercise 1.37: Velocity transformation<a id="sec-1-38"></a>

## Exercise 1.38: Properties of \\(E\\)<a id="sec-1-39"></a>

## Exercise 1.39: Combining Lagrangians<a id="sec-1-40"></a>

## Exercise 1.40: Bead on a triaxial surface<a id="sec-1-41"></a>

## Exercise 1.41: Motion of a tiny golf ball<a id="sec-1-42"></a>

## Exercise 1.42: Augmented Lagrangian<a id="sec-1-43"></a>

## Exercise 1.43: A numerical investigation<a id="sec-1-44"></a>

## Exercise 1.44: Double pendulum behavior<a id="sec-1-45"></a>

# Rigid Bodies<a id="sec-2"></a>

## Exercise 2.1<a id="sec-2-1"></a>

## Exercise 2.2<a id="sec-2-2"></a>

## Exercise 2.3<a id="sec-2-3"></a>

## Exercise 2.4<a id="sec-2-4"></a>

## Exercise 2.5<a id="sec-2-5"></a>

## Exercise 2.6<a id="sec-2-6"></a>

## Exercise 2.7<a id="sec-2-7"></a>

## Exercise 2.8<a id="sec-2-8"></a>

## Exercise 2.9<a id="sec-2-9"></a>

## Exercise 2.10<a id="sec-2-10"></a>

## Exercise 2.11<a id="sec-2-11"></a>

## Exercise 2.12<a id="sec-2-12"></a>

## Exercise 2.13<a id="sec-2-13"></a>

## Exercise 2.14<a id="sec-2-14"></a>

## Exercise 2.15<a id="sec-2-15"></a>

## Exercise 2.16<a id="sec-2-16"></a>

## Exercise 2.17<a id="sec-2-17"></a>

## Exercise 2.18<a id="sec-2-18"></a>

## Exercise 2.19<a id="sec-2-19"></a>

## Exercise 2.20<a id="sec-2-20"></a>

# Hamiltonian Mechanics<a id="sec-3"></a>

## Exercise 3.1<a id="sec-3-1"></a>

## Exercise 3.2<a id="sec-3-2"></a>

## Exercise 3.3<a id="sec-3-3"></a>

## Exercise 3.4<a id="sec-3-4"></a>

## Exercise 3.5<a id="sec-3-5"></a>

## Exercise 3.6<a id="sec-3-6"></a>

## Exercise 3.7<a id="sec-3-7"></a>

## Exercise 3.8<a id="sec-3-8"></a>

## Exercise 3.9<a id="sec-3-9"></a>

## Exercise 3.10<a id="sec-3-10"></a>

## Exercise 3.11<a id="sec-3-11"></a>

## Exercise 3.12<a id="sec-3-12"></a>

## Exercise 3.13<a id="sec-3-13"></a>

## Exercise 3.14<a id="sec-3-14"></a>

## Exercise 3.15<a id="sec-3-15"></a>

## Exercise 3.16<a id="sec-3-16"></a>

# Phase Space Structure<a id="sec-4"></a>

## Exercise 4.0<a id="sec-4-1"></a>

## Exercise 4.1<a id="sec-4-2"></a>

## Exercise 4.2<a id="sec-4-3"></a>

## Exercise 4.3<a id="sec-4-4"></a>

## Exercise 4.4<a id="sec-4-5"></a>

## Exercise 4.5<a id="sec-4-6"></a>

## Exercise 4.6<a id="sec-4-7"></a>

## Exercise 4.7<a id="sec-4-8"></a>

## Exercise 4.8<a id="sec-4-9"></a>

## Exercise 4.9<a id="sec-4-10"></a>

## Exercise 4.10<a id="sec-4-11"></a>

# Canonical Transformations<a id="sec-5"></a>

## Exercise 5.1<a id="sec-5-1"></a>

## Exercise 5.2<a id="sec-5-2"></a>

## Exercise 5.3<a id="sec-5-3"></a>

## Exercise 5.4<a id="sec-5-4"></a>

## Exercise 5.5<a id="sec-5-5"></a>

## Exercise 5.6<a id="sec-5-6"></a>

## Exercise 5.7<a id="sec-5-7"></a>

## Exercise 5.8<a id="sec-5-8"></a>

## Exercise 5.9<a id="sec-5-9"></a>

## Exercise 5.10<a id="sec-5-10"></a>

## Exercise 5.11<a id="sec-5-11"></a>

## Exercise 5.12<a id="sec-5-12"></a>

## Exercise 5.13<a id="sec-5-13"></a>

## Exercise 5.14<a id="sec-5-14"></a>

## Exercise 5.15<a id="sec-5-15"></a>

## Exercise 5.16<a id="sec-5-16"></a>

## Exercise 5.17<a id="sec-5-17"></a>

## Exercise 5.18<a id="sec-5-18"></a>

## Exercise 5.19<a id="sec-5-19"></a>

## Exercise 5.20<a id="sec-5-20"></a>

# Canonical Evolution<a id="sec-6"></a>

## Exercise 6.1<a id="sec-6-1"></a>

## Exercise 6.2<a id="sec-6-2"></a>

## Exercise 6.3<a id="sec-6-3"></a>

## Exercise 6.4<a id="sec-6-4"></a>

## Exercise 6.5<a id="sec-6-5"></a>

## Exercise 6.6<a id="sec-6-6"></a>

## Exercise 6.7<a id="sec-6-7"></a>

## Exercise 6.8<a id="sec-6-8"></a>

## Exercise 6.9<a id="sec-6-9"></a>

## Exercise 6.10<a id="sec-6-10"></a>

## Exercise 6.11<a id="sec-6-11"></a>

## Exercise 6.12<a id="sec-6-12"></a>

# Canonical Perturbation Theory<a id="sec-7"></a>

## Exercise 7.1<a id="sec-7-1"></a>

## Exercise 7.2<a id="sec-7-2"></a>

## Exercise 7.3<a id="sec-7-3"></a>

## Exercise 7.4<a id="sec-7-4"></a>

## Exercise 7.5<a id="sec-7-5"></a>

# Our Notation<a id="sec-8"></a>

Notation Appendix. This is all about getting cozy with scheme, and with the various idiosyncracies of the tuple and functional notation.

## Exercise 9.1 Chain Rule<a id="sec-8-1"></a>

You're supposed to do these by hand, so I'll do that in the textbook. But here, let's redo them on the machine.

1.  Compute \\(\partial\_0 F(x, y)\\) and \\(\partial\_1 F(x, y)\\)

    First, let's define the functions we need.

    ```scheme
    (define (F x y)
      (* (square x)
         (cube y)))

    (define (G x y)
      (up (F x y) y))

    (define (H x y)
      (F (F x y) y))
    ```

    You can do this with explicit partials:

    ```scheme
    (let ((f (down ((partial 0) F) ((partial 1) F))))
      (->tex-equation
       (f 'x 'y)))
    ```

    \begin{equation}
    \begin{bmatrix} \displaystyle{ 2 x {y}^{3}} \cr \cr \displaystyle{ 3 {x}^{2} {y}^{2}}\end{bmatrix}
    \end{equation}

    Or with the \\(D\\) symbol:

    ```scheme
    (->tex-equation
     ((D F) 'x 'y))
    ```

    \begin{equation}
    \begin{bmatrix} \displaystyle{ 2 x {y}^{3}} \cr \cr \displaystyle{ 3 {x}^{2} {y}^{2}}\end{bmatrix}
    \end{equation}

    Or, we could show that they're equivalent this way:

    ```scheme
    (let ((f (down ((partial 0) F) ((partial 1) F))))
      (->tex-equation
       (- ((D F) 'x 'y)
          (f 'x 'y))))
    ```

    \begin{equation}
    \begin{bmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{bmatrix}
    \end{equation}

2.  Compute \\(\partial\_0 F(F(x, y), y)\\) and \\(\partial\_1 F(F(x, y), y)\\)

    \\(H\\) is already that composition, so:

    ```scheme
    (->tex-equation
     ((D H) 'x 'y))
    ```

    \begin{equation}
    \begin{bmatrix} \displaystyle{ 4 {x}^{3} {y}^{9}} \cr \cr \displaystyle{ 9 {x}^{4} {y}^{8}}\end{bmatrix}
    \end{equation}

3.  Compute \\(\partial\_0 G(x, y)\\) and \\(\partial\_1 G(x, y)\\)

    ```scheme
    (->tex-equation
     ((D G) 'x 'y))
    ```

    \begin{equation}
    \begin{bmatrix} \displaystyle{ \begin{pmatrix} \displaystyle{ 2 x {y}^{3}} \cr \cr \displaystyle{ 0}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ 3 {x}^{2} {y}^{2}} \cr \cr \displaystyle{ 1}\end{pmatrix}}\end{bmatrix}
    \end{equation}

4.  Compute \\(DF(a, b)\\), \\(DG(3, 5)\\) and \\(DH(3a^2, 5b^3)\\)

    ```scheme
    (->tex-equation
     (up ((D F) 'a 'b)
         ((D G) 3 5)
         ((D H) (* 3 (square 'a)) (* 5 (cube 'b)))))
    ```

    \begin{equation}
    \begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{ 2 a {b}^{3}} \cr \cr \displaystyle{ 3 {a}^{2} {b}^{2}}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ \begin{pmatrix} \displaystyle{ 750} \cr \cr \displaystyle{ 0}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ 675} \cr \cr \displaystyle{ 1}\end{pmatrix}}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ 210937500 {a}^{6} {b}^{27}} \cr \cr \displaystyle{ 284765625 {a}^{8} {b}^{24}}\end{bmatrix}}\end{pmatrix}
    \end{equation}

## Exercise 9.2: Computing Derivatives<a id="sec-8-2"></a>

A further exercise is to try defining the functions so that they use explicit tuples, so you can compose them:

```scheme
(define (F* v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (* (square x) (cube y))))

(define (G* v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (up (F* v) y)))

(define H* (compose F* G*))
```

to be really pro, I'd make a function that takes these as arguments and prints a nice formatted exercise output. Let's do the final exercise, for fun:

```scheme
(->tex-equation
 (up ((D F*) (up 'a 'b))
     ((D G*) (up 3 5))
     ((D H*) (up (* 3 (square 'a)) (* 5 (cube 'b))))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{ 2 a {b}^{3}} \cr \cr \displaystyle{ 3 {a}^{2} {b}^{2}}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ \begin{pmatrix} \displaystyle{ 750} \cr \cr \displaystyle{ 0}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ 675} \cr \cr \displaystyle{ 1}\end{pmatrix}}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ 210937500 {a}^{6} {b}^{27}} \cr \cr \displaystyle{ 284765625 {a}^{8} {b}^{24}}\end{bmatrix}}\end{pmatrix}
\end{equation}

# Org-Mode Demo<a id="sec-9"></a>

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

### Equations<a id="sec-9-0-1"></a>

Here's (a test) of \\(a = bc\\) and more \\[ \alpha\_t \\] equations:

And again this is a thing:

\\[ e^{i\pi} = -1 \\]

\\[ \int\_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2} \\]

\\(\vec{x} \dot (\vec{x} \times \vec{v}) = \vec{v} \dot (\vec{x} \times \vec{v}) = 0\\)

\\(\vec{x} \cdot (\vec{x} \times \vec{v}) = \vec{v} \dot (\vec{x} \times \vec{b}) = 0\\)
