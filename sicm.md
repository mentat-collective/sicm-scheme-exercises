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
    - [Intuition](#sec-1-7-1)
    - [Implementation](#sec-1-7-2)
    - [Execution](#sec-1-7-3)
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
    - [Preliminary Notes](#sec-1-13-1)
    - [Part A: Ideal Planar Pendulum](#sec-1-13-2)
    - [Part B: 2D Potential](#sec-1-13-3)
    - [Part C: Particle on a Sphere](#sec-1-13-4)
  - [Exercise 1.13: Higher-derivative Lagrangians (code)](#sec-1-14)
    - [Part A: Acceleration-dependent Lagrangian Implementation](#sec-1-14-1)
    - [Part B: Applying HO-Lagrangians](#sec-1-14-2)
    - [Part C: Generalized Lagrange Equations](#sec-1-14-3)
  - [Exercise 1.14: Coordinate-independence of Lagrange equations](#sec-1-15)
  - [Exercise 1.15: Equivalence](#sec-1-16)
    - [Definitions and Preliminaries](#sec-1-16-1)
    - [Approach](#sec-1-16-2)
    - [Scheme Tools](#sec-1-16-3)
    - [Derivation](#sec-1-16-4)
    - [Scheme Derivation](#sec-1-16-5)
    - [Final Comments](#sec-1-16-6)
  - [Exercise 1.16: Central force motion](#sec-1-17)
    - [Analytic Approach](#sec-1-17-1)
    - [Scheme Approach](#sec-1-17-2)
    - [Discussion](#sec-1-17-3)
  - [Exercise 1.17: Bead on a helical wire](#sec-1-18)
  - [Exercise 1.18: Bead on a triaxial surface](#sec-1-19)
  - [Exercise 1.19: Two-bar linkage](#sec-1-20)
  - [Exercise 1.20: Sliding pendulum](#sec-1-21)
  - [Exercise 1.21: A dumbbell](#sec-1-22)
    - [Multiple Particle API](#sec-1-22-1)
    - [Part A: Newton's Equations](#sec-1-22-2)
    - [Part B: Dumbbell Lagrangian](#sec-1-22-3)
    - [Part C: Coordinate Change](#sec-1-22-4)
    - [Part D: Substitute \\(c(t) = l\\)](#sec-1-22-5)
    - [Part E: New Lagrangian](#sec-1-22-6)
  - [Exercise 1.22: Driven pendulum](#sec-1-23)
    - [Part A: Newton's Equations](#sec-1-23-1)
    - [Part B: Lagrangian](#sec-1-23-2)
    - [Part C: Coordinate Change](#sec-1-23-3)
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

This exercise is designed to get you thinking about the idea of *configuration space*. One goal of classical mechanics is to predict the future of a system, based on a description of the current state of the system. So how to describe the system?

One way would be to track, for every instant of time, a 3-dimensional position for every particle in a system, along with velocities in each direction, for a total of 6 numbers - or *dimensions* - per particle.

That's fine for particles moving in straight lines through space, not affecting each other. But remember, this is all an accounting trick that we're using to represent reality, not reality itself. If there is some more convenient way to track the state of the system, we're free to use that as well, provided we can recover the original positions and velocities after evolving the system.

Maybe two particles are attached to each other, so their positions in space are the same, or offset by a tiny amount. Then it's enough to track:

-   the position of one of the particles (3 numbers)
-   the angle and distance of the offset (2 numbers)

What seemed like a system that required 6 numbers actually required 5.

Theo Jansen's incredible [Strandbeesten](https://www.youtube.com/watch?v=LewVEF2B_pM) are built out of copies of [Jansen's Linkage](https://en.wikipedia.org/wiki/Jansen%27s_linkage). Each of these legs has 11 pipes that flex and bend, but only **one** degree of freedom.

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-23_14-48-36_screenshot.png)

The first exercise gives us some practice thinking about the redundancy in different physical systems.

> For each of the mechanical systems described below, give the number of degrees of freedom of the configuration space. ([SICM, ex1](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-1))

[Exercise 1.2](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-2) asks about the "generalized coordinates" of each system. What are the actual numbers that we want to track for each system, if not the \\(3N\\) positions of each of \\(N\\) particles?

> For each of the systems in exercise 1.1, specify a system of generalized coordinates that can be used to describe the behavior of the system.

1.  Three juggling pins.

    The system has ****18 degrees of freedom****. Each pin requires 3 coordinates to specify its center of mass, and 3 angles for each pin. If you assume that each pin is symmetric about its central axis, then it doesn't matter how far around the pin has rotated and you can make do with ****15 degrees of freedom****. 3 positions and 2 angles for each.

2.  A spherical pendulum consisting of a point mass (the pendulum bob) hanging from a rigid massless rod attached to a fixed support point. The pendulum bob may move in any direction subject to the constraint imposed by the rigid rod. The point mass is subject to the uniform force of gravity.

    This system has only ****2 degrees of freedom****. One for the latitude of the pendulum, and one for the longitude.

3.  A spherical double pendulum, consisting of one point mass hanging from a rigid massless rod attached to a second point mass hanging from a second massless rod attached to a fixed support point. The point masses are subject to the uniform force of gravity.

    ****4 degrees of freedom****; two angles from previous, plus two more angles for the second pendulum off of the first.

4.  A point mass sliding without friction on a rigid curved wire.

    ****1 degree of freedom****; the distance along the wire.

5.  A top consisting of a rigid axisymmetric body with one point on the symmetry axis of the body attached to a fixed support, subject to a uniform gravitational force.

    This system seems to have ****2 degrees of freedom****, for the angles off of vertical. It's like a spherical pendulum, but upside down. What I find strange about this answer is that the top does have a rotation speed, which is a measure of how far the top has rotated in time. How can we track this velocity if we don't track the top's spin angle? This may be wrong.

6.  The same as **5**, but not axisymmetric.

    A non-symmetric top has ****3 degrees of freedom****. 2 from before, and an additional angle to measure how far around the top has rotated.

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

I don't plan on doing this for every section in the book, but section 1.4 is the first place where we're introduced to Scheme, so I followed along and made a few notes.

This is the first demo of how any of this stuff works, starting on page 15. Here's our first Lagrangian, super simple.

```scheme
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))
```

`L-free-particle` is a function that takes some `mass` and returns a *new* function. The new function takes an instance of a "local tuple" and returns the value of the "Lagrangian". This is the function that you query at every point along some evolving path in configuration space. For realizable physical paths, the integral of this function should by minimized, or stationary.

Why? That's what we're trying to develop here.

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

Composing the Langrangian with \\(\Gamma[q]\\) gives you a function that computes the Lagrangian at some instant:

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

Calculate the action for a particle of mass 3, between \\(t\_1 = 0\\) and \\(t\_2 = 10\\):

```scheme
(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
```

    #| 435. |#

This happens to be the minimal action, since the path we provided was a uniform path and the Lagrangian was for a free particle. If we'd provided a different path, we would still get an action. Just not a stationary action. Infinitesimal wiggles would change the action.

## Exercise 1.4: Lagrangian actions<a id="sec-1-4"></a>

This exercise has us calculating the actual value of the action along some realizable path taken by a free particle.

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

I'm not sure I see the point of this exercise, for developing intuition about Langrangian mechanics. I think it may be here to make sure we understand that we're not minimizing the *function* \\(L\\). We're minimizing (finding the stationary point of) the integral of \\(L\\) between \\(t\_a\\) and \\(t\_b\\).

The velocity is constant between the two points, so it must be equal to the difference in position over the difference in time:

\begin{equation}
\label{eq:constant-v}
v = {{x(t\_b) - x(t\_a)} \over {t\_b - t\_a}} = {{x\_b - x\_a} \over {t\_b - t\_a}}
\end{equation}

The action is equal to the integral of \\(L\\) evaluated between the two time points:

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

Boom, solution achieved.

## Paths of Minimum Action<a id="sec-1-5"></a>

This section takes us through an example action calculation on a path with an adjustable "variation", or wiggle. We should see that, if we consider a "realizable path", then any wiggle we add will increase the calculated action.

The only restriction on a variation is that it can't affect the endpoints of the realizable path. the times and positions of the start and end of the path are pinned.

`make-eta` takes some path \\(\nu\\) and returns a function that wiggles in some similar way to \\(\nu\\), but equals 0 at \\(t\_1\\) and \\(t\_2\\):

```scheme
(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))
```

Next, define a function that calculates the Lagrangian for a free particle, like before, but adds in the path variation \\(\eta\\) multiplied by some small scaling factor \\(\epsilon\\):

```scheme
(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* epsilon eta))
                       t1
                       t2)))
```

Consider some variation like \\(v(t) = (\sin(t), \cos(t), t^2)\\). The action of the path with this small wiggle (processed through `make-eta` to pin its endpoints) is larger (top entry) than the action of the non-varied path (bottom entry), as expected:

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

\#| test-path |#

\begin{equation}
\begin{pmatrix} \displaystyle{ 436.2912142857153} \cr \cr \displaystyle{ 435.}\end{pmatrix}
\end{equation}

What value of \\(\epsilon\\) minimizes the action for the test path?

We can search over values of \\(\epsilon\\) from \\(-2.0\\) to \\(1.0\\) using the built-in `minimize` function:

```scheme
(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (->tex-equation
   (car (minimize action-fn -2.0 1.0))))
```

\begin{equation}
5.134781488891349e-15
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

The next function sort-of-composes `make-path` and `Lagrangian-action` into a function that takes \\(L\\) and the endpoints, and returns the total action along the path.

```scheme
(define ((parametric-path-action L t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action L path t0 t1)))
```

Finally, `find-path` takes the previous function's arguments, plus a parameter \\(n\\). \\(n\\) controls how many intermediate points the optimizer will inject and modify in its attempt to find an action-minimizing path. The more points you specify, the longer minimization will take, but the more accurate the final path will be.

```scheme
(define (find-path L t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action L t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))
```

Let's test it out with a Lagrangian for a one dimensional harmonic oscillator with spring constant \\(k\\). Here is the Lagrangian, equal to the kinetic energy minus the potential from the spring:

```scheme
(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))
```

    #| L-harmonic |#

Now we invoke the procedures we've built, and plot the final, path-minimizing trajectory.

```scheme
(define harmonic-path
  (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 3))

(define win2 (frame 0.0 :pi/2 0 1))

(plot-function win2 harmonic-path 0 :pi (/ :pi 100))
```

The path looks like a harmonic oscillator that starts high and bounces down, after \\(\pi \over 2\\) seconds, down to 0. This is the first quarter of a sine wave with period \\(2 \pi\\).

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_14-24-14_screenshot.png)

## Exercise 1.5: Solution process<a id="sec-1-6"></a>

The goal of this The goal of this exercise is to watch the minimization process that we just discussed proceed, from the initial guess of a straight-line path to the final, natural looking harmonic oscillation.

The exercise states:

> We can watch the progress of the minimization by modifying the procedure parametric-path-action to plot the path each time the action is computed.

The functions the authors provide in the exercise define a window, and then a version of `parametric-path-action` that updates the graph as it minimizes:

```scheme
(define win2 (frame 0.0 :pi/2 0.0 1.2))

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1)
         intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
    ;; display path
    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
    ;; compute action
    (Lagrangian-action Lagrangian path t0 t1)))
```

Run the minimization with the same parameters as in the previous section:

```scheme
(find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 2)
```

and watch the plot update:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-05-29_10-12-19_AJBpDgU.gif)

## Exercise 1.6: Minimizing action<a id="sec-1-7"></a>

The authors have lightly demonstrated that plausible-looking paths have stationary action between fixed endpoints. What happens if we overconstrain the problem?

The exercise asks:

> Suppose we try to obtain a path by minimizing an action for an impossible problem. For example, suppose we have a free particle and we impose endpoint conditions on the velocities as well as the positions that are inconsistent with the particle being free. Does the formalism protect itself from such an unpleasant attack? You may find it illuminating to program it and see what happens.

I spent a good amount of time thinking about this exercise. When I attempted to read this book in 2015, I found it very confusing.

Let's say you take, as the authors suggest, some path, and impose velocity constraints on the endpoints in addition to the required position constraints.

Usually, you constrain the coordinates at each endpoint and force a path that minimizes the action between two times. What does it mean to impose velocity conditions?

The key is to realize that on the computer, you're forcing a path to be composed of a bunch of discrete points. If you can force a point into the path that is NOT controlled by the optimizer, then you can force a velocity at some point in the path that makes no sense for minimal action.

Let's define a new version of `parametric-path-action` that also takes an offset for the initial and final points. We'll force the first and last intermediate point to be equal to the start and end points, plus some offset we can supply to the function.

Then, we can try to find an action-minimizing path, but force the optimizer to deal with not just our endpoint conditions, but these two extra points as well. Forcing two points on each end will force an initial velocity condition. An offset of 0 would be equivalent to imposing a velocity of 0 at the start.

### Intuition<a id="sec-1-7-1"></a>

I simply proceeded with the implementation, but I'd recommend you take a minute to consider what you *think* will happen here. A hint is that the code is attempting to minimize action, given the constraint of the actual Lagrangian. What will it do when it's forced to battle with a new exterior constraint, not captured in the Lagrangian?

### Implementation<a id="sec-1-7-2"></a>

Here's the implementation of the modification described earlier:

```scheme

(define (((parametric-path-action* win)
          Lagrangian t0 q0 offset0 t1 q1 offset1)
         intermediate-qs)
  ;; See the two new points?
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

You might try a similar trick by modifying the first and last entries of `intermediate-qs` instead of appending a point, but I suspect that the optimizer would be able to figure out how to undo your offset. (Try this as an exercise.)

Next, a new version of `find-path` that passes the offsets through to the new `parametric-path-action*`:

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

### Execution<a id="sec-1-7-3"></a>

Let's run the code with 0 offsets and 3 interpolation points. Note that this should *still* distort the path, since we now have two fixed points at the start and end. This is effectively imposing a 0 velocity constraint at the beginning and end.

Here's the code, and its output:

```scheme
(one-six 0 0 3)
```

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_15-10-46_ex1_6_nooffset.gif)

The path ends up looking almost sinusoidal, and takes a while to converge. This is the best polynomial that the system can come up with that matches the 7 points (3 interpolated, 2 offsets, 1 start and 1 end).

The actual realizable path should be a straight line between the two points. The initial velocity of

Here's a small positive velocity imposed at the beginning, and 0 at the end:

```scheme
(one-six 0.2 0 3)
```

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_15-10-53_ex1_6_02offset.gif)

The system takes longer to converge. Here's a larger impulse of 0.5 at the beginning:

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
(one-six -0.5 0 10)
```

The optimization freezes.

What is going on here? Why does the minimizer converge?

With velocity constraints imposed, we're no longer minimizing the action with respect to some Lagrangian. We're minimizing the action given two constraints. You have the Lagrangian, and then the warring goal of the polynomial interpolation forcing a certain shape on the path. At some point, the minimizer breaks; internally it ends up pinned between two tugging constraints.

If you make the impulse too big or force too many intermediate points, then the war is too hardcore and the process never converges. But it's important to note here that these are details of the computational process. This detail doesn't break reality. (It would break your model of reality, as it did here, if you have constraints or forces that you don't capture in the Lagrangian.)

If you *do* need to impose velocity conditions, it turns out you can use a Lagrangian that takes acceleration into account. This is discussed in Exercise 1.10.

## Exercise 1.7: Properties of \\(\delta\\)<a id="sec-1-8"></a>

This exercise asks us to prove various products of the variation operator \\(\delta\_\eta\\). This is a sort of higher-order derivative operator. Apply it to a higher order function \\(f\\), and you'll get a function back that returns the *sensitivity* of \\(f\\) to fluctuations in its input path function. (Confusing? Check out [the textbook](https://tgvaughan.github.io/sicm/chapter001.html#h1-6a).)

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

The sum rule is easier. Our goal is to show that:

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

\\(fn\\) takes a symbol like \\(F\\) and a path function &#x2013; a function from \\(t\\) to any number of coordinates (see the `UP*`?) &#x2013; and returns a generic expression for a path dependent function \\(F\\) that acts via \\(F \circ \Gamma[q]\\). \\(F\\) might be a Lagrangian, for example.

The textbook also gives us this function from \\(t \to (x, y)\\) to test out the properties above. I've added an \\(\eta\\) of the same type signature that we can use to add variation to the path.

```scheme
(define q (literal-function 'q (-> Real (UP Real Real))))
(define eta (literal-function 'eta (-> Real (UP Real Real))))
```

These weren't easy to write down, but they really do constitute proofs. If you trust the system managing the algebra, then the equalities here are general. This is an area of computing I haven't worked with much, but I'm left with the eery feeling that these are more powerful than any tests I might have decided to write, if I weren't guided by this exercise.

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

This exercise asks us to derive Lagrange's equations in steps for three different systems. Is this conscionable, given that we have the computer available to do the algebra for us? I think that this exercise is good practice for understanding the syntax, and maybe for refreshing your memory of how to symbolically manipulate derivatives.

But I'm feeling more and more that we're in the middle of a thicket of exercises that are smugly attempting to show us how bad life is with pencil and paper, and how nice a computer algebra system can be. These paper-and-pencil problems are going to get harder and harder, while they stay trivial in Scheme.

Decide for yourself. Exercise 1.12 solves implements each Lagrangian in Scheme and demonstrates the steps required to get to Lagrange's equations; I do buy that it would be difficult to do this without a good handle on the syntax.

Give it a try, then go examine exercise 1.12.

## Exercise 1.10: Higher-derivative Lagrangians<a id="sec-1-11"></a>

The only reason that the Lagrangians we've been considering don't take any local tuple components beyond velocity is that the physics of our universe seems concerned with updating velocities and nothing beyond. Newton's second law gives us an update rule for the velocities in a system, and we picked the Lagrangian so that Lagrange's equations would match Newton's second law.

But the formula for action works just as well if the Lagrangian takes many, or infinite, derivatives of the original coordinates. This exercise asks us to:

> Derive Lagrange's equations for Lagrangians that depend on accelerations. In particular, show that the Lagrange equations for Lagrangians of the form \\(L(t, q, \dot{q}, \ddot{q})\\) with \\(\ddot{q}\\) terms are
>
> \begin{equation}
>   D^2(\partial\_3L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]) + (\partial\_1L \circ \Gamma[q]) = 0
> \end{equation}

In other words, find the constraint that has to be true of the Lagrangian for the action to be stationary along the supplied path.

This derivation follows the derivation of the Lagrange equations from the book, starting on page 28. Begin with the equation for action with an acceleration argument to \\(L\\):

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

Expand the right side out using the chain rule for variations, equation \eqref{eq:var-chain}:

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

Our goal now is to find some quantity inside the integral that doesn't depend on \\(\eta\\). Setting that quantity to \\(0\\) will give us the Lagrange equations. Focus on the \\(\partial\_2 L\\) term:

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

The original Lagrange equations are peeking at us, multiplied by \\(\eta\\).

Can we get another term into that form and move it in with the original Lagrange equation terms? Take the next term and integrate by parts twice:

\begin{equation}
  \begin{aligned}
    \int\_{t\_a}^{t\_b} (\partial\_3L \circ \Gamma[q])D^2\eta & = (\partial\_3L \circ \Gamma[q])D\eta \Bigr|\_{t\_a}^{t\_b} - \int\_{t\_a}^{t\_b} D(\partial\_2L \circ \Gamma[q])D\eta \cr
    & = (\partial\_3L \circ \Gamma[q])D\eta \Bigr|\_{t\_a}^{t\_b} - D(\partial\_2L \circ \Gamma[q])\eta \Bigr|\_{t\_a}^{t\_b} + \int\_{t\_a}^{t\_b} D^2(\partial\_2L \circ \Gamma[q])\eta
  \end{aligned}
\end{equation}

The second of the two definite evaluations disappears, since, as before, \\(\eta(t\_a) = \eta(t\_b) = 0\\). The first of the definite evaluations suggests that we need a new constraint to achieve higher-derivative Lagrange equations.

If we require \\(D\eta(t\_a) = D\eta(t\_b) = 0\\), then the first definite evaluation disappears as well. So, for a Lagrangian that considers acceleration, we have to impose the constraint that the path's endpoint velocities can't vary. The path-wiggle's endpoints can't be in motion.

The term collapses to:

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

The goal was to find conditions under which the action is stationary, ie, \\(\delta\_\eta S[q](t\_a, t\_b) = 0\\). For arbitrary \\(\eta\\) with fixed endpoints, this can only occur if the non-\\(\eta\\) factor inside the integral is 0:

\begin{equation}
(\partial\_1L \circ \Gamma[q]) - D(\partial\_2L \circ \Gamma[q]) + D^2(\partial\_3L \circ \Gamma[q]) = 0
\end{equation}

This is the result we were seeking.

### Higher dimensions<a id="sec-1-11-1"></a>

To keep going, we have to integrate by parts once more for each new term of the local tuple that the Lagrangian depends on. For each new term we gain a new constraint:

\begin{equation}
D^{n-1}\eta(t\_a) = D^{n-1}\eta(t\_b) = 0
\end{equation}

And a new term in the ever-higher-dimensional Lagrange's equations:

\begin{equation}
  (-1)^{n} D^{n}(\partial\_{n+1}L \circ \Gamma[q])
\end{equation}

The fully general Lagrange's equations are, for a Lagrangian that depends on the local tuple up to the $n$th derivative of \\(q\\):

\begin{equation}
  0 = \sum\_{i = 0}^n(-1)^i D^{i}(\partial\_{i+1}L \circ \Gamma[q])
\end{equation}

Constrained by, for all \\(i\\) from 0 to \\(n-1\\):

\begin{equation}
D^i\eta(t\_a) = D^i\eta(t\_b) = 0
\end{equation}

Equivalently, the constraint is that all derivatives of \\(q\\) from \\(i\\) to \\(n-1\\) must remain constant at \\(t\_a\\) and \\(t\_b\\).

Exercise 1.13 implements a procedure that generates the residual required by these higher-dimensional Lagrange equations in Scheme.

## Exercise 1.11: Kepler's third law<a id="sec-1-12"></a>

This exercise asks us to derive [Kepler's third law](https://en.wikipedia.org/wiki/Kepler%27s_laws_of_planetary_motion#Third_law_of_Kepler) by considering a Langrangian that describes two particles rotating in a circular orbit around their center of mass at some rate.

Here's the Lagrangian for "central force", in polar coordinates. This is rotational kinetic energy, minus some arbitrary potential \\(V\\) that depends on the distance \\(r\\) between the two particles.

```scheme
(define ((L-central-polar m V) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0))     (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
            (+ (square rdot) (square (* r phidot))))
         (V r)))))
```

This function defines gravitational potential energy:

```scheme
(define ((gravitational-energy G m1 m2) r)
  (- (/ (* G m1 m2) r)))
```

What is the mass \\(m\\) in the Lagrangian above? It's the "[reduced mass](https://en.wikipedia.org/wiki/Reduced_mass)", totally unjustified at this point in the book:

```scheme
(define (reduced-mass m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))
```

If you want to see why the reduced mass has the form it does, check out [this derivation](https://en.wikipedia.org/wiki/Reduced_mass#Lagrangian_mechanics).

The Lagrangian is written in terms of some angle \\(\phi\\) and \\(r\\), the distance between the two particles. \\(q\\) defines a circular path:

```scheme
(define ((q r omega) t)
  (let ((phi (* omega t)))
    (up r phi)))
```

Write the Lagrange equations, given \\(r = a\\) and \\(\omega = n\\):

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

These two entries are *residuals*, equal to zero. Stare at the top residual and you might notice that you can can factor out:

-   the reduced mass, and
-   a factor of \\(1 \over a^2\\)

Manually factor these out:

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

And, boom, with some cleanup, we see Kepler's third law:

\begin{equation}
\label{eq:kepler3}
n^2a^3 = G(m\_1 + m\_2)
\end{equation}

## Exercise 1.12: Lagrange's equations (code)<a id="sec-1-13"></a>

This exercise asks us to write Scheme implementations for each of the three systems described in [Exercise 1.9](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-9).

Before we begin, here is a function that will display an up-tuple of:

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

These are the steps required on the road to a derivation of Lagrange's equations.

### Preliminary Notes<a id="sec-1-13-1"></a>

I found this exercise to be challenging because I was searching for a particular elegant form of the Lagrange equations for each system. Because the Lagrange equations are residuals, any linear combination of the equations also has to equal 0. In a few of the exercises below, I reached a solution that was technically correct, but cluttered.

If I were using Lagrangian mechanics to develop a game, or to simulate motion in some virtual world, I would just move on. But it seems that one of the tasks for the learner in modern physics is to take transferable lessons from the equations, and this means that it's important to try and unmask terms that might appear in different systems under superficially different forms.

Exercise 1.14 has an example of this problem that took me a long time to notice. The systems we analyze here happen to have yielded nice, familiar solutions. But note now that this is not a gimme.

### Part A: Ideal Planar Pendulum<a id="sec-1-13-2"></a>

From the book:

> An ideal planar pendulum consists of a bob of mass \\(m\\) connected to a pivot by a massless rod of length \\(l\\) subject to uniform gravitational acceleration \\(g\\). A Lagrangian is \\(L(t, \theta, \dot{\theta}) = {1 \over 2} ml^2\dot{\theta}^2 + mgl\cos \theta\\). The formal parameters of \\(L\\) are \\(t\\), \\(\theta\\), and \\(\dot{\theta}\\); \\(\theta\\) measures the angle of the pendulum rod to a plumb line and \\(\dot{\theta}\\) is the angular velocity of the rod.

Here is the Lagrangian described by the exercise:

```scheme
(define ((L-pendulum m g l) local)
  (let ((theta (coordinate local))
        (theta_dot (velocity local)))
    (+ (* 1/2 m (square l) (square theta_dot))
       (* m g l (cos theta)))))
```

And the steps that lead us to Lagrange's equations:

```scheme
(lagrange-equation-steps
 (L-pendulum 'm 'g 'l)
 (literal-function 'theta))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ \left( - 1 \right) g l m \sin\left( \theta\left( t \right) \right)} \cr \cr \displaystyle{ {l}^{2} m D\theta\left( t \right)} \cr \cr \displaystyle{ {l}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ g l m \sin\left( \theta\left( t \right) \right) + {l}^{2} m {D}^{2}\theta\left( t \right)}\end{pmatrix}
\end{equation}

The final entry is the Lagrange equation, equal to \\(0\\). Divide out the shared factors of \\(m\\) and \\(l\\):

```scheme
(let* ((L (L-pendulum 'm 'g 'l))
       (theta (literal-function 'theta))
       (eqs ((Lagrange-equations L) theta)))
  (->tex-equation
   ((/ eqs (* 'm 'l))
    't)))
```

\begin{equation}
g \sin\left( \theta\left( t \right) \right) + l {D}^{2}\theta\left( t \right)
\end{equation}

This is the [familiar equation of motion](https://en.wikipedia.org/wiki/Pendulum_(mathematics)) for a planar pendum.

### Part B: 2D Potential<a id="sec-1-13-3"></a>

The next problem is in rectangular coordinates. This means that we'll end up with two Lagrange equations that have to be satisfied.

From the book:

> A particle of mass \\(m\\) moves in a two-dimensional potential \\(V(x, y) = {(x^2 + y^2) \over 2} + x^2 y - {y^3 \over 3}\\), where \\(x\\) and \\(y\\) are rectangular coordinates of the particle. A Lagrangian is \\(L(t;x, y; v\_x, v\_y) = {1 \over 2} m (v\_x^2 + v\_y^2) - V(x, y)\\).

I have no intuition for *what* this potential is, by the way. One term, \\({x^2 + y^2} \over 2\\), looks like half the square of the distance of the particle away from 0, or \\({1 \over 2} r^2\\). What are the other terms? I've been so well trained that I simply start calculating.

Define the Lagrangian to be the difference of the kinetic energy and some potential \\(V\\) that has access to the coordinates:

```scheme
(define (((L-2d-potential m) V) local)
  (- (* 1/2 m (square (velocity local)))
     (V (coordinate local))))
```

Thanks to the tuple algebra of `scmutils`, This form of the Lagrangian is general enough that it would work for any number of dimensions in rectangular space, given some potential \\(V\\). `square` takes a dot product, so we end up with a kinetic energy term for every spatial dimension.

Note this for later, as this idea will become useful when the book reaches the discussion of coordinate transformations.

Next define the potential from the problem description:

```scheme
(define (V q)
  (let ((x (ref q 0))
        (y (ref q 1)))
    (- (+ (/ (+ (square x)
                (square y))
             2)
          (* (square x) y))
       (/ (cube y) 3))))
```

Our helpful function generates the Lagrange equations, along with each intermediate step:

```scheme
(lagrange-equation-steps
 ((L-2d-potential 'm) V)
 (up (literal-function 'x)
     (literal-function 'y)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{  - 2 y\left( t \right) x\left( t \right) - x\left( t \right)} \cr \cr \displaystyle{ {\left( y\left( t \right) \right)}^{2} - {\left( x\left( t \right) \right)}^{2} - y\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ m Dx\left( t \right)} \cr \cr \displaystyle{ m Dy\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ m {D}^{2}x\left( t \right)} \cr \cr \displaystyle{ m {D}^{2}y\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ m {D}^{2}x\left( t \right) + 2 y\left( t \right) x\left( t \right) + x\left( t \right)} \cr \cr \displaystyle{ m {D}^{2}y\left( t \right) - {\left( y\left( t \right) \right)}^{2} + {\left( x\left( t \right) \right)}^{2} + y\left( t \right)}\end{bmatrix}}\end{pmatrix}
\end{equation}

The final down-tuple gives us the Lagrange equations that \\(x\\) and \\(y\\) (respectively) must satisfy.

### Part C: Particle on a Sphere<a id="sec-1-13-4"></a>

This problem is slightly more clear. From the book:

> A Lagrangian for a particle of mass \\(m\\) constrained to move on a sphere of radius \\(R\\) is \\(L(t; \theta, \phi; \alpha, \beta) = {1 \over 2} m R^2(\alpha^2+(\beta \sin\theta)^2)\\). The angle \\(\theta\\) is the colatitude of the particle and \\(\phi\\) is the longitude; the rate of change of the colatitude is \\(\alpha\\) and the rate of change of the longitude is \\(\beta\\).

So the particle has some generalized kinetic energy with terms for:

-   its speed north and south, and
-   its speed east and west, scaled to be strongest at 0 longitude along the \\(x\\) axis and fall off to nothing at the \\(y\\) axis.

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

Here is the full derivation:

```scheme
(lagrange-equation-steps
 (L-sphere 'm 'R)
 (up (literal-function 'theta)
     (literal-function 'phi)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2}} \cr \cr \displaystyle{ 0}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m D\theta\left( t \right)} \cr \cr \displaystyle{ {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} D\phi\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{ {R}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {R}^{2} m \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\phi\left( t \right)}\end{bmatrix}} \cr \cr \displaystyle{ \begin{bmatrix} \displaystyle{  - {R}^{2} m \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + {R}^{2} m {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {R}^{2} m \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) \cos\left( \theta\left( t \right) \right) D\phi\left( t \right) + {R}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\phi\left( t \right)}\end{bmatrix}}\end{pmatrix}
\end{equation}

The final Lagrange residuals have a few terms that we can divide out. Scheme doesn't know that these are meant to be residuals, so it won't cancel out factors that we can see by eye are missing.

Isolate the Lagrange equations from the derivation and manually simplify each equation by dividing out, respectively, \\(mR^2\\) and \\(mR^2 \sin \theta\\):

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

These are the Lagrange equations for \\(\theta\\) and \\(\phi\\), respectively.

## Exercise 1.13: Higher-derivative Lagrangians (code)<a id="sec-1-14"></a>

This exercise completes exercise 1.10 by asking for implementations of the higher-order Lagrange equations that we derived. This was a nice Scheme exercise; I would argue that this implementation should exist in the standard library. But that would ruin the fun of the exercise&#x2026;

### Part A: Acceleration-dependent Lagrangian Implementation<a id="sec-1-14-1"></a>

From the book:

> Write a procedure to compute the Lagrange equations for Lagrangians that depend upon acceleration, as in exercise 1.10. Note that Gamma can take an optional argument giving the length of the initial segment of the local tuple needed. The default length is 3, giving components of the local tuple up to and including the velocities.

Now that we know the math, the implementation is a straightforward extension of the `Lagrange-equations` procedure presented in the book:

```scheme
(define ((Lagrange-equations3 L) q)
  (let ((state-path (Gamma q 4)))
    (+ ((square D) (compose ((partial 3) L) state-path))
       (- (D (compose ((partial 2) L) state-path)))
       (compose ((partial 1) L) state-path))))
```

### Part B: Applying HO-Lagrangians<a id="sec-1-14-2"></a>

Now it's time to use the new function. From the book:

> Use your procedure to compute the Lagrange equations for the Lagrangian
>
> \begin{equation}
> L(t, x, v, a) = - {1 \over 2}mxa - {1 \over 2}kx^2
> \end{equation}
>
> Do you recognize the resulting equation of motion?

Here is the Lagrangian described in the problem:

```scheme
(define ((L-1-13 m k) local)
  (let ((x (coordinate local))
        (a (acceleration local)))
    (- (* -1/2 m x a)
       (* 1/2 k (square x)))))
```

Use the new function to generate the Lagrange equations. This call includes a factor of \\(-1\\) to make the equation look nice:

```scheme
(->tex-equation
 (- (((Lagrange-equations3 (L-1-13 'm 'k))
      (literal-function 'x)) 't)))
```

\begin{equation}
k x\left( t \right) + m {D}^{2}x\left( t \right)
\end{equation}

This looks like the equation of motion for a [classical harmonic oscillator](https://en.wikipedia.org/wiki/Harmonic_oscillator). Again, I leave this problem with no new physical intuition for what is going on here, or what type of system would need an acceleration dependent Lagrangian. I suspect that we could build a harmonic oscillator for Lagrange equations of any order by properly tuning the Lagrangian. But I don't know why this would be helpful.

### Part C: Generalized Lagrange Equations<a id="sec-1-14-3"></a>

Now, some more fun with Scheme. It just feels nice to implement Scheme procedures. From the book:

> For more fun, write the general Lagrange equation procedure that takes a Lagrangian that depends on any number of derivatives, and the number of derivatives, to produce the required equations of motion.

As a reminder, this is the equation that we need to implement for each coordinate:

\begin{equation}
  0 = \sum\_{i = 0}^n(-1)^i D^{i}(\partial\_{i+1}L \circ \Gamma[q])
\end{equation}

There are two ideas playing together here. Each term takes an element from:

-   an alternating sequence of \\(1\\) and \\(-1\\)
-   a sequence of increasing-index \\(D^i(\partial\_i L \circ \Gamma[q])\\) terms

The alternating \\(1, -1\\) sequence is similar to a more general idea: take any ordered collection arranged in a cycle, start at some point and walk around the cycle for \\(n\\) steps.

If you need to take \\(n\\) steps along a cycle of length \\(l\\), you'll end up traveling around the cycle between \\(n \over l\\) and \\({n \over l} + 1\\) times.

`alternate` generates a list of \\(n\\) total elements generated by walking around the ordered cycle of supplied `elems`:

```scheme
(define (cycle n elems)
  (apply append (make-list n elems)))

(define (alternating n elems)
  (let* ((l (length elems))
         (times (quotient (+ n (-1+ l)) l)))
    (list-head (cycle times elems) n)))
```

Now, the general `Lagrange-equations*` implementation.

This function defines an internal function `term` that generates the $i$th term of the derivative combination described above. This sequence is zipped with the sequence of \\(1, -1\\), and `fold-left` generates the sum.

```scheme
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

Generate the Lagrange equations from part b once more to check that we get the same result:

```scheme
(->tex-equation
 (- (((Lagrange-equations* (L-1-13 'm 'k) 3)
      (literal-function 'x)) 't)))
```

\begin{equation}
k x\left( t \right) + m {D}^{2}x\left( t \right)
\end{equation}

There it is again, the harmonic oscillator. I don't have any intuition for higher order Lagrangians, so I can't cook up any further examples to test the implementation.

## Exercise 1.14: Coordinate-independence of Lagrange equations<a id="sec-1-15"></a>

Look carefully at what this exercise is asking us to do:

> Check that the Lagrange equations for central force motion in polar coordinates and in rectangular coordinates are equivalent. Determine the relationship among the second derivatives by substituting paths into the transformation equations and computing derivatives, then substitute these relations into the equations of motion.

The punchline that we'll encounter soon is that a coordinate transformation of applied to some path function \\(q\\) can commute with \\(\Gamma\\). You can always write some function \\(C\\) of the local tuple that handles the coordinate transformation *after* \\(\Gamma[q]\\) instead of transforming the path directly. In other words, you can always find some \\(C\\) such that

\begin{equation}
C \circ \Gamma[q] = \Gamma[q']
\end{equation}

Because function composition is associative, instead of ever transforming the path, you can push the coordinate transformation into the Lagrangian to generate a new Lagrangian: \\(L = L' \circ C\\).

The section of textbook just before the exercise has given us two Lagrangians in different coordinates &#x2013; `L-central-polar` and `L-rectangular` &#x2013; and generated Lagrange equations from each.

Our task is to directly transform the Lagrange equations by substituting the first and second derivatives of the coordinate transformation expression into one of the sets of equations, and looking to see that it's equivalent to the other.

Fair warning: this is much more painful than transforming the Lagrangian *before* generating the Lagrange equations. This exercise continues the theme of devastating you with algebra as a way to show you the horror that the later techniques were developed to avoid. Let us proceed.

Here are the two Lagrangians from the book:

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

And the polar Lagrange equations:

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

Once again, our goal is to show that, if you can write down coordinate transformations for the coordinates, velocities and accelerations and substitute them in to one set of Lagrange equations, the other will appear.

To do this by hand, take the coordinate transformation described in 1.64 in the book:

\begin{equation}
  \begin{split}
    x &= r \cos \phi \cr
    y &= r \sin \phi
  \end{split}
\end{equation}

Note that \\(x\\), \\(y\\), \\(r\\) and \\(\phi\\) are functions of \\(t\\). Take the derivative of each equation (Use the product and chain rules) to obtain expressions for the rectangular velocities in terms of the polar coordinates, just like equation 1.66 in the book:

\begin{equation}
  \begin{split}
    Dx(t) &= Dr(t) \cos \phi(t) - r(t) D\phi(t) \sin \phi(t) \cr
    Dy(t) &= Dr(t) \sin \phi(t) + r(t) D\phi(t) \cos \phi(t)
  \end{split}
\end{equation}

The rectangular equations of motion have second derivatives, so we need to keep going. This is too devastating to imagine doing by hand. Let's move to Scheme.

Write the coordinate transformation for polar coordinates to rectangular in Scheme:

```scheme
(define (p->r local)
  (let* ((polar-tuple (coordinate local))
         (r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))
```

Now use `F->C`, first described on page 46. This is a function that takes a coordinate transformation like `p->r` and returns a *new* function that can convert an entire local tuple from one coordinate system to another; the \\(C\\) discussed above.

The version that the book presents on page 46 can only generate a velocity transformation given a coordinate transformation, but `scmutils` contains a more general version that will convert as many path elements as you pass to it.

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

Ordinarily, it would be too heartbreaking to substitute these in to the rectangular equations of motion. The fact that we have Scheme on our side gives me the strength to proceed.

Write the rectangular Lagrange equations as a function of the local tuple, so we can call it directly:

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

Verify that these are, in fact, the rectangular equations of motion by passing in a symbolic rectangular local tuple:

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

Oh no. This looks quite different from the polar Lagrange equations above. What is the problem?

I had to stare at this for a long time before I saw what to do. Notice that the terms we want from the polar Lagrange equations all seem to appear in the first equation with a \\(\cos \phi\\), and in the second equation with a \\(\sin \phi\\). Using the trigonometric identity:

\begin{equation}
(\cos \phi)^2 + (\sin \phi)^2 = 1
\end{equation}

I realized that I could recover the first equation through a linear combination of both terms. Multiply the first by \\(\cos \phi\\) and the second by \\(\sin \phi\\), add them together and the unwanted terms all drop away.

A similar trick recovers the second equation,given an extra factor of \\(r\\):

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

This was a powerful lesson. We're allowed to take a linear combination here because each equation is a residual, equal to zero. \\(a0 + b0 = 0\\) for any \\(a\\) and \\(b\\), so any combination we generate is still a valid residual.

There is something important going on here, with the way we were able to remove \\(\phi\\) completely from the Lagrange equations. It seemed like \\(\phi\\) was quite important, until we managed to kill it. Is this related to the discussions of symmetries that we'll encounter later in the book? Let me know if you know the answer here.

## Exercise 1.15: Equivalence<a id="sec-1-16"></a>

This is one of the more important exercises in the chapter. The problem asks for a proof that it's possible to absorb a coordinate transformation directly into the Lagrangian. If you can do this, you can express your paths and your forces in whatever coordinates you like, so long as you can transition between them.

I also found that this exposed, and repaired, my weakness with the [functional notation](http://fmnt.info/blog/20180228_sicm/index.html#orga0630c4) that Sussman and Wisdom have used in the book.

The problem states:

> Show by direct calculation that the Lagrange equations for \\(L'\\) are satisfied if the Lagrange equations for \\(L\\) are satisfied.

### Definitions and Preliminaries<a id="sec-1-16-1"></a>

\\(L'\\) and \\(L\\) refer to ideas from section 1.6.1. The major promise of Lagrangian mechanics is that both the potential and kinetic energies are coordinate-independent; looking at the system through different coordinates can't affect the energy in the system, or you've dropped some information in the conversation. If this is true, then the value of the Lagrangian, equal to \\(T - V\\), must be coordinate independent too.

Imagine a coordinate transformation \\(F\\), maybe time-dependent, that can convert "primed coordinates" like \\(x'\\) into "unprimed coordinates", \\(x\\):

\begin{equation}
x = F(t, x')
\end{equation}

You might imagine that \\(x'\\) is in polar coordinates and \\(x\\) is in rectangular coordinates. Time-dependence means that the coordinate system itself is moving around in time. Imagine looking at the world out of a spinning window.

Now imagine some Lagrangian \\(L\\) that acts on a local tuple built out of the unprimed coordinates. If the Lagrangian's value is coordinate-independent, there must be some other form, \\(L'\\), that can act on the primed coordinates. \\(L'\\) has to satisfy:

\begin{equation}
L' \circ \Gamma[q'] = L \circ \Gamma[q]
\end{equation}

The function \\(F\\) only acts on specific coordinates. Imagine a function \\(C\\) that uses \\(F\\) to transform the entire local tuple from the primed coordinates to the unprimed coordinates:

\begin{equation}
\label{eq:1-15-relations}
L \circ \Gamma[q] = L \circ C \circ \Gamma[q'] = L' \circ \Gamma[q']
\end{equation}

Function composition is associative, so two facts stare at us from \eqref{eq:1-15-relations}:

\begin{equation}
\begin{aligned}
L' & = L \circ C \cr
\Gamma[q] & = C \circ \Gamma[q']
\end{aligned}
\end{equation}

This implies that if we want describe our path in some primed coordinate system (imagine polar coordinates), but we only have a Lagrangian defined in an unprimed coordinate system (like rectangular coordinates), if we can just write down \\(C\\) we can generate a new Lagrangian \\(L'\\) by composing our old \\(L\\) with \\(C\\). This brings us to the exercise.

The goal is to show that if the Lagrange equations hold in the unprimed coordinate system:

\begin{equation}
\label{eq:1-15-lagrange}
D(\partial\_2L \circ \Gamma[q]) - (\partial\_1L \circ \Gamma[q]) = 0
\end{equation}

Then they hold in the primed coordinate system:

\begin{equation}
\label{eq:lagrange-prime}
D(\partial\_2L' \circ \Gamma[q']) - (\partial\_1L' \circ \Gamma[q']) = 0
\end{equation}

### Approach<a id="sec-1-16-2"></a>

The approach we'll take will be to:

-   Write down the form of \\(C\\), given some arbitrary \\(F\\)
-   start calculating the terms of the Lagrange equations in the primed coordinate system using \\(L' = L \circ C\\)
-   Keep an eye out for some spot where we can use our assumption that the Lagrange equations hold in the unprimed coordinates.

I'll walk through a solution using "pen and paper", then show how we can run this derivation using Scheme to help us along.

First, some Scheme tools that will help us in both cases.

### Scheme Tools<a id="sec-1-16-3"></a>

Equation (1.77) in the book describes how to implement \\(C\\) given some arbitrary \\(F\\). Looking ahead slightly, this is implemented as `F->C` on page 46.

The following function is a slight redefinition that allows us to use an \\(F\\) that takes an explicit \\((t, x')\\), instead of the entire local tuple:

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

Next we define \\(F\\), \\(C\\) and \\(L\\) as described above, as well as `qprime`, a function that can represent our unprimed coordinate path function.

The types here all imply that the path has one real coordinate. I did this to make the types easier to understand; the derivation applies equally well to paths with many dimensions.

```scheme
(define F
  (literal-function 'F (-> (X Real Real) Real)))

(define C (F->C* F))

(define L
  (literal-function 'L (-> (UP Real Real Real) Real)))

(define qprime
  (literal-function 'qprime))
```

When we apply \\(C\\) to the primed local tuple, do we get the transformed tuple that we expect from 1.77 in the book?

```scheme
(->tex-equation
 ((compose C (Gamma qprime)) 't))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix}
\end{equation}

This looks correct. We can also transform the path before passing it to \\(\Gamma\\):

```scheme
(define ((to-q F qp) t)
  (F t (qp t)))
```

Subtract the two forms to see that they're equivalent:

```scheme
(->tex-equations
 ((- (compose C (Gamma qprime))
     (Gamma (to-q F qprime)))
  't))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{pmatrix}
\end{equation}

Now that we know \\(C\\) is correct we can define \\(q\\), the unprimed coordinate path function, and `Lprime`:

```scheme
(define q (to-q F qprime))
(define Lprime (compose L C))
```

### Derivation<a id="sec-1-16-4"></a>

Begin by calculating the components of the Lagrange equations in equation \eqref{eq:lagrange-prime}. Examine the \\(\partial\_2L'\\) term first.

As we discussed above, function composition is associative, so:

\begin{equation}
\label{eq:c-l}
(L \circ C) \circ \Gamma[q'] = L' \circ \Gamma[q'] \implies L' = L \circ C
\end{equation}

Substituting \\(L'\\) from \eqref{eq:c-l} and using the chain rule:

\begin{equation}
  \partial\_2L' = \partial\_2(L \circ C) = ((DL) \circ C) \partial\_2 C
\end{equation}

I found the next step troubling until I became more comfortable with the functional notation.

\\(C\\) is a function that transforms a local tuple. It takes 3 arguments (a tuple with 3 elements, technically) and returns 3 arguments. \\(\partial\_2 C\\) is an up-tuple with 3 entries. Each entry describes the derivative each component of \\(C\\)'s output with respect to the velocity component of the local tuple.

\\(L\\) is a function that transforms the 3-element local to a scalar output. \\(DL\\) is a down-tuple with 3 entries. Each entry describes the derivative of the single output with respect to each entry of the local tuple.

The tuple algebra described in Chapter 9 defines multiplication between an up and down tuple as a dot product, or a "contraction" in the book's language. This means that we can expand out the product above:

\begin{equation}
  (DL \circ C)\partial\_2 C = (\partial\_0L \circ C)(I\_0 \circ \partial\_2 C) + (\partial\_1L \circ C)(I\_1 \circ \partial\_2 C) + (\partial\_2L \circ C)(I\_2 \circ \partial\_2 C)
\end{equation}

\\(I\_0\\), \\(I\_1\\) and \\(I\_2\\) are "selectors" that return that single element of the local tuple.

Example the value of \\(\partial\_2C\\) using our Scheme utilities:

```scheme
(->tex-equation
 (((partial 2) C) (up 't 'xprime 'vprime))
 "eq:p2c")
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ {\partial}\_{1}F\left( t, {x}^\prime \right)}\end{pmatrix}
\label{eq:p2c}
\end{equation}

The first two components are 0, leaving us with:

\begin{equation}
  \partial\_2 L' = (DL \circ C)\partial\_2 C = (\partial\_2L \circ C)(I\_2 \circ \partial\_2 C)
\end{equation}

Compose this quantity with \\(\Gamma[q']\\) and distribute the composition into the product. Remember that \\(C \circ \Gamma[q'] = \Gamma[q]\\):

\begin{equation}
  \begin{aligned}
    \partial\_2L' \circ \Gamma[q'] & = (\partial\_2L \circ C)(I\_2 \circ \partial\_2 C) \circ \Gamma[q'] \cr
    & = (\partial\_2L \circ C \circ \Gamma[q'])(I\_2 \circ \partial\_2 C \circ \Gamma[q']) \cr
    & = (\partial\_2L \circ \Gamma[q])(I\_2 \circ \partial\_2 C \circ \Gamma[q'])
  \end{aligned}
\end{equation}

Take the derivative (with respect to time, remember, from the types):

\begin{equation}
  D(\partial\_2L' \circ \Gamma[q']) = D\left[(\partial\_2L \circ \Gamma[q])(I\_2 \circ \partial\_2 C \circ \Gamma[q'])\right]
\end{equation}

Substitute the second term using \eqref{eq:p2c}:

\begin{equation}
  D(\partial\_2L' \circ \Gamma[q']) = D\left[(\partial\_2L \circ \Gamma[q])\partial\_1F(t, q'(t))\right]
\end{equation}

Expand using the product rule:

\begin{equation}
\label{eq:ex1-15-dp2l}
  D(\partial\_2L' \circ \Gamma[q']) = \left[ D(\partial\_2L \circ \Gamma[q]) \right]\partial\_1F(t, q'(t)) + (\partial\_2L \circ \Gamma[q])D\left[ \partial\_1F(t, q'(t)) \right]
\end{equation}

A term from the unprimed Lagrange's equation is peeking. Notice this, but don't make the substitution just yet.

Next, expand the \\(\partial\_1 L'\\) term:

\begin{equation}
  \partial\_1L' = \partial\_1(L \circ C) = ((DL) \circ C) \partial\_1 C
\end{equation}

Calculate \\(\partial\_1C\\) using our Scheme utilities:

```scheme
(->tex-equation
 (((partial 1) C) (up 't 'xprime 'vprime)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ {\partial}\_{1}F\left( t, {x}^\prime \right)} \cr \cr \displaystyle{ {v}^\prime {{\partial}\_{1}}^{2}\left( F \right)\left( t, {x}^\prime \right) + \left( {\partial}\_{0} {\partial}\_{1} \right)\left( F \right)\left( t, {x}^\prime \right)}\end{pmatrix}
\end{equation}

Expand the chain rule out and remove 0 terms, as before:

\begin{equation}
  \begin{aligned}
    \partial\_1L' & = ((DL) \circ C) \partial\_1 C \cr
    & = (\partial\_0L \circ C)(I\_0 \circ \partial\_1 C) + (\partial\_1L \circ C)(I\_1 \circ \partial\_1 C) + (\partial\_2L \circ C)(I\_2 \circ \partial\_1 C) \cr
    & = (\partial\_1L \circ C)(I\_1 \circ \partial\_1 C) + (\partial\_2L \circ C)(I\_2 \circ \partial\_1 C)
  \end{aligned}
\end{equation}

Compose \\(\Gamma[q']\\) and distribute:

\begin{equation}
  \begin{aligned}
  \partial\_1L' \circ \Gamma[q'] & = (\partial\_1L \circ \Gamma[q])(I\_1 \circ \partial\_1 C \circ \Gamma[q']) + (\partial\_2L \circ \Gamma[q])(I\_2 \circ \partial\_1 C \circ \Gamma[q']) \cr
& = (\partial\_1L \circ \Gamma[q])(\partial\_1F(t, q'(t))) + (\partial\_2L \circ \Gamma[q])D(\partial\_1F(t, q'(t)))
  \end{aligned}
\end{equation}

We now have both components of the primed Lagrange equations from \eqref{eq:lagrange-prime}.

Subtract the two terms, extract common factors and use our assumption \eqref{eq:1-15-lagrange} that the original Lagrange equations hold:

\begin{equation}
  \begin{aligned}
  D(\partial\_2L' \circ \Gamma[q']) - (\partial\_1L' \circ \Gamma[q']) & = \left[ D(\partial\_2L \circ \Gamma[q]) - (\partial\_1L \circ \Gamma[q]) \right] \partial\_1F(t, q'(t)) \cr
  & + \left[ (\partial\_2L \circ \Gamma[q])  - (\partial\_2L \circ \Gamma[q]) \right] D(\partial\_1F(t, q'(t))) \cr
& = \left[ D(\partial\_2L \circ \Gamma[q]) - (\partial\_1L \circ \Gamma[q]) \right] \partial\_1F(t, q'(t)) \cr
& = 0
  \end{aligned}
\end{equation}

And boom, we're done! The primed Lagranged equations \eqref{eq:lagrange-prime} hold if the unprimed equations \eqref{eq:1-15-lagrange} hold.

I'm not sure what to make of the new constant terms. The new Lagrange equations are scaled by \\(\partial\_1 F(t, q'(t))\\), the derivative of \\(F\\) with respect to the path; that seems interesting, and possibly there's some nice physical intuition waiting to be discovered.

### Scheme Derivation<a id="sec-1-16-5"></a>

Can we use Scheme to pursue the same derivation? If we can write the relationships of the derivation in code, then we'll have a sort of computerized proof that the primed Lagrange equations are valid.

First, consider \\(\partial\_1 L' \circ \Gamma[q']\\):

```scheme
(->tex-equation
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))
```

\begin{equation}
D{q}^\prime\left( t \right) {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) {{\partial}\_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) + {\partial}\_{2}L\left( \begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ F\left( t, {q}^\prime\left( t \right) \right)} \cr \cr \displaystyle{ D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right)}\end{pmatrix} \right) \left( {\partial}\_{0} {\partial}\_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

This is completely insane, and already unhelpful. The argument to \\(L\\), we know, is actually \\(\Gamma[q]\\). Make a function that will replace the tuple with that reference:

```scheme
(define (->eq expr)
  (write-string
   (replace-all (->tex-equation* expr)
                (->tex* ((Gamma q) 't))
                "\\circ \\Gamma[q]")))
```

Try again:

```scheme
(->eq
 ((compose ((partial 1) Lprime) (Gamma qprime))
  't))
```

\begin{equation}
D{q}^\prime\left( t \right) {\partial}\_{2}L\left( \circ \Gamma[q] \right) {{\partial}\_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}L\left( \circ \Gamma[q] \right) + {\partial}\_{2}L\left( \circ \Gamma[q] \right) \left( {\partial}\_{0} {\partial}\_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

Ignore the parentheses around \\(\circ \Gamma[q]\\) and this looks better.

The \\(\partial\_1 L \circ \Gamma[q]\\) term of the unprimed Lagrange equations is nestled inside the expansion above, multiplied by a factor \\(\partial\_1F(t, q'(t))\\):

```scheme
(let* ((factor (((partial 1) F) 't (qprime 't))))
  (->eq
   ((* factor (compose ((partial 1) L) (Gamma q)))
    't)))
```

\begin{equation}
{\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}L\left( \circ \Gamma[q] \right)
\end{equation}

Next, consider the \\(D(\partial\_2 L' \circ \Gamma[q'])\\) term:

```scheme
(->eq
 ((D (compose ((partial 2) Lprime) (Gamma qprime)))
  't))
```

\begin{equation}
{\left( D{q}^\prime\left( t \right) \right)}^{2} {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {{\partial}\_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + D{q}^\prime\left( t \right) {\left( {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} \left( {\partial}\_{1} {\partial}\_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + 2 D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}\_{0} {\partial}\_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\left( {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \right)}^{2} {D}^{2}{q}^\prime\left( t \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}\_{1} {\partial}\_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + D{q}^\prime\left( t \right) {\partial}\_{2}L\left( \circ \Gamma[q] \right) {{\partial}\_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) {{\partial}\_{0}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}\_{0} {\partial}\_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}\_{2}L\left( \circ \Gamma[q] \right) \left( {\partial}\_{0} {\partial}\_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right)
\end{equation}

This, again, is total madness. We really want some way to control how Scheme expands terms.

But we know what we're looking for. Expand out the matching term of the unprimed Lagrange equations:

```scheme
(->eq
 ((D (compose ((partial 2) L) (Gamma q)))
  't))
```

\begin{equation}
{\left( D{q}^\prime\left( t \right) \right)}^{2} {{\partial}\_{1}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + D{q}^\prime\left( t \right) {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}\_{1} {\partial}\_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + 2 D{q}^\prime\left( t \right) \left( {\partial}\_{0} {\partial}\_{1} \right)\left( F \right)\left( t, {q}^\prime\left( t \right) \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}\_{1}F\left( t, {q}^\prime\left( t \right) \right) {D}^{2}{q}^\prime\left( t \right) {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) + {\partial}\_{0}F\left( t, {q}^\prime\left( t \right) \right) \left( {\partial}\_{1} {\partial}\_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right) + {{\partial}\_{2}}^{2}\left( L \right)\left( \circ \Gamma[q] \right) {{\partial}\_{0}}^{2}\left( F \right)\left( t, {q}^\prime\left( t \right) \right) + \left( {\partial}\_{0} {\partial}\_{2} \right)\left( L \right)\left( \circ \Gamma[q] \right)
\end{equation}

Staring at these two equations, it becomes clear that the first contains the second, multiplied by \\(\partial\_1F(t, q'(t))\\), the same factor that appeared in the expansion of the \\(\partial\_1 L \circ \Gamma[q]\\) term.

Try writing out the primed Lagrange equations, and subtracting the unprimed Lagrange equations, scaled by this factor:

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

Done! It seems that the extra terms on each side exactly cancel. As with the pen and paper derivation, we've shown that the primed Lagranged equations \eqref{eq:lagrange-prime} hold if the unprimed equations \label{eq:1-15-lagrange} hold.

### Final Comments<a id="sec-1-16-6"></a>

I'm troubled by my lack of intuition around two ideas:

-   what is the meaning of the \\(\partial\_1F(t, q'(t))\\) scaling factor?
-   Both sides acquire a constant \\((\partial\_2L \circ \Gamma[q]) \cdot D(\partial\_1F(t, q'(t)))\\).

The right factor is a total time derivative. Is this meaningful? We know from later discussion that if we add a total time derivative to the Lagrangian we don't affect the shape of the realizable path.

I learned quite a bit about functional notation from this exercise, and I think that test that the result represents is critical for leaning hard on the coordinate transformations that we'll continue to explore. But I do feel like I'm leaving intuitive cake on the table.

## Exercise 1.16: Central force motion<a id="sec-1-17"></a>

```scheme
(load "ch1/utils.scm")
```

This exercise gives you practice in writing Lagrangians as compositions.

> Find Lagrangians for central force motion in three dimensions in rectangular coordinates and in spherical coordinates. First, find the Lagrangians analytically, then check the results with the computer by generalizing the programs that we have presented.

### Analytic Approach<a id="sec-1-17-1"></a>

Analytically in 3d coordinates, we have a straightforward extension:

\begin{equation}
L = T - V = {1 \over 2} m (v\_x^2 + v\_y^2 + v\_z^2) - U(\sqrt{x^2 + y^2 + z^2})
\end{equation}

How would you find the spherical Lagrangian analytically?

The potential part is easy. We know that \\(r = \sqrt{x^2 + y^2 + z^2}\\), so the final Lagrangian will have a \\(U(r)\\) term.

To transform the kinetic energies:

-   write down the coordinate transformation from spherical to rectangular:
-   get the velocity components by taking derivatives, chain rule
-   substitute these in to the rectangular Lagrangian

This is equivalent to generating \\(L' = L \circ C\\), where \\(C\\) is a function that substitutes each position \\(x, y, z\\) or velocity \\(v\_x, v\_y, v\_z\\) for its spherical representation. \\(L'\\) is the spherical Lagrangian, \\(L\\) is the rectangular.

How do you get the velocities?

Remember, from equation 1.77 in the book:

\begin{equation}
C(t, x', v') = (t,\, F(t, x'),\, \partial\_0F(t, x') + \partial\_1F(t, x')v')
\end{equation}

\\(F(t, x')\\) is the conversion from spherical to rectangular coordinates. \\(x'\\) is a 3-tuple of \\((r, \theta, \phi)\\), and \\(F(t, x')\\) is a 3-tuple \\((x, y, z)\\), where:

\begin{equation}
\begin{aligned}
  x & = r \sin \theta \cos \phi \cr
  y & = r \sin \theta \sin \phi \cr
  z & = r \cos \theta
\end{aligned}
\end{equation}

It's important to remember that the partials in equation 1.77 are taken as if the arguments were static. The chain rule has already been applied. It's more san

\begin{equation}
\partial\_0F(t, x') = (0, 0, 0)
\end{equation}

This has three components; one for each of the components in the return value.

\\(\partial\_1F(t, x')\\) is a tuple with three components, each of which *also* has three components, one for each of the spherical coordinates.

The inner tuples collapse upon multiplication by \\(v' = (\dot{r}, \dot{\theta}, \dot{\phi})\\), leaving:

\begin{equation}
  \begin{aligned}
    v\_x & = \dot{r} \sin \theta \cos \phi + r \dot{\theta} \cos \theta \cos \phi + r \dot{\phi} \sin \theta \sin \phi \cr
    v\_y & = \dot{r} \sin \theta \sin \phi + r \dot{\theta} \cos \theta \sin \phi + r \dot{\phi} \sin \theta \cos \phi \cr
    v\_z & = \dot{r} \cos \theta + r \dot{\theta} \sin \theta
  \end{aligned}
\end{equation}

Substituting these in results, eventually, in a waterfall of cancellations and leaves:

\begin{equation}
L' = {1 \over 2} m (r^2 \dot{\phi}^2 \sin^2\phi + r^2 \dot{\theta}^2 + \dot{r}^2) - U(r)
\end{equation}

### Scheme Approach<a id="sec-1-17-2"></a>

To show the rectangular Lagrangian, get the procedure from page 41:

```scheme
(define ((L-central-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U (sqrt (square q))))))
```

This is already written in a form that can handle an arbitrary number of coordiantes. Confirm the rectangular Lagrangian by passing in a local tuple with 3 dimensional coordinates and velocities:

```scheme
(->tex-equation
 ((L-central-rectangular 'm (literal-function 'U))
  (up 't
      (up 'x 'y 'z)
      (up 'v_x 'v_y 'v_z))))
```

\begin{equation}
{{1}\over {2}} m {{v}\_{x}}^{2} + {{1}\over {2}} m {{v}\_{y}}^{2} + {{1}\over {2}} m {{v}\_{z}}^{2} - U\left( \sqrt{{x}^{2} + {y}^{2} + {z}^{2}} \right)
\end{equation}

Next, the spherical. Write down the coordinate transformation from spherical to rectangular coordinates as a Scheme procedure:

```scheme
(define (spherical->rect local)
  (let* ((q (coordinate local))
         (r (ref q 0))
         (theta (ref q 1))
         (phi (ref q 2)))
    (up (* r (sin theta) (cos phi))
        (* r (sin theta) (sin phi))
        (* r (cos theta)))))
```

Here are the velocities calculated above by hand:

```scheme
(->tex-equation
 (velocity
  ((F->C spherical->rect)
   (up 't
       (up 'r 'theta 'phi)
       (up 'rdot 'thetadot 'phidot)))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{  - \dot{\phi} r \sin\left( \phi \right) \sin\left( \theta \right) + r \dot{\theta} \cos\left( \phi \right) \cos\left( \theta \right) + \dot{r} \cos\left( \phi \right) \sin\left( \theta \right)} \cr \cr \displaystyle{ \dot{\phi} r \cos\left( \phi \right) \sin\left( \theta \right) + r \dot{\theta} \sin\left( \phi \right) \cos\left( \theta \right) + \dot{r} \sin\left( \phi \right) \sin\left( \theta \right)} \cr \cr \displaystyle{  - r \dot{\theta} \sin\left( \theta \right) + \dot{r} \cos\left( \theta \right)}\end{pmatrix}
\end{equation}

Now that we have \\(L\\) and \\(C\\), we can compose them to get \\(L'\\), our spherical Lagrangian:

```scheme
(define (L-central-spherical m U)
  (compose (L-central-rectangular m U)
           (F->C spherical->rect)))
```

Confirm that this is equivalent to the analytic solution:

```scheme
(->tex-equation
 ((L-central-spherical 'm (literal-function 'U))
  (up 't
      (up 'r 'theta 'phi)
      (up 'rdot 'thetadot 'phidot))))
```

\begin{equation}
{{1}\over {2}} m {\dot{\phi}}^{2} {r}^{2} {\left( \sin\left( \theta \right) \right)}^{2} + {{1}\over {2}} m {r}^{2} {\dot{\theta}}^{2} + {{1}\over {2}} m {\dot{r}}^{2} - U\left( r \right)
\end{equation}

### Discussion<a id="sec-1-17-3"></a>

Langrangian coordinate transformation from spherical -> rectangular on paper, which of course is a total nightmare, writing vx<sup>2</sup> + vy<sup>2</sup> + vz<sup>2</sup> and simplifying. BUT then, of course, you write down the spherical => rectangular position change&#x2026;

the explicit link to function composition, and how the new lagrangian is (Lagrangian A + A<-B + B<-C)&#x2026; really drives home how coordinate transforms can stack associatively through function composition. the lesson is, prove that the code works, then trust the program to go to crazy coordinate systems.

Later, the authors add in a very simple-to-write coordinate transform that has one of the angles depend on t. and then compose that in, and boom, basically for free you're in rotating spherical coords.

## Exercise 1.17: Bead on a helical wire<a id="sec-1-18"></a>

```scheme
(load "ch1/utils.scm")
```

This, and the next three exercises, are here to give you practice in the real art, of difficulty, of any dynamics problem. It's easy to change coordinates. So what coordinates do you use?

> A bead of mass \\(m\\) is constrained to move on a frictionless helical wire. The helix is oriented so that its axis is horizontal. The diameter of the helix is \\(d\\) and its pitch (turns per unit length) is \\(h\\). The system is in a uniform gravitational field with vertical acceleration \\(g\\). Formulate a Lagrangian that describes the system and find the Lagrange equations of motion.

I'll replace this with a better picture later, but this is the setup:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-25_11-03-55_screenshot.png)

```scheme
(define ((turns->rect d h) local)
  (let* ((turns (coordinate local))
         (theta (* turns 2 'pi)))
    (up (/ turns h)
        (* (/ d 2) (cos theta))
        (* (/ d 2) (sin theta)))))
```

Or you could do this. Remember, these transformations need to be functions of a local tuple, so if you're going to compose them, remember to put `coordinate` at the beginning of the composition.

```scheme
(define ((turns->x-theta h) q)
  (up (/ q h)
      (* q 2 'pi)))

(define ((x-theta->rect d) q)
  (let* ((x (ref q 0))
         (theta (ref q 1)))
    (up x
        (* (/ d 2) (cos theta))
        (* (/ d 2) (sin theta)))))

(define (turns->rect* d h)
  (compose (x-theta->rect d)
           (turns->x-theta h)
           coordinate))
```

The transformations are identical:

```scheme
(->tex-equation
 ((- (turns->rect 'd 'h)
     (turns->rect* 'd 'h))
  (up 't 'n 'ndot)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0}\end{pmatrix}
\end{equation}

Define the Lagrangian:

```scheme
(define ((L-rectangular m U) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (U q))))

(define (L-turns m d h U)
  (compose (L-rectangular m U)
           (F->C (turns->rect d h))))
```

The potential is a uniform gravitational acceleration:

```scheme
(define ((U-grav m g) q)
  (* m g (ref q 2)))
```

Final Lagrangian:

```scheme
(->tex-equation
 ((L-turns 'm 'd 'h (U-grav 'm 'g))
  (up 't 'n 'ndot)))
```

\begin{equation}
{{{{1}\over {2}} {d}^{2} {h}^{2} m {\dot{n}}^{2} {\pi}^{2} - {{1}\over {2}} d g {h}^{2} m \sin\left( 2 n \pi \right) + {{1}\over {2}} m {\dot{n}}^{2}}\over {{h}^{2}}}
\end{equation}

Lagrange equations of motion:

```scheme
(let* ((L (L-turns 'm 'd 'h (U-grav 'm 'g)))
       (n (literal-function 'n)))
  (->tex-equation
   (((Lagrange-equations L) n) 't)))
```

\begin{equation}
{{{d}^{2} {h}^{2} m {\pi}^{2} {D}^{2}n\left( t \right) + d g {h}^{2} m \pi \cos\left( 2 \pi n\left( t \right) \right) + m {D}^{2}n\left( t \right)}\over {{h}^{2}}}
\end{equation}

## Exercise 1.18: Bead on a triaxial surface<a id="sec-1-19"></a>

```scheme
(load "ch1/utils.scm")
```

> A bead of mass \\(m\\) moves without friction on a triaxial ellipsoidal surface. In rectangular coordinates the surface satisfies
>
> \begin{equation}
>   {x^2 \over a^2} + {y^2 \over b^2} + {z^2 \over c^2} = 1
> \end{equation}
>
> for some constants \\(a\\), \\(b\\), and \\(c\\). Identify suitable generalized coordinates, formulate a Lagrangian, and find Lagrange's equations.

The transformation to elliptical coordinates is very similar to the spherical coordinate transformation, but with a fixed \\(a\\), \\(b\\) and \\(c\\) coefficient for each rectangular dimension, and no more radial degree of freedom:

```scheme
(define ((elliptical->rect a b c) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (phi (ref q 1)))
    (up (* a (sin theta) (cos phi))
        (* b (sin theta) (sin phi))
        (* c (cos theta)))))
```

Next, the Lagrangian:

```scheme
(define ((L-free-particle m) local)
  (* 1/2 m (square
            (velocity local))))

(define (L-central-triaxial m a b c)
  (compose (L-free-particle m)
           (F->C (elliptical->rect a b c))))
```

Final Lagrangian:

```scheme
(let ((local (up 't
                 (up 'theta 'phi)
                 (up 'thetadot 'phidot))))
  (->tex-equation
   ((L-central-triaxial 'm 'a 'b 'c) local)))
```

\begin{equation}
{{1}\over {2}} {a}^{2} m {\dot{\phi}}^{2} {\left( \sin\left( \phi \right) \right)}^{2} {\left( \sin\left( \theta \right) \right)}^{2} - {a}^{2} m \dot{\phi} \dot{\theta} \sin\left( \phi \right) \sin\left( \theta \right) \cos\left( \phi \right) \cos\left( \theta \right) + {{1}\over {2}} {a}^{2} m {\dot{\theta}}^{2} {\left( \cos\left( \phi \right) \right)}^{2} {\left( \cos\left( \theta \right) \right)}^{2} + {{1}\over {2}} {b}^{2} m {\dot{\phi}}^{2} {\left( \sin\left( \theta \right) \right)}^{2} {\left( \cos\left( \phi \right) \right)}^{2} + {b}^{2} m \dot{\phi} \dot{\theta} \sin\left( \phi \right) \sin\left( \theta \right) \cos\left( \phi \right) \cos\left( \theta \right) + {{1}\over {2}} {b}^{2} m {\dot{\theta}}^{2} {\left( \sin\left( \phi \right) \right)}^{2} {\left( \cos\left( \theta \right) \right)}^{2} + {{1}\over {2}} {c}^{2} m {\dot{\theta}}^{2} {\left( \sin\left( \theta \right) \right)}^{2}
\end{equation}

I'm sure there's some simplification in there for us. But why?

Lagrange equations of motion:

```scheme
(let* ((L (L-central-triaxial 'm 'a 'b 'c))
       (theta (literal-function 'theta))
       (phi (literal-function 'phi)))
  (->tex-equation
   (((Lagrange-equations L) (up theta phi))
    't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{  - 2 {a}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) D\theta\left( t \right) D\phi\left( t \right) {\left( \cos\left( \theta\left( t \right) \right) \right)}^{2} - {a}^{2} m {\left( \cos\left( \phi\left( t \right) \right) \right)}^{2} {\left( D\theta\left( t \right) \right)}^{2} \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) - {a}^{2} m {\left( \cos\left( \phi\left( t \right) \right) \right)}^{2} {\left( D\phi\left( t \right) \right)}^{2} \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) - {b}^{2} m {\left( \sin\left( \phi\left( t \right) \right) \right)}^{2} {\left( D\theta\left( t \right) \right)}^{2} \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) - {b}^{2} m {\left( \sin\left( \phi\left( t \right) \right) \right)}^{2} {\left( D\phi\left( t \right) \right)}^{2} \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) + 2 {b}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) D\theta\left( t \right) D\phi\left( t \right) {\left( \cos\left( \theta\left( t \right) \right) \right)}^{2} - {a}^{2} m {D}^{2}\phi\left( t \right) \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) + {a}^{2} m {\left( \cos\left( \phi\left( t \right) \right) \right)}^{2} {\left( \cos\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\theta\left( t \right) + {b}^{2} m {D}^{2}\phi\left( t \right) \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) + {b}^{2} m {\left( \sin\left( \phi\left( t \right) \right) \right)}^{2} {\left( \cos\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\theta\left( t \right) + {c}^{2} m {\left( D\theta\left( t \right) \right)}^{2} \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) + {c}^{2} m {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} {D}^{2}\theta\left( t \right)} \cr \cr \displaystyle{ 2 {a}^{2} m {\left( \sin\left( \phi\left( t \right) \right) \right)}^{2} D\theta\left( t \right) D\phi\left( t \right) \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) + {a}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} + {a}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} - {b}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} - {b}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} + 2 {b}^{2} m {\left( \cos\left( \phi\left( t \right) \right) \right)}^{2} D\theta\left( t \right) D\phi\left( t \right) \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) + {a}^{2} m {D}^{2}\phi\left( t \right) {\left( \sin\left( \phi\left( t \right) \right) \right)}^{2} {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} - {a}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {D}^{2}\theta\left( t \right) + {b}^{2} m {D}^{2}\phi\left( t \right) {\left( \cos\left( \phi\left( t \right) \right) \right)}^{2} {\left( \sin\left( \theta\left( t \right) \right) \right)}^{2} + {b}^{2} m \sin\left( \phi\left( t \right) \right) \cos\left( \phi\left( t \right) \right) \sin\left( \theta\left( t \right) \right) \cos\left( \theta\left( t \right) \right) {D}^{2}\theta\left( t \right)}\end{bmatrix}
\end{equation}

This is fairly horrifying. This really demands animation, as I bet it looks cool, but it's not comprehensible in this form.

## Exercise 1.19: Two-bar linkage<a id="sec-1-20"></a>

Double pendulum, sort of, except the whole thing can fly around the plane.

The system description is:

```scheme
(load "ch1/utils.scm")
```

> The two-bar linkage shown in figure 1.3 is constrained to move in the plane. It is composed of three small massive bodies interconnected by two massless rigid rods in a uniform gravitational field with vertical acceleration g. The rods are pinned to the central body by a hinge that allows the linkage to fold. The system is arranged so that the hinge is completely free: the members can go through all configurations without collision. Formulate a Lagrangian that describes the system and find the Lagrange equations of motion. Use the computer to do this, because the equations are rather big.

This is new. Now we have multiple bodies:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-29_05-39-01_Art_P146.jpg)

We can handle this by treating our coordinate space as having new dimensions for, say, \\(x\_0\\), \\(y\_0\\), \\(x\_1\\), \\(y\_1\\). The fact that multiple coordinates refer to the same particle doesn't matter for the Lagrangian. But it's a confusing API.

*Without* any constraints, we have six degrees of freedom. \\(x, y\\) for each particle. With the constraints we have:

1.  \\(x, y\\) for the central body
2.  \\(\theta\\) and \\(\phi\\) for the angles off center.

(Sketch these out on the picture for the final version.)

\begin{equation}
\begin{aligned}
  x\_2(t) & = x\_2(t) \cr
  y\_2(t) & = y\_2(t) \cr
  x\_1(t) & = x\_2(t) + l\_1 \sin \theta \cr
  y\_1(t) & = y\_2(t) - l\_1 \cos \theta \cr
  x\_3(t) & = x\_2(t) + l\_2 \sin \phi \cr
  y\_3(t) & = y\_2(t) - l\_2 \cos \phi
\end{aligned}
\end{equation}

Sketch out why this makes sense. Each angle is positive CCW for consistency, since they can swing all the way around.

Write the coordinate transformation in scheme.

```scheme
(define ((double-linkage->rect l1 l2) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (phi (ref q 1))
         (x2 (ref q 2))
         (y2 (ref q 3)))
    (up (+ x2 (* l1 (sin theta)))
        (- y2 (* l1 (cos theta)))
        x2
        y2
        (+ x2 (* l2 (sin phi)))
        (- y2 (* l2 (cos phi))))))
```

Next, the Lagrangian given rectangular coordinates, assuming no constraints. Remember, we have a uniform gravitational field pointing down; this means that each of the components has a potential dragging on it.

```scheme
(define ((L-double-linkage-rect m1 m2 m3 U) local)
  (let* ((v (velocity local))
         (vx1 (ref v 0))
         (vy1 (ref v 1))
         (vx2 (ref v 2))
         (vy2 (ref v 3))
         (vx3 (ref v 4))
         (vy3 (ref v 5)))
    (- (+ (* m1 (+ (square vx1)
                   (square vy1)))
          (* m2 (+ (square vx2)
                   (square vy2)))
          (* m3 (+ (square vx3)
                   (square vy3))))
       (U (coordinate local)))))
```

And the composition:

```scheme
(define (L-double-linkage l1 l2 m1 m2 m3 U)
  (compose (L-double-linkage-rect m1 m2 m3 U)
           (F->C (double-linkage->rect l1 l2))))
```

Gravitational potential:

```scheme
(define ((U-gravity g m1 m2 m3) q)
  (let* ((y1 (ref q 1))
         (y2 (ref q 3))
         (y3 (ref q 5)))
    (* g (+ (* m1 y1)
            (* m2 y2)
            (* m3 y3)))))
```

```scheme
(let ((local (up 't
                 (up 'theta 'phi 'x_2 'y_2)
                 (up 'thetadot 'phidot 'xdot_2 'ydot_2)))
      (U (U-gravity 'g 'm_1 'm_2 'm_3)))
  (->tex-equation
   ((L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U) local)))
```

\begin{equation}
{{l}\_{1}}^{2} {m}\_{1} {\dot{\theta}}^{2} + 2 {l}\_{1} {m}\_{1} \dot{\theta} {\dot{x}}\_{2} \cos\left( \theta \right) + 2 {l}\_{1} {m}\_{1} \dot{\theta} {\dot{y}}\_{2} \sin\left( \theta \right) + {{l}\_{2}}^{2} {m}\_{3} {\dot{\phi}}^{2} + 2 {l}\_{2} {m}\_{3} \dot{\phi} {\dot{x}}\_{2} \cos\left( \phi \right) + 2 {l}\_{2} {m}\_{3} \dot{\phi} {\dot{y}}\_{2} \sin\left( \phi \right) + g {l}\_{1} {m}\_{1} \cos\left( \theta \right) + g {l}\_{2} {m}\_{3} \cos\left( \phi \right) - g {m}\_{1} {y}\_{2} - g {m}\_{2} {y}\_{2} - g {m}\_{3} {y}\_{2} + {m}\_{1} {{\dot{x}}\_{2}}^{2} + {m}\_{1} {{\dot{y}}\_{2}}^{2} + {m}\_{2} {{\dot{x}}\_{2}}^{2} + {m}\_{2} {{\dot{y}}\_{2}}^{2} + {m}\_{3} {{\dot{x}}\_{2}}^{2} + {m}\_{3} {{\dot{y}}\_{2}}^{2}
\end{equation}

Lagrange equations of motion:

```scheme
(let* ((U (U-gravity 'g 'm_1 'm_2 'm_3))
       (L (L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U))
       (theta (literal-function 'theta))
       (phi (literal-function 'phi))
       (x2 (literal-function 'x_2))
       (y2 (literal-function 'y_2)))
  (->tex-equation
   (((Lagrange-equations L) (up theta phi x2 y2))
    't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ g {l}\_{1} {m}\_{1} \sin\left( \theta\left( t \right) \right) + 2 {{l}\_{1}}^{2} {m}\_{1} {D}^{2}\theta\left( t \right) + 2 {l}\_{1} {m}\_{1} \sin\left( \theta\left( t \right) \right) {D}^{2}{y}\_{2}\left( t \right) + 2 {l}\_{1} {m}\_{1} \cos\left( \theta\left( t \right) \right) {D}^{2}{x}\_{2}\left( t \right)} \cr \cr \displaystyle{ g {l}\_{2} {m}\_{3} \sin\left( \phi\left( t \right) \right) + 2 {{l}\_{2}}^{2} {m}\_{3} {D}^{2}\phi\left( t \right) + 2 {l}\_{2} {m}\_{3} \sin\left( \phi\left( t \right) \right) {D}^{2}{y}\_{2}\left( t \right) + 2 {l}\_{2} {m}\_{3} \cos\left( \phi\left( t \right) \right) {D}^{2}{x}\_{2}\left( t \right)} \cr \cr \displaystyle{  - 2 {l}\_{1} {m}\_{1} \sin\left( \theta\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} - 2 {l}\_{2} {m}\_{3} \sin\left( \phi\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + 2 {l}\_{1} {m}\_{1} {D}^{2}\theta\left( t \right) \cos\left( \theta\left( t \right) \right) + 2 {l}\_{2} {m}\_{3} {D}^{2}\phi\left( t \right) \cos\left( \phi\left( t \right) \right) + 2 {m}\_{1} {D}^{2}{x}\_{2}\left( t \right) + 2 {m}\_{2} {D}^{2}{x}\_{2}\left( t \right) + 2 {m}\_{3} {D}^{2}{x}\_{2}\left( t \right)} \cr \cr \displaystyle{ 2 {l}\_{1} {m}\_{1} {\left( D\theta\left( t \right) \right)}^{2} \cos\left( \theta\left( t \right) \right) + 2 {l}\_{2} {m}\_{3} {\left( D\phi\left( t \right) \right)}^{2} \cos\left( \phi\left( t \right) \right) + 2 {l}\_{1} {m}\_{1} {D}^{2}\theta\left( t \right) \sin\left( \theta\left( t \right) \right) + 2 {l}\_{2} {m}\_{3} {D}^{2}\phi\left( t \right) \sin\left( \phi\left( t \right) \right) + g {m}\_{1} + g {m}\_{2} + g {m}\_{3} + 2 {m}\_{1} {D}^{2}{y}\_{2}\left( t \right) + 2 {m}\_{2} {D}^{2}{y}\_{2}\left( t \right) + 2 {m}\_{3} {D}^{2}{y}\_{2}\left( t \right)}\end{bmatrix}
\end{equation}

Kill some clear factors:

```scheme
(let* ((U (U-gravity 'g 'm_1 'm_2 'm_3))
       (L (L-double-linkage 'l_1 'l_2 'm_1 'm_2 'm_3 U))
       (theta (literal-function 'theta))
       (phi (literal-function 'phi))
       (x2 (literal-function 'x_2))
       (y2 (literal-function 'y_2))
       (eqs (((Lagrange-equations L) (up theta phi x2 y2))
             't)))
  (->tex-equation
   (up (/ (ref eqs 0) 'l_1 'm_1)
       (/ (ref eqs 1) 'l_2 'm_3)
       (/ (ref eqs 2) 2)
       (ref eqs 3))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ g \sin\left( \theta\left( t \right) \right) + 2 {l}\_{1} {D}^{2}\theta\left( t \right) + 2 \sin\left( \theta\left( t \right) \right) {D}^{2}{y}\_{2}\left( t \right) + 2 \cos\left( \theta\left( t \right) \right) {D}^{2}{x}\_{2}\left( t \right)} \cr \cr \displaystyle{ g \sin\left( \phi\left( t \right) \right) + 2 {l}\_{2} {D}^{2}\phi\left( t \right) + 2 \sin\left( \phi\left( t \right) \right) {D}^{2}{y}\_{2}\left( t \right) + 2 \cos\left( \phi\left( t \right) \right) {D}^{2}{x}\_{2}\left( t \right)} \cr \cr \displaystyle{  - {l}\_{1} {m}\_{1} \sin\left( \theta\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} - {l}\_{2} {m}\_{3} \sin\left( \phi\left( t \right) \right) {\left( D\phi\left( t \right) \right)}^{2} + {l}\_{1} {m}\_{1} {D}^{2}\theta\left( t \right) \cos\left( \theta\left( t \right) \right) + {l}\_{2} {m}\_{3} {D}^{2}\phi\left( t \right) \cos\left( \phi\left( t \right) \right) + {m}\_{1} {D}^{2}{x}\_{2}\left( t \right) + {m}\_{2} {D}^{2}{x}\_{2}\left( t \right) + {m}\_{3} {D}^{2}{x}\_{2}\left( t \right)} \cr \cr \displaystyle{ 2 {l}\_{1} {m}\_{1} {\left( D\theta\left( t \right) \right)}^{2} \cos\left( \theta\left( t \right) \right) + 2 {l}\_{2} {m}\_{3} {\left( D\phi\left( t \right) \right)}^{2} \cos\left( \phi\left( t \right) \right) + 2 {l}\_{1} {m}\_{1} {D}^{2}\theta\left( t \right) \sin\left( \theta\left( t \right) \right) + 2 {l}\_{2} {m}\_{3} {D}^{2}\phi\left( t \right) \sin\left( \phi\left( t \right) \right) + g {m}\_{1} + g {m}\_{2} + g {m}\_{3} + 2 {m}\_{1} {D}^{2}{y}\_{2}\left( t \right) + 2 {m}\_{2} {D}^{2}{y}\_{2}\left( t \right) + 2 {m}\_{3} {D}^{2}{y}\_{2}\left( t \right)}\end{pmatrix}
\end{equation}

This was not as gnarly as the previous problem. Perhaps I did something wrong there. We'll see when we get animation.

## Exercise 1.20: Sliding pendulum<a id="sec-1-21"></a>

```scheme
(load "ch1/utils.scm")
```

> Consider a pendulum of length \\(l\\) attached to a support that is free to move horizontally, as shown in figure 1.4. Let the mass of the support be \\(m1\\) and the mass of the pendulum bob be \\(m2\\). Formulate a Lagrangian and derive Lagrange's equations for this system.

This is interesting, and totally not-obvious how to represent with Newtonian mechanics. Here it is pretty simple. The setup:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-29_05-39-33_Art_P147.jpg)

We can use 2 coordinates:

1.  the horizontal position of the cart
2.  the angle \\(\theta\\) of the bob.

Here's the conversion to rectangular:

\begin{equation}
\begin{aligned}
  x\_1(t) & = x\_1(t) \cr
  y\_1(t) & = l \cr
  x\_2(t) & = x\_1(t) + l \sin \theta \cr
  y\_2(t) & = l(1 - \cos \theta)
\end{aligned}
\end{equation}

Draw these on the picture to make it clearer.

Write the coordinate transformation in scheme.

```scheme
(define ((sliding-pend->rect l) local)
  (let* ((q (coordinate local))
         (x1 (ref q 0))
         (theta (ref q 1)))
    (up x1
        l
        (+ x1 (* l (sin theta)))
        (* l (- 1 (cos theta))))))
```

Next, the Lagrangian given rectangular coordinates, assuming no constraints:

```scheme
(define ((L-sliding-pend-rect m1 m2 U) local)
  (let* ((v (velocity local))
         (vx1 (ref v 0))
         (vy1 (ref v 1))
         (vx2 (ref v 2))
         (vy2 (ref v 3)))
    (- (+ (* m1 (+ (square vx1)
                   (square vy1)))
          (* m2 (+ (square vx2)
                   (square vy2))))
       (U (coordinate local)))))
```

And the composition:

```scheme
(define (L-sliding-pend l m1 m2 U)
  (compose (L-sliding-pend-rect m1 m2 U)
           (F->C (sliding-pend->rect l))))
```

Gravitational potential. I could include the cart here, but since we know it's fixed gravitationally it wouldn't change the equations of motion.

```scheme
(define ((U-gravity g m2) q)
  (let* ((y2 (ref q 3)))
    (* m2 g y2)))
```

```scheme
(let ((local (up 't
                 (up 'x_1 'theta)
                 (up 'xdot_1 'thetadot)))
      (U (U-gravity 'g 'm_2)))
  (->tex-equation
   ((L-sliding-pend 'l 'm_1 'm_2 U) local)))
```

\begin{equation}
{l}^{2} {m}\_{2} {\dot{\theta}}^{2} + 2 l {m}\_{2} \dot{\theta} {\dot{x}}\_{1} \cos\left( \theta \right) + g l {m}\_{2} \cos\left( \theta \right) - g l {m}\_{2} + {m}\_{1} {{\dot{x}}\_{1}}^{2} + {m}\_{2} {{\dot{x}}\_{1}}^{2}
\end{equation}

Lagrange equations of motion:

```scheme
(let* ((U (U-gravity 'g 'm_2))
       (L (L-sliding-pend 'l 'm_1 'm_2 U))
       (x1 (literal-function 'x_1))
       (theta (literal-function 'theta)))
  (->tex-equation
   (((Lagrange-equations L) (up x1 theta))
    't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{  - 2 l {m}\_{2} \sin\left( \theta\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} + 2 l {m}\_{2} {D}^{2}\theta\left( t \right) \cos\left( \theta\left( t \right) \right) + 2 {m}\_{1} {D}^{2}{x}\_{1}\left( t \right) + 2 {m}\_{2} {D}^{2}{x}\_{1}\left( t \right)} \cr \cr \displaystyle{ g l {m}\_{2} \sin\left( \theta\left( t \right) \right) + 2 {l}^{2} {m}\_{2} {D}^{2}\theta\left( t \right) + 2 l {m}\_{2} {D}^{2}{x}\_{1}\left( t \right) \cos\left( \theta\left( t \right) \right)}\end{bmatrix}
\end{equation}

Cleaner:

```scheme
(let* ((U (U-gravity 'g 'm_2))
       (L (L-sliding-pend 'l 'm_1 'm_2 U))
       (x1 (literal-function 'x_1))
       (theta (literal-function 'theta))
       (eqs (((Lagrange-equations L) (up x1 theta))
             't)))
  (->tex-equation
   (up (ref eqs 0)
       (/ (ref eqs 1) 'l 'm_2))))
```

\begin{equation}
\begin{pmatrix} \displaystyle{  - 2 l {m}\_{2} \sin\left( \theta\left( t \right) \right) {\left( D\theta\left( t \right) \right)}^{2} + 2 l {m}\_{2} {D}^{2}\theta\left( t \right) \cos\left( \theta\left( t \right) \right) + 2 {m}\_{1} {D}^{2}{x}\_{1}\left( t \right) + 2 {m}\_{2} {D}^{2}{x}\_{1}\left( t \right)} \cr \cr \displaystyle{ g \sin\left( \theta\left( t \right) \right) + 2 l {D}^{2}\theta\left( t \right) + 2 {D}^{2}{x}\_{1}\left( t \right) \cos\left( \theta\left( t \right) \right)}\end{pmatrix}
\end{equation}

## Exercise 1.21: A dumbbell<a id="sec-1-22"></a>

The uneven dumbbell. We've just made it through four exercises which embrace the idea that you can bake constraints into the coordinate transformation. But why should we believe that this is allowed?

This exercise comes after a section called "Why it Works".

The next exercise tries to do a coordinate change that is really careful about *not* changing the dimension of the configuration space, so that we can show that this move is allowed. Here's the setup:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-29_05-40-00_Art_P166.jpg)

```scheme
(load "ch1/utils.scm")
```

The idea here is to take the distance between the particles \\(l\\) and treat it as a new dimension \\(c\\).

Goal is to assume that Newtonian mechanics' approach to constraints, shown [here](https://en.wikipedia.org/wiki/Newtonian_dynamics#Constraint_forces), I think, is correct, and then show that the resulting equations of motion let us treat the distance coordinate \\(c\\) as a constant.

Takes in any number of up tuples and zips them into a new list of up-tuples by taking each element.

### Multiple Particle API<a id="sec-1-22-1"></a>

Many exercises have been dealing with multiple particles so far. Let's introduce some functions that let us pluck the appropriate coordinates out of the local tuple.

If we have the velocity and mass of a particle, its kinetic energy is easy to define:

```scheme
(define (KE-particle m v)
  (* 1/2 m (square v)))
```

This next function, `extract-particle`, takes a number of components &#x2013; 2 for a particle with 2 components, 3 for a particle in space, etc &#x2013; and returns a function of `local` and `i`, a particle index. This function can be used to extract a sub-local-tuple for that particle from a flattened list.

```scheme
(define ((extract-particle pieces) local i)
  (let* ((indices (apply up (iota pieces (* i pieces))))
         (extract (lambda (tuple)
                    (vector-map (lambda (i)
                                  (ref tuple i))
                                indices))))
    (up (time local)
        (extract (coordinate local))
        (extract (velocity local)))))
```

### Part A: Newton's Equations<a id="sec-1-22-2"></a>

> Write Newton's equations for the balance of forces for the four rectangular coordinates of the two particles, given that the scalar tension in the rod is \\(F\\).

TODO: Write these down from the notebook.

### Part B: Dumbbell Lagrangian<a id="sec-1-22-3"></a>

> Write the formal Lagrangian
>
> \begin{equation}
> L(t; x\_0, y\_0, x\_1, y\_1, F; \dot{x}\_0, \dot{y}\_0, \dot{x}\_1, \dot{y}\_1, \dot{F})
> \end{equation}
>
> such that Lagrange's equations will yield the Newton's equations you derived in part **a**.

Here is how we model constraint forces. Each pair of particles has some constraint potential acting between them:

```scheme
(define (U-constraint q0 q1 F l)
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))
```

    #| U-constraint |#

And here's a Lagrangian for two free particles, subject to a constraint potential \\(F\\) acting between them.

```scheme
(define ((L-free-constrained m0 m1 l) local)
  (let* ((extract (extract-particle 2))
         (p0 (extract local 0))
         (q0 (coordinate p0))
         (qdot0 (velocity p0))

         (p1 (extract local 1))
         (q1 (coordinate p1))
         (qdot1 (velocity p1))

         (F (ref (coordinate local) 4)))
    (- (+ (KE-particle m0 qdot0)
          (KE-particle m1 qdot1))
       (U-constraint q0 q1 F l))))
```

    #| L-free-constrained |#

Finally, the path. This is rectangular coordinates for each piece, plus \\(F\\) between them.

```scheme
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
  (->tex-equation
   (f 't)))
```

\begin{equation}
{{{{1}\over {2}} l {m}\_{0} {\left( D{x}\_{0}\left( t \right) \right)}^{2} + {{1}\over {2}} l {m}\_{0} {\left( D{y}\_{0}\left( t \right) \right)}^{2} + {{1}\over {2}} l {m}\_{1} {\left( D{x}\_{1}\left( t \right) \right)}^{2} + {{1}\over {2}} l {m}\_{1} {\left( D{y}\_{1}\left( t \right) \right)}^{2} + {{1}\over {2}} {l}^{2} F\left( t \right) - {{1}\over {2}} F\left( t \right) {\left( {x}\_{1}\left( t \right) \right)}^{2} + F\left( t \right) {x}\_{1}\left( t \right) {x}\_{0}\left( t \right) - {{1}\over {2}} F\left( t \right) {\left( {x}\_{0}\left( t \right) \right)}^{2} - {{1}\over {2}} F\left( t \right) {\left( {y}\_{1}\left( t \right) \right)}^{2} + F\left( t \right) {y}\_{1}\left( t \right) {y}\_{0}\left( t \right) - {{1}\over {2}} F\left( t \right) {\left( {y}\_{0}\left( t \right) \right)}^{2}}\over {l}}
\end{equation}

Here are the Lagrange equations, which, if you squint, are like Newton's equations from part a.

```scheme
(let* ((L (L-free-constrained 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q-rect)))
  (->tex-equation
   (f 't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ {{l {m}\_{0} {D}^{2}{x}\_{0}\left( t \right) - F\left( t \right) {x}\_{1}\left( t \right) + F\left( t \right) {x}\_{0}\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{l {m}\_{0} {D}^{2}{y}\_{0}\left( t \right) - F\left( t \right) {y}\_{1}\left( t \right) + F\left( t \right) {y}\_{0}\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{l {m}\_{1} {D}^{2}{x}\_{1}\left( t \right) + F\left( t \right) {x}\_{1}\left( t \right) - F\left( t \right) {x}\_{0}\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{l {m}\_{1} {D}^{2}{y}\_{1}\left( t \right) + F\left( t \right) {y}\_{1}\left( t \right) - F\left( t \right) {y}\_{0}\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{ - {{1}\over {2}} {l}^{2} + {{1}\over {2}} {\left( {x}\_{1}\left( t \right) \right)}^{2} - {x}\_{1}\left( t \right) {x}\_{0}\left( t \right) + {{1}\over {2}} {\left( {x}\_{0}\left( t \right) \right)}^{2} + {{1}\over {2}} {\left( {y}\_{1}\left( t \right) \right)}^{2} - {y}\_{1}\left( t \right) {y}\_{0}\left( t \right) + {{1}\over {2}} {\left( {y}\_{0}\left( t \right) \right)}^{2}}\over {l}}}\end{bmatrix}
\end{equation}

### Part C: Coordinate Change<a id="sec-1-22-4"></a>

> Make a change of coordinates to a coordinate system with center of mass coordinates \\(x\_{CM}\\), \\(y\_{CM}\\), angle \\(\theta\\), distance between the particles \\(c\\), and tension force \\(F\\). Write the Lagrangian in these coordinates, and write the Lagrange equations.

This is a coordinate change that is very careful not to reduce the degrees of freedom.

First, the coordinate change:

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
```

Then the coordinate change applied to the local tuple:

```scheme
(let ((local (up 't
                 (up 'x_cm 'y_cm 'theta 'c 'F)
                 (up 'xdot_cm 'ydot_cm 'thetadot 'cdot 'Fdot)))
      (C (F->C (cm-theta->rect 'm_0 'm_1))))
  (->tex-equation
   (C local)))
```

\begin{equation}
\begin{pmatrix} \displaystyle{ t} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ {{ - c {m}\_{1} \cos\left( \theta \right) + {m}\_{0} {x}\_{cm} + {m}\_{1} {x}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{ - c {m}\_{1} \sin\left( \theta \right) + {m}\_{0} {y}\_{cm} + {m}\_{1} {y}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{c {m}\_{0} \cos\left( \theta \right) + {m}\_{0} {x}\_{cm} + {m}\_{1} {x}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{c {m}\_{0} \sin\left( \theta \right) + {m}\_{0} {y}\_{cm} + {m}\_{1} {y}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ F}\end{pmatrix}} \cr \cr \displaystyle{ \begin{pmatrix} \displaystyle{ {{c {m}\_{1} \dot{\theta} \sin\left( \theta \right) - \dot{c} {m}\_{1} \cos\left( \theta \right) + {m}\_{0} {\dot{x}}\_{cm} + {m}\_{1} {\dot{x}}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{ - c {m}\_{1} \dot{\theta} \cos\left( \theta \right) - \dot{c} {m}\_{1} \sin\left( \theta \right) + {m}\_{0} {\dot{y}}\_{cm} + {m}\_{1} {\dot{y}}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{ - c {m}\_{0} \dot{\theta} \sin\left( \theta \right) + \dot{c} {m}\_{0} \cos\left( \theta \right) + {m}\_{0} {\dot{x}}\_{cm} + {m}\_{1} {\dot{x}}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{c {m}\_{0} \dot{\theta} \cos\left( \theta \right) + \dot{c} {m}\_{0} \sin\left( \theta \right) + {m}\_{0} {\dot{y}}\_{cm} + {m}\_{1} {\dot{y}}\_{cm}}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ \dot{F}}\end{pmatrix}}\end{pmatrix}
\end{equation}

Then the Lagrangian in the new coordinates;

```scheme
(define (L-free-constrained* m0 m1 l)
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
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f (compose L (Gamma q))))
  (->tex-equation
   (f 't)))
```

\begin{equation}
{{l {m}\_{0} {m}\_{1} {\left( c\left( t \right) \right)}^{2} {\left( D\theta\left( t \right) \right)}^{2} + l {{m}\_{0}}^{2} {\left( D{x}\_{cm}\left( t \right) \right)}^{2} + l {{m}\_{0}}^{2} {\left( D{y}\_{cm}\left( t \right) \right)}^{2} + 2 l {m}\_{0} {m}\_{1} {\left( D{x}\_{cm}\left( t \right) \right)}^{2} + 2 l {m}\_{0} {m}\_{1} {\left( D{y}\_{cm}\left( t \right) \right)}^{2} + l {m}\_{0} {m}\_{1} {\left( Dc\left( t \right) \right)}^{2} + l {{m}\_{1}}^{2} {\left( D{x}\_{cm}\left( t \right) \right)}^{2} + l {{m}\_{1}}^{2} {\left( D{y}\_{cm}\left( t \right) \right)}^{2} + {l}^{2} {m}\_{0} F\left( t \right) + {l}^{2} {m}\_{1} F\left( t \right) - {m}\_{0} F\left( t \right) {\left( c\left( t \right) \right)}^{2} - {m}\_{1} F\left( t \right) {\left( c\left( t \right) \right)}^{2}}\over {2 l {m}\_{0} + 2 l {m}\_{1}}}
\end{equation}

Here are the Lagrange equations:

```scheme
(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (literal-function 'c)
              (literal-function 'F)))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ {m}\_{0} {D}^{2}{x}\_{cm}\left( t \right) + {m}\_{1} {D}^{2}{x}\_{cm}\left( t \right)} \cr \cr \displaystyle{ {m}\_{0} {D}^{2}{y}\_{cm}\left( t \right) + {m}\_{1} {D}^{2}{y}\_{cm}\left( t \right)} \cr \cr \displaystyle{ {{2 {m}\_{0} {m}\_{1} Dc\left( t \right) c\left( t \right) D\theta\left( t \right) + {m}\_{0} {m}\_{1} {\left( c\left( t \right) \right)}^{2} {D}^{2}\theta\left( t \right)}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{ - l {m}\_{0} {m}\_{1} c\left( t \right) {\left( D\theta\left( t \right) \right)}^{2} + l {m}\_{0} {m}\_{1} {D}^{2}c\left( t \right) + {m}\_{0} F\left( t \right) c\left( t \right) + {m}\_{1} F\left( t \right) c\left( t \right)}\over {l {m}\_{0} + l {m}\_{1}}}} \cr \cr \displaystyle{ {{ - {{1}\over {2}} {l}^{2} + {{1}\over {2}} {\left( c\left( t \right) \right)}^{2}}\over {l}}}\end{bmatrix}
\end{equation}

That final equation states that \\(c(t) = l\\). Amazing!

### Part D: Substitute \\(c(t) = l\\)<a id="sec-1-22-5"></a>

> You may deduce from one of these equations that \\(c(t) = l\\). From this fact we get that \\(Dc = 0\\) and \\(D^2c = 0\\). Substitute these into the Lagrange equations you just computed to get the equation of motion for \\(x\_{CM}\\), \\(y\_{CM}\\), \\(\theta\\).

We can substitute the constant value of \\(c\\) using a function that always returns \\(l\\) to get simplified equations:

```scheme
(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ {m}\_{0} {D}^{2}{x}\_{cm}\left( t \right) + {m}\_{1} {D}^{2}{x}\_{cm}\left( t \right)} \cr \cr \displaystyle{ {m}\_{0} {D}^{2}{y}\_{cm}\left( t \right) + {m}\_{1} {D}^{2}{y}\_{cm}\left( t \right)} \cr \cr \displaystyle{ {{{l}^{2} {m}\_{0} {m}\_{1} {D}^{2}\theta\left( t \right)}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ {{ - l {m}\_{0} {m}\_{1} {\left( D\theta\left( t \right) \right)}^{2} + {m}\_{0} F\left( t \right) + {m}\_{1} F\left( t \right)}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ 0}\end{bmatrix}
\end{equation}

This is saying that the acceleration on the center of mass is 0.

The fourth equation, the equation of motion for the \\(c(t)\\), is interesting here. We need to pull in the definition of "reduced mass" from exercise 1.11:

```scheme
(define (reduced-mass m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))
```

If we let \\(m\\) be the reduced mass, this equation states:

\begin{equation}
\label{eq:constraint-force}
F(t) = m l \dot{\theta}^2
\end{equation}

We can verify this with Scheme by subtracting the two equations:

```scheme
(let* ((F (literal-function 'F))
       (theta (literal-function 'theta))
       (q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              theta
              (lambda (t) 'l)
              F))
       (L (L-free-constrained* 'm_0 'm_1 'l))
       (f ((Lagrange-equations L) q))

       (m (reduced-mass 'm_0 'm_1)))
  (->tex-equation
   (- (ref (f 't) 3)
      (- (F 't)
         (* m 'l (square ((D theta) 't)))))))
```

\begin{equation}
0
\end{equation}

### Part E: New Lagrangian<a id="sec-1-22-6"></a>

> Make a Lagrangian (\\(= T − V\\)) for the system described with the irredundant generalized coordinates \\(x\_{CM}\\), \\(y\_{CM}\\), \\(\theta\\) and compute the Lagrange equations from this Lagrangian. They should be the same equations as you derived for the same coordinates in part d.

For part e, I wrote this in the notebook - it is effectively identical to the substitution that is happening on the computer, so I'm going to ignore this. You just get more cancellations.

But let's go at it, for fun.

Here's the Lagrangian of 2 free particles:

```scheme
(define ((L-free2 m0 m1) local)
  (let* ((extract (extract-particle 2))

         (p0 (extract local 0))
         (q0 (coordinate p0))
         (qdot0 (velocity p0))

         (p1 (extract local 1))
         (q1 (coordinate p1))
         (qdot1 (velocity p1)))
    (+ (KE-particle m0 qdot0)
       (KE-particle m1 qdot1))))
```

Then a version of `cm-theta->rect` where we ignore \\(F\\), and sub in a constant \\(l\\):

```scheme
(define ((cm-theta->rect* m0 m1 l) local)
  (let* ((q (coordinate local))
         (x_cm (ref q 0))
         (y_cm (ref q 1))
         (theta (ref q 2))
         (total-mass (+ m0 m1))
         (m0-distance (* l (/ m1 total-mass)))
         (m1-distance (* l (/ m0 total-mass))))
    (up (- x_cm (* m0-distance (cos theta)))
        (- y_cm (* m0-distance (sin theta)))
        (+ x_cm (* m1-distance (cos theta)))
        (+ y_cm (* m1-distance (sin theta))))))
```

\#| cm-theta->rect\* |#

The Lagrangian:

```scheme
(define (L-free-constrained2 m0 m1 l)
  (compose (L-free2 m0 m1)
           (F->C (cm-theta->rect* m0 m1 l))))
```

Equations:

```scheme
(let* ((q (up (literal-function 'x_cm)
              (literal-function 'y_cm)
              (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (L1 (L-free-constrained* 'm_0 'm_1 'l))
       (L2 (L-free-constrained2 'm_0 'm_1 'l)))
  (->tex-equation
   ((- ((Lagrange-equations L1) q)
       ((Lagrange-equations L2) q))
    't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ 0} \cr \cr \displaystyle{ {{ - l {m}\_{0} {m}\_{1} {\left( D\theta\left( t \right) \right)}^{2} + {m}\_{0} F\left( t \right) + {m}\_{1} F\left( t \right)}\over {{m}\_{0} + {m}\_{1}}}} \cr \cr \displaystyle{ 0}\end{bmatrix}
\end{equation}

The only remaining equation is \eqref{eq:constraint-force} from above. This remains because the simplified Lagrangian ignores the \\(F\\) term.

## Exercise 1.22: Driven pendulum<a id="sec-1-23"></a>

```scheme
(load "ch1/utils.scm")
```

> Show that the Lagrangian (1.93) can be used to describe the driven pendulum (section 1.6.2), where the position of the pivot is a specified function of time: Derive the equations of motion using the Newtonian constraint force prescription, and show that they are the same as the Lagrange equations. Be sure to examine the equations for the constraint forces as well as the position of the pendulum bob.

I might be slightly misunderstanding this question. The problem states that we should use equations 1.91 and 1.92 in the book to express the equations of motion of the driven pendulum, and then to rederive the equations of motion using the Lagrangian.

The final note about examining the constraint forces means that we'll need to follow the approach of equation 1.21 and include a coordinate transformation from some \\(c(t)\\), and then substitute \\(c(t) = l\\) down the road.

### Part A: Newton's Equations<a id="sec-1-23-1"></a>

Step one is to use 1.91 and 1.92 in the book to express \\(F = ma\\). The only potential is a uniform gravitational potential:

\begin{equation}
V(t, \theta, \dot{\theta}) = m g (y\_s(t) - l \cos \theta)
\end{equation}

So equation 1.91 becomes, for the single pendulum bob:

\begin{equation}
\begin{aligned}
m D^2x(t) & = F(t) {{-x(t)} \over l} \cr
m D^2y(t) & = -mgl + F(t) {{y\_s(t) - y(t)} \over l}
\end{aligned}
\end{equation}

The assumption here is that the pendulum support sits at \\((0, y\_s(t))\\).

### Part B: Lagrangian<a id="sec-1-23-2"></a>

Now write the Lagrangian for the driven pendulum in rectangular coordinates. The constraint force takes the same shape as in exercise 1.21:

```scheme
(define (U-constraint q0 q1 F l)
  (* (/ F (* 2 l))
     (- (square (- q1 q0))
        (square l))))
```

The Lagrangian is similar, but only involves a single particle &#x2013; the pendulum bob. We can generate the constraint force by directly building the support's coordinates, rather than extracting them from the local tuple.

```scheme
(define ((L-driven-free m l y U) local)
  (let* ((extract (extract-particle 2))
         (bob (extract local 0))
         (q (coordinate bob))
         (qdot (velocity bob))
         (F (ref (coordinate local) 2)))
    (- (KE-particle m qdot)
       (U q)
       (U-constraint (up 0 (y (time local)))
                     q
                     F
                     l))))
```

Here is the now-familiar equation for a uniform gravitational potential, acting on the \\(y\\) coordinate:

```scheme
(define ((U-gravity g m) q)
  (let* ((y (ref q 1)))
    (* m g y)))
```

    #| U-gravity |#

Now use the new Lagrangian to generate equations of motion for the three coordinates \\(x\\), \\(y\\) and \\(F\\):

```scheme
(let* ((q (up (literal-function 'x)
              (literal-function 'y)
              (literal-function 'F)))
       (U (U-gravity 'g 'm))
       (y (literal-function 'y_s))
       (L (L-driven-free 'm 'l y U))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ {{l m {D}^{2}x\left( t \right) + F\left( t \right) x\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{g l m + l m {D}^{2}y\left( t \right) - F\left( t \right) {y}\_{s}\left( t \right) + F\left( t \right) y\left( t \right)}\over {l}}} \cr \cr \displaystyle{ {{ - {{1}\over {2}} {l}^{2} + {{1}\over {2}} {\left( {y}\_{s}\left( t \right) \right)}^{2} - {y}\_{s}\left( t \right) y\left( t \right) + {{1}\over {2}} {\left( y\left( t \right) \right)}^{2} + {{1}\over {2}} {\left( x\left( t \right) \right)}^{2}}\over {l}}}\end{bmatrix}
\end{equation}

The first two equations of motion match the equations we derived in part A, using Newton's equations. The third states that

\begin{equation}
l^2 = x(t)^2 + (y\_s(t) - y(t))^&2
\end{equation}

Verified, with some extra terms to force the simplification:

```scheme
(let* ((q (up (literal-function 'x)
              (literal-function 'y)
              (literal-function 'F)))
       (U (U-gravity 'g 'm))
       (y (literal-function 'y_s))
       (L (L-driven-free 'm 'l y U))
       (f ((Lagrange-equations L) q))
       (eq (ref (f 't) 2)))
  (->tex-equation
   (- eq
      (/ (* 1/2 (- (+ (square ((literal-function 'x) 't))
                      (square ((- y (literal-function 'y)) 't)))
                   (square 'l)))
         'l))))
```

\begin{equation}
0
\end{equation}

### Part C: Coordinate Change<a id="sec-1-23-3"></a>

Now we want to verify that we get the same Lagrangian and equations of motion as in 1.88 from the book. We also want to analyze the constraint forces. To do this we need to introduce a coordinate change.

To analyze the constraint forces, we have to do the same trick as in exercise 1.21 and use a coordinate \\(c(t) = l\\). The new coordinates are \\((\theta, c, F)\\):

```scheme
(define ((driven-polar->rect y) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (c (ref q 1))
         (F (ref q 2)))
    (up (* c (sin theta))
        (- (y (time local)) (* c (cos theta)))
        F)))
```

    #| driven-polar->rect |#

Compose the coordinate change with the rectangular Lagrangian:

```scheme
(define (L-driven-pend m l y U)
  (compose (L-driven-free m l y U)
           (F->C (driven-polar->rect y))))
```

    #| L-driven-pend |#

Examine the Lagrangian itself, after the coordinate transformation. (Notice that we're using a constant function for \\(c(t)\\) that always returns \\(l\\).)

```scheme
(let* ((q (up (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (y (literal-function 'y_s))
       (L (L-driven-pend 'm 'l y (U-gravity 'g 'm)))
       (f (compose L (Gamma q))))
  (->tex-equation
   (f 't)))
```

\begin{equation}
{{1}\over {2}} {l}^{2} m {\left( D\theta\left( t \right) \right)}^{2} + l m D{y}\_{s}\left( t \right) \sin\left( \theta\left( t \right) \right) D\theta\left( t \right) + g l m \cos\left( \theta\left( t \right) \right) - g m {y}\_{s}\left( t \right) + {{1}\over {2}} m {\left( D{y}\_{s}\left( t \right) \right)}^{2}
\end{equation}

Looks just like equation 1.88.

Next, examine the Lagrange equations, using the same substitution of \\(c(t) = l\\):

```scheme
(let* ((q (up (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (y (literal-function 'y_s))
       (L (L-driven-pend 'm 'l y (U-gravity 'g 'm)))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (f 't)))
```

\begin{equation}
\begin{bmatrix} \displaystyle{ g l m \sin\left( \theta\left( t \right) \right) + {l}^{2} m {D}^{2}\theta\left( t \right) + l m {D}^{2}{y}\_{s}\left( t \right) \sin\left( \theta\left( t \right) \right)} \cr \cr \displaystyle{  - l m {\left( D\theta\left( t \right) \right)}^{2} - g m \cos\left( \theta\left( t \right) \right) - m {D}^{2}{y}\_{s}\left( t \right) \cos\left( \theta\left( t \right) \right) + F\left( t \right)} \cr \cr \displaystyle{ 0}\end{bmatrix}
\end{equation}

The third equation is 0 because of the substitution of constant \\(c(t) = l\\). The first equation of motion, for \\(\theta\\), is identical to the equation on page 52.

The second equation describes the constraint force on the driven pendulum as a function of the other coordinates and the support position.

## Exercise 1.23: Fill in the details<a id="sec-1-24"></a>

```scheme
(load "ch1/utils.scm")
```

TODO: Expand out the explicit Lagrangian, using a coordinate transformation, and do the manual substitution&#x2026;

## Exercise 1.24: Constraint forces<a id="sec-1-25"></a>

This is a special case of a solution we found in exercise 1.22. In that exercise, we found the constraint forces on a driven pendulum. By setting \\(y\_s(t) = l\\), we can read off the constraint forces for the undriven pendulum.

```scheme
(load "ch1/utils.scm")
```

Take some definitions that we need:

```scheme
(define ((L-driven-free m l y U) local)
  (let* ((extract (extract-particle 2))
         (bob (extract local 0))
         (q (coordinate bob))
         (qdot (velocity bob))
         (F (ref (coordinate local) 2)))
    (- (KE-particle m qdot)
       (U q)
       (U-constraint (up 0 (y (time local)))
                     q
                     F
                     l))))

(define ((U-gravity g m) q)
  (let* ((y (ref q 1)))
    (* m g y)))

(define ((driven-polar->rect y) local)
  (let* ((q (coordinate local))
         (theta (ref q 0))
         (c (ref q 1))
         (F (ref q 2)))
    (up (* c (sin theta))
        (- (y (time local)) (* c (cos theta)))
        F)))

(define (L-driven-pend m l y U)
  (compose (L-driven-free m l y U)
           (F->C (driven-polar->rect y))))
```

The second equation of motion, for the \\(c\\) coordinate, gives us an equation in terms of tension. Substitute in a constant pendulum support position by defining the support position function to be `(lambda (t) 'l)`:

```scheme
(let* ((q (up (literal-function 'theta)
              (lambda (t) 'l)
              (literal-function 'F)))
       (y (lambda (t) 'l))
       (L (L-driven-pend 'm 'l y (U-gravity 'g 'm)))
       (f ((Lagrange-equations L) q)))
  (->tex-equation
   (ref (f 't) 1)))
```

\begin{equation}
 - l m {\left( D\theta\left( t \right) \right)}^{2} - g m \cos\left( \theta\left( t \right) \right) + F\left( t \right)
\end{equation}

Solve for \\(F(t)\\), the tension on the pendulum linkage:

\begin{equation}
F(t) = m (g \cos \theta + l \dot{\theta}^2)
\end{equation}

## Exercise 1.25: Foucalt pendulum Lagrangian<a id="sec-1-26"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.26: Properties of \\(D\_t\\)<a id="sec-1-27"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.27: Lagrange equations for total time derivatives<a id="sec-1-28"></a>

```scheme
(load "ch1/utils.scm")
```

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

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.31: Foucault pendulum evolution<a id="sec-1-32"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.32: Time-dependent constraints<a id="sec-1-33"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.33: Falling off a log<a id="sec-1-34"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.34: Driven spherical pendulum<a id="sec-1-35"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.35: Restricted equations of motion<a id="sec-1-36"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.36: Noether integral<a id="sec-1-37"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.37: Velocity transformation<a id="sec-1-38"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.38: Properties of \\(E\\)<a id="sec-1-39"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.39: Combining Lagrangians<a id="sec-1-40"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.40: Bead on a triaxial surface<a id="sec-1-41"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.41: Motion of a tiny golf ball<a id="sec-1-42"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.42: Augmented Lagrangian<a id="sec-1-43"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.43: A numerical investigation<a id="sec-1-44"></a>

```scheme
(load "ch1/utils.scm")
```

## Exercise 1.44: Double pendulum behavior<a id="sec-1-45"></a>

```scheme
(load "ch1/utils.scm")
```

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
