  - [Exercise 1.1: Degrees of Freedom and 1.2: Generalized Coordinates](#sec-1)
  - [Exercise 1.3: Fermat optics](#sec-2)
    - [Law of Reflection](#sec-2-1)
    - [Law of Refraction](#sec-2-2)
  - [Section 1.4: Computing Actions](#sec-3)
  - [Exercise 1.4: Lagrangian actions](#sec-4)
  - [Paths of Minimum Action](#sec-5)
    - [Finding trajectories that minimize action](#sec-5-1)
  - [Exercise 1.5: Solution process](#sec-6)
  - [Exercise 1.6: Minimizing action](#sec-7)
  - [Exercise 1.7: Properties of \\(\delta\\)](#sec-8)
  - [Exercise 1.8: Implementation of \\(\delta\\)](#sec-9)
  - [Orbital Motion](#sec-10)
  - [Exercise 1.9: Lagrange's equations](#sec-11)
  - [Exercise 1.10: Higher-derivative Lagrangians](#sec-12)
  - [Exercise 1.11: Kepler's third law](#sec-13)
  - [Exercise 1.12: Lagrange's equations (code)](#sec-14)
  - [Exercise 1.13: Higher-derivative Lagrangians (code)](#sec-15)
  - [Exercise 1.14: Coordinate-independence of Lagrange equations](#sec-16)
  - [Exercise 1.15: Equivalence](#sec-17)
  - [Exercise 1.16: Central force motion](#sec-18)
  - [Exercise 1.17: Bead on a helical wire](#sec-19)
  - [Exercise 1.18: Bead on a triaxial surface](#sec-20)
  - [Exercise 1.19: Two-bar linkage](#sec-21)
  - [Exercise 1.20: Sliding pendulum](#sec-22)
  - [Exercise 1.21: A dumbbell](#sec-23)
  - [Exercise 1.22: Driven pendulum](#sec-24)
  - [Exercise 1.23: Fill in the details](#sec-25)
  - [Exercise 1.24: Constraint forces](#sec-26)
  - [Exercise 1.25: Foucalt pendulum Lagrangian](#sec-27)
  - [Exercise 1.26: Properties of \\(D\_t\\)](#sec-28)
  - [Exercise 1.27: Lagrange equations for total time derivatives](#sec-29)
  - [Exercise 1.28: Total Time Derivatives](#sec-30)
    - [part A](#sec-30-1)
    - [Part B](#sec-30-2)
    - [Part C](#sec-30-3)
    - [Part D](#sec-30-4)
    - [Part E](#sec-30-5)
    - [Part F](#sec-30-6)
  - [Exercise 1.29: Galilean Invariance](#sec-31)
  - [Exercise 1.30: Orbits in a central potential](#sec-32)
  - [Exercise 1.31: Foucault pendulum evolution](#sec-33)
  - [Exercise 1.32: Time-dependent constraints](#sec-34)
  - [Exercise 1.33: Falling off a log](#sec-35)
  - [Exercise 1.34: Driven spherical pendulum](#sec-36)
  - [Exercise 1.35: Restricted equations of motion](#sec-37)
  - [Exercise 1.36: Noether integral](#sec-38)
  - [Exercise 1.37: Velocity transformation](#sec-39)
  - [Exercise 1.38: Properties of \\(E\\)](#sec-40)
  - [Exercise 1.39: Combining Lagrangians](#sec-41)
  - [Exercise 1.40: Bead on a triaxial surface](#sec-42)
  - [Exercise 1.41: Motion of a tiny golf ball](#sec-43)
  - [Exercise 1.42: Augmented Lagrangian](#sec-44)
  - [Exercise 1.43: A numerical investigation](#sec-45)
  - [Exercise 1.44: Double pendulum behavior](#sec-46)

The book's about motion, how things move. Here's a nice image of some action getting minimized:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-05-29_10-12-19_AJBpDgU.gif)

# Exercise 1.1: Degrees of Freedom and 1.2: Generalized Coordinates<a id="sec-1"></a>

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

# Exercise 1.3: Fermat optics<a id="sec-2"></a>

The [problem](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-3) states:

> Fermat observed that the laws of reflection and refraction could be accounted for by the following facts: Light travels in a straight line in any particular medium with a velocity that depends upon the medium. The path taken by a ray from a source to a destination through any sequence of media is a path of least total time, compared to neighboring paths. Show that these facts imply the laws of reflection and refraction.

## Law of Reflection<a id="sec-2-1"></a>

The [law of reflection](https://en.wikipedia.org/wiki/Reflection_(physics)#Laws_of_reflection) is described in the footnote:

> For reflection the angle of incidence is equal to the angle of reflection.

We have to show that if we consider all possible paths from a given starting point to a given endpoint, the path of minimum time will give us the law of reflection.

Here's the setup:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_10-31-24_screenshot.png)

If we force the light to bounce off of a mirror, then we have to figure out where it will hit, where \\(x\_p\\) is, to minimize the time between the start and end points.

There are two ways to solve this problem. The first is to remember this fact from the problem definition:

> Light travels in a straight line in any particular medium with a velocity that depends upon the medium.

There's no medium change, so if there were no mirror in its path, the light bean would continue in a straight line. Instead of figuring out what the beam will do when it hits the mirror, reflect the endpoint across the mirror and draw a straight line between the start and "end":

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png)

The angle that the beam makes with the plane of the mirror is the same on both sides of the mirror.

Now reflect the the "end" point and the segment of the beam that's crossed the mirror back up across the mirror, and you'll find that \\(\theta\_1 = \theta\_2\\). (Pardon my not-to-scale drawing.)

We can also solve this with calculus. Because we don't change media, the speed of light \\(c\\) is constant, so minimizing the total distance is equivalent to minimizing the time.

Set \\(x\_1 = 0\\) for convenience, and write the total distance the light travels as a function of \\(x\_p\\):

\begin{equation}
f(x\_p) = \sqrt{y\_1^2 + x\_p^2} + \sqrt{(x\_2 - x\_p)^2 + y\_2^2}
\end{equation}

We can also define this function in Scheme.

```scheme
(define ((total-distance x1 y1 x2 y2) xp)
  (+ (sqrt (+ (square (+ x1 xp))
              (square y1)))
     (sqrt (+ (square (- x2 (+ x1 xp)))
              (square y2)))))
```

Here's the function again, with general \\(t\_1\\):

```scheme
(->tex-equation
 ((total-distance 'x_1 'y_1 'x_2 'y_2) 'x_p))
```

\begin{equation}
\sqrt{{{x}\_{1}}^{2} + 2 {x}\_{1} {x}\_{p} + {{x}\_{p}}^{2} + {{y}\_{1}}^{2}} + \sqrt{{{x}\_{1}}^{2} - 2 {x}\_{1} {x}\_{2} + 2 {x}\_{1} {x}\_{p} + {{x}\_{2}}^{2} - 2 {x}\_{2} {x}\_{p} + {{x}\_{p}}^{2} + {{y}\_{2}}^{2}}
\end{equation}

Now we need to take the derivative with respect to \\(x\_p\\), set it equal to 0 and solve for \\(x\_p\\). That will give us the stationary point of the total distance.

The derivative will look cleaner if we keep the components of the sum separate. Redefine the function to return a tuple:

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

The structure of the problem makes the next step a little annoying. If you look at the first component, you might recognize the \\(\cos \theta\_1\\), the base \\(x\_p\\) of the left triangle over the total length:

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-10_10-36-53_screenshot.png)

The bottom component is \\(-\cos \theta\_2\\), or \\({- (x\_2 - x\_p)}\\) over the length of the right segment. Add these together and set them equal to 0, and you find that

\begin{equation}
\label{eq:reflect-laws}
\cos \theta\_1 = \cos \theta\_2 \implies \theta\_1 = \theta\_2
\end{equation}

This isn't so obvious from the scheme code, but you can use Scheme to check this result. If the two angles are equal, then the left and right triangles are similar, and the ratio of the base to height is equal:

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

## Law of Refraction<a id="sec-2-2"></a>

The law of refraction is also called [Snell's law](https://en.wikipedia.org/wiki/Snell%27s_law). Here's the description from the footnote:

> Refraction is described by Snell's law: when light passes from one medium to another, the ratio of the sines of the angles made to the normal to the interface is the inverse of the ratio of the refractive indices of the media. The refractive index is the ratio of the speed of light in the vacuum to the speed of light in the medium.

# Section 1.4: Computing Actions<a id="sec-3"></a>

This is the first demo of how any of this stuff works, starting on page 15.

Here's our first Lagrangian, super simple.

```scheme
(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))
```

Suppose we let q denote a coordinate path function that maps time to position components:

```scheme
(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))
```

"Gamma" (looks like an L reflected across the origin) takes a coordinate path and returns a function of time that gives the local tuple. Looks like that fucker defaults to 2 levels deep, but you can call (Gamma 4) to get more derivatives.

The returned thing is called the "local tuple":

```scheme
((Gamma q) 't)
```

This is just (t, q(t), (Dq)(t), &#x2026;.) Where D is the derivative maybe of the vector, which does partial derivatives along each component. (Q: can a component of the coordinate path depend on the others? YES, and that would impose constraints beyond the degrees of freedom.)

```scheme
((compose (L-free-particle 'm) (Gamma q)) 't)
```

So this little bastard doesn't depend on the coordinate system you choose, as long as its true that the lagrangian (kinetic energy equation) is the same for all reference frames.

Lagrangian action! Minimal lagrangian action is key.

```scheme
(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))
```

For a particle with mass 3, between 0 and 10&#x2026; look at page 17 for an example here. This is an example path:

```scheme
(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))
```

And we can run it here:

```scheme
(Lagrangian-action (L-free-particle 3) test-path 0.0 10.0)
```

# Exercise 1.4: Lagrangian actions<a id="sec-4"></a>

![img](https://github.com/sritchie/sicm/raw/master/images/Lagrangian_Mechanics/2020-06-08_11-47-37_imgs%2Fapp%2Fsritchie%2FVqfQv6wgb-.png.png)

# Paths of Minimum Action<a id="sec-5"></a>

Next, we'll take the old langrangian calculation for the straight line and fuck with the line by small variations; or some small epsilon multiplied by the effect of some other function added to the original q. Eta baby!

This makes a new function that has zeroes at t1 and t2.

```scheme
(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))
```

Boom, calculate the action AGAIN for a path that's pretty close&#x2026;

```scheme
(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* epsilon eta))
                       t1
                       t2)))

(let ((action-fn (varied-free-particle-action 3 test-path
                                              (up sin cos square)
                                              0.0 10.0)))
  (action-fn 0.001))
```

Now we can do a minimization over -2.0 to 1.0:

```scheme
(let ((action-fn (varied-free-particle-action
                  3.0 test-path
                  (up sin cos square)
                  0.0 10.0)))
  (minimize action-fn -2.0 1.0))
```

## Finding trajectories that minimize action<a id="sec-5-1"></a>

First build up a function that hits all the listed points.

```scheme
(define (make-path t0 q0 t1 q1 qs)
  (let ((n (length qs)))
    (let ((ts (linear-interpolants t0 t1 n)))
      (Lagrange-interpolation-function
       (append (list q0) qs (list q1))
       (append (list t0) ts (list t1))))))

(define ((parametric-path-action L t0 q0 t1 q1) qs)
  (let ((path (make-path t0 q0 t1 q1 qs)))
    (Lagrangian-action L path t0 t1)))

(define (find-path L t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action L t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))
```

That's pretty sick. Now we can write a version that does some mother fucking plotting. First, a new lagrangian for a spring system. This one's the difference between the kinetic and potential energies of a spring system.

```scheme
(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))
```

# Exercise 1.5: Solution process<a id="sec-6"></a>

watch the progress of the minimization. This is not great design, since we're overwriting the previous function and depending on the closure, but let's follow the text.

Defines a window:

```scheme
(define win2
  (frame 0. :pi/2 0. 1.2))
```

new version of this that prints:

```scheme
(define ((parametric-path-action Lagrangian t0 q0 t1 q1)
         intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
    ;; display path
    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
    ;; compute action
    (Lagrangian-action Lagrangian path t0 t1)))
```

And boom, we find a path (and get to watch a pretty chart):

```scheme
(define (run-q)
  (find-path (L-harmonic 1.0 1.0) 0. 1. :pi/2 0. 3))
```

# Exercise 1.6: Minimizing action<a id="sec-7"></a>

The problem asks:

> Suppose we try to obtain a path by minimizing an action for an impossible problem. For example, suppose we have a free particle and we impose endpoint conditions on the velocities as well as the positions that are inconsistent with the particle being free. Does the formalism protect itself from such an unpleasant attack? You may find it illuminating to program it and see what happens.

I spent some time thinking about this, because when I attacked this book five years ago this problem clearly tripped me up.

Let's say you take, as they suggest, some path, and impose velocity constraints on the endpoints in addition to the position constraints.

Usually, you constrain the coordinates at each endpoint and force a path that minimizes the action between two times. So what does it mean to impose velocity conditions? The key is to realize that on the computer, you're forcing a path to be composed of a bunch of discrete points. If you can force a point into the path that is NOT controlled by the optimizer, then you can force a velocity at some point in the path that makes no sense for minimal action.

```scheme
(define (((parametric-path-action* win)
          Lagrangian t0 q0 offset0 t1 q1 offset1)
         intermediate-qs)
  (let ((intermediate-qs* (append (list (- q0 offset0))
                                  intermediate-qs
                                  (list (+ q1 offset1)))))
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs*)))
      ;; display path
      (graphics-clear win)
      (plot-function win path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1))))

;; Version of find path that allows for an offset to the initial and
;; final points.

(define ((find-path* win) L t0 q0 offset0 t1 q1 offset1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let* ((action (parametric-path-action* win))
           (minimizing-qs
            (multidimensional-minimize
             (action L t0 q0 offset0 t1 q1 offset1)
             initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))

;; This runs (and graphs!) the motion of a free particle using the
;; fucked up path.
(define (one-six offset0 offset1 n)
  (let* ((tmax 10)
         (win (frame -1 (+ tmax 1) 0. (+ 1.2 offset0 offset1)))
         (find (find-path* win))
         (L (L-free-particle 3.0))
         (path (find L
                     0. 1. offset0
                     tmax 0. offset1
                     n)))
    (Lagrangian-action L path 0 tmax)))
```

What happens when you program this? You get a funky, wiggly path like this: (insert the gif!)

And you can show that the action you calculate is NOT in fact the minimum. Here's an interface that says "take 3 interpolated points, and force an offset point of a small amount into the action.

```scheme
(one-six 0 0 3)
```

```scheme
(one-six 0.2 0 3)
```

You <span class="underline"><span class="underline">still</span></span> can get the process to converge! But that is only because you're not minimizing action with respect to some Lagrangian anymore; you're minimizing the action of two constraints. You have the Lagrangian, and then the warring goal of the polynomial interpolation. At some point, the minimizer breaks as you almost certainly oscillate between two paths, as each constraint tugs at you.

If you make the impulse too big, then the war is too hardcore and the process never converges. But it's important to note here the details of the optimizer. The only reason this can "work" is settings on the optimizer.

# Exercise 1.7: Properties of \\(\delta\\)<a id="sec-8"></a>

# Exercise 1.8: Implementation of \\(\delta\\)<a id="sec-9"></a>

This FEELS weird&#x2026; but we want something that is&#x2026; well, let's see.

```scheme
(define (((delta eta) f) q)
  (let (g (lambda (eps)
            (f (q + (* eps eta)))))
    ((D g) 0)))
```

Why does this work? WELL&#x2026; we need a way to force the limit in.

this is a PATH function, remember. This takes a path function, then passes it into \\(\Gamma\\), and composes THAT with F. F is a function from the local tuple to some output variable. You can imagine it as the Langrangian, for example.

The local tuple type defined here can take any number of path components.

```scheme
(define (f q)
  (let* ((Local (Up Real (UP* Real) (UP* Real)))
         (F (literal-function 'F (-> Local Real))))
    (compose F (Gamma q))))
```

This is a path function that returns a 2d path; we can use this as an example.

```scheme
(define q (literal-function 'q (-> Real (Up Real Real))))
```

# Orbital Motion<a id="sec-10"></a>

Page 31.

```scheme
(define ((L-orbital mass mu) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (+ (* 1/2 mass (square qdot))
       (/ mu (sqrt (square q))))))

(define q2
  (up (literal-function 'xi)
      (literal-function 'eta)))
```

To test:

```scheme
((compose ((partial 1) (L-orbital 'm 'mu)) (Gamma q2)) 't)
```

# Exercise 1.9: Lagrange's equations<a id="sec-11"></a>

# Exercise 1.10: Higher-derivative Lagrangians<a id="sec-12"></a>

# Exercise 1.11: Kepler's third law<a id="sec-13"></a>

# Exercise 1.12: Lagrange's equations (code)<a id="sec-14"></a>

# Exercise 1.13: Higher-derivative Lagrangians (code)<a id="sec-15"></a>

# Exercise 1.14: Coordinate-independence of Lagrange equations<a id="sec-16"></a>

# Exercise 1.15: Equivalence<a id="sec-17"></a>

NOTE - I have a strong suspicion here that we can show that what is actually going on is that we end up with a total time derivative that we can ignore. The final terms at the end that cancel&#x2026; why is it that they work out the way they do? It would be nice to try and use the time-derivative test machinery we built to take a look there.

This one was a serious doozy. I think that this exercise can be a great way to show off the computer algebra system, and show off the steps that I go through to make a proof.

But I also want to pull back and stare at the formula. What is going on? What is the meaning of the extra terms? If we can say, for example, that they're a total time derivative, looking at the future, that would be great. There has to be a reason that the Lagrangian doesn't change.

The same thing happens when you look at a new Lagrangian and see a "fictitious force term" for, say, centrifugal force. There is something going on here.

Checking that composition distributes over multiplication&#x2026;

```scheme
(define f (literal-function 'f))
(define g (literal-function 'g))
(define h (literal-function 'h))
```

looks good! These are the same expression.

```scheme
((compose (* f g) h) 't)
((* (compose f h) (compose g h)) 't)
```

This is the general form of a path transformation; big surprise, this is very close to the code on page 46. I'm going to keep my version, since I don't want to get too confused, here.

```scheme
(define ((F->C F) local)
  (let ((t (time local))
        (x (coordinate local))
        (v (velocity local)))
    (up t
        (F t x)
        (+ (((partial 0) F) t x)
           (* (((partial 1) F) t x)
              v)))))
```

Here's a literal function we can play with.

```scheme
(define F*
  (literal-function 'F (-> (X Real Real) Real)))
```

Okay, boom, this is the literal function.

```scheme
(define q-prime
  (literal-function 'q-prime))
```

This is the manual generation of q from q-prime.

```scheme
(define ((to-q F) qp)
  (lambda (t) (F t (qp t))))
```

We can check that these are now equal. This uses C to get us to q

```scheme
((compose (F->C F*) (Gamma q-prime)) 't)
```

And this does it by passing in q manually.

```scheme
((Gamma ((to-q F*) q-prime)) 't)
```

I can convert the proof to code, no problem, by showing that these sides are equal.

YES!! the final step of my proof was the note that these are equal. THIS IS HUGE!!!

```scheme
((compose (lambda (x) (ref x 1)) ((partial 1) (F->C F*)) (Gamma q-prime)) 't)
((compose (lambda (x) (ref x 2)) ((partial 2) (F->C F*)) (Gamma q-prime)) 't)
```

Just for fun, note that this successfully pushes things inside gamma.

```scheme
(let ((L (literal-function 'L (-> (UP Real Real Real) Real)))
      (C (F->C F*)))
  ((Gamma ((to-q ((partial 1) F*)) q-prime)) 't))

(define (p->r t polar-tuple)
  (let* ((r (ref polar-tuple 0))
         (phi (ref polar-tuple 1))
         (x (* r (cos phi)))
         (y (* r (sin phi))))
    (up x y)))

(literal-function 'q-prime (-> Real (UP Real Real)))((Gamma ((to-q p->r) )) 't)
```

trying again. get a function:

```scheme
(define q
  ;; time to x y.
  (literal-function 'q (-> Real (UP Real Real))))

(define (C local)
  (up (time local)
     (square (coordinate local))
     (velocity local)))

((compose C (Gamma q)) 't)
```

That's good for now.

# Exercise 1.16: Central force motion<a id="sec-18"></a>

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

# Exercise 1.17: Bead on a helical wire<a id="sec-19"></a>

# Exercise 1.18: Bead on a triaxial surface<a id="sec-20"></a>

# Exercise 1.19: Two-bar linkage<a id="sec-21"></a>

# Exercise 1.20: Sliding pendulum<a id="sec-22"></a>

# Exercise 1.21: A dumbbell<a id="sec-23"></a>

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

# Exercise 1.22: Driven pendulum<a id="sec-24"></a>

# Exercise 1.23: Fill in the details<a id="sec-25"></a>

# Exercise 1.24: Constraint forces<a id="sec-26"></a>

# Exercise 1.25: Foucalt pendulum Lagrangian<a id="sec-27"></a>

# Exercise 1.26: Properties of \\(D\_t\\)<a id="sec-28"></a>

# Exercise 1.27: Lagrange equations for total time derivatives<a id="sec-29"></a>

# Exercise 1.28: Total Time Derivatives<a id="sec-30"></a>

```scheme
(load "ch1/utils.scm")
```

## part A<a id="sec-30-1"></a>

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

## Part B<a id="sec-30-2"></a>

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

## Part C<a id="sec-30-3"></a>

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

## Part D<a id="sec-30-4"></a>

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

## Part E<a id="sec-30-5"></a>

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

## Part F<a id="sec-30-6"></a>

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

# Exercise 1.29: Galilean Invariance<a id="sec-31"></a>

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

# Exercise 1.30: Orbits in a central potential<a id="sec-32"></a>

# Exercise 1.31: Foucault pendulum evolution<a id="sec-33"></a>

# Exercise 1.32: Time-dependent constraints<a id="sec-34"></a>

# Exercise 1.33: Falling off a log<a id="sec-35"></a>

# Exercise 1.34: Driven spherical pendulum<a id="sec-36"></a>

# Exercise 1.35: Restricted equations of motion<a id="sec-37"></a>

# Exercise 1.36: Noether integral<a id="sec-38"></a>

# Exercise 1.37: Velocity transformation<a id="sec-39"></a>

# Exercise 1.38: Properties of \\(E\\)<a id="sec-40"></a>

# Exercise 1.39: Combining Lagrangians<a id="sec-41"></a>

# Exercise 1.40: Bead on a triaxial surface<a id="sec-42"></a>

# Exercise 1.41: Motion of a tiny golf ball<a id="sec-43"></a>

# Exercise 1.42: Augmented Lagrangian<a id="sec-44"></a>

# Exercise 1.43: A numerical investigation<a id="sec-45"></a>

# Exercise 1.44: Double pendulum behavior<a id="sec-46"></a>
