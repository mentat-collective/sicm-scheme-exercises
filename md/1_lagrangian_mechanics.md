  - [Configuration Spaces](#sec-1)
      - [Exercises 1.1: Degrees of Freedom and 1.2: Generalized Coordinates](#sec-1-0-1)
  - [Generalized Coordinates](#sec-2)
      - [Exercise 1.3: Fermat optics](#sec-2-0-1)
      - [Exercise 1.4: Lagrangian actions](#sec-2-0-2)
      - [Exercise 1.5: Solution process](#sec-2-0-3)
      - [Exercise 1.6: Minimizing action](#sec-2-0-4)
      - [Exercise 1.7: Properties of \\(\delta\\)](#sec-2-0-5)
      - [Exercise 1.8: Implementation of \\(\delta\\)](#sec-2-0-6)
      - [Exercise 1.9: Lagrange's equations](#sec-2-0-7)
      - [Exercise 1.10: Higher-derivative Lagrangians](#sec-2-0-8)
      - [Exercise 1.11: Kepler's third law](#sec-2-0-9)
      - [Exercise 1.12: Lagrange's equations (code)](#sec-2-0-10)
      - [Exercise 1.13: Higher-derivative Lagrangians (code)](#sec-2-0-11)
      - [Exercise 1.14: Coordinate-independence of Lagrange equations](#sec-2-0-12)
      - [Exercise 1.15: Equivalence](#sec-2-0-13)
      - [Exercise 1.16: Central force motion](#sec-2-0-14)
      - [Exercise 1.17: Bead on a helical wire](#sec-2-0-15)
      - [Exercise 1.18: Bead on a triaxial surface](#sec-2-0-16)
      - [Exercise 1.19: Two-bar linkage](#sec-2-0-17)
      - [Exercise 1.20: Sliding pendulum](#sec-2-0-18)
      - [Exercise 1.21: A dumbbell](#sec-2-0-19)
      - [Exercise 1.22: Driven pendulum](#sec-2-0-20)
      - [Exercise 1.23: Fill in the details](#sec-2-0-21)
      - [Exercise 1.24: Constraint forces](#sec-2-0-22)
      - [Exercise 1.25: Foucalt pendulum Lagrangian](#sec-2-0-23)
      - [Exercise 1.26: Properties of \\(D\_t\\)](#sec-2-0-24)
      - [Exercise 1.27: Lagrange equations for total time derivatives](#sec-2-0-25)
      - [Exercise 1.28: Total Time Derivatives](#sec-2-0-26)
      - [Exercise 1.29: Galilean Invariance](#sec-2-0-27)
      - [Exercise 1.30: Orbits in a central potential](#sec-2-0-28)
      - [Exercise 1.31: Foucault pendulum evolution](#sec-2-0-29)
      - [Exercise 1.32: Time-dependent constraints](#sec-2-0-30)
      - [Exercise 1.33: Falling off a log](#sec-2-0-31)
      - [Exercise 1.34: Driven spherical pendulum](#sec-2-0-32)
      - [Exercise 1.35: Restricted equations of motion](#sec-2-0-33)
      - [Exercise 1.36: Noether integral](#sec-2-0-34)
      - [Exercise 1.37: Velocity transformation](#sec-2-0-35)
      - [Exercise 1.38: Properties of \\(E\\)](#sec-2-0-36)
      - [Exercise 1.39: Combining Lagrangians](#sec-2-0-37)
      - [Exercise 1.40: Bead on a triaxial surface](#sec-2-0-38)
      - [Exercise 1.41: Motion of a tiny golf ball](#sec-2-0-39)
      - [Exercise 1.42: Augmented Lagrangian](#sec-2-0-40)
      - [Exercise 1.43: A numerical investigation](#sec-2-0-41)
      - [Exercise 1.44: Double pendulum behavior](#sec-2-0-42)
  - [The Principle of Stationary Action](#sec-3)
  - [Computing Actions](#sec-4)
  - [The Euler–Lagrange Equations](#sec-5)
    - [Derivation of the Lagrange Equations](#sec-5-1)
    - [Computing Lagrange's Equations](#sec-5-2)
  - [How to Find Lagrangians](#sec-6)
    - [Coordinate Transformations](#sec-6-1)
    - [Systems with Rigid Constraints](#sec-6-2)
    - [Constraints as Coordinate Transformations](#sec-6-3)
    - [The Lagrangian Is Not Unique](#sec-6-4)
  - [Evolution of Dynamical State](#sec-7)
  - [Conserved Quantities](#sec-8)
    - [Conserved Momenta](#sec-8-1)
    - [Energy Conservation](#sec-8-2)
    - [Central Forces in Three Dimensions](#sec-8-3)
    - [The Restricted Three-Body Problem](#sec-8-4)
    - [Noether's Theorem](#sec-8-5)
  - [Abstraction of Path Functions](#sec-9)
  - [Constrained Motion](#sec-10)
    - [Coordinate Constraints](#sec-10-1)
    - [Derivative Constraints](#sec-10-2)
    - [Nonholonomic Systems](#sec-10-3)
  - [Summary](#sec-11)
  - [Projects](#sec-12)

# Configuration Spaces<a id="sec-1"></a>

Discuss the section here. This is a test of a new heading style.

Here's a nice image of some action getting minimized:

![img](https://i.imgur.com/AJBpDgU.gif)

### Exercises 1.1: Degrees of Freedom and 1.2: Generalized Coordinates<a id="sec-1-0-1"></a>

For each of the mechanical systems described below, give the number of degrees of freedom of the configuration space.

(Exercise 1.2 asks about the generalized coordinates of each, so I'll note those here too.)

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

# Generalized Coordinates<a id="sec-2"></a>

### Exercise 1.3: Fermat optics<a id="sec-2-0-1"></a>

Fermat Optics. It's in the folded section in the blue notebook. Do it again!

### Exercise 1.4: Lagrangian actions<a id="sec-2-0-2"></a>

### Exercise 1.5: Solution process<a id="sec-2-0-3"></a>

### Exercise 1.6: Minimizing action<a id="sec-2-0-4"></a>

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

### Exercise 1.7: Properties of \\(\delta\\)<a id="sec-2-0-5"></a>

### Exercise 1.8: Implementation of \\(\delta\\)<a id="sec-2-0-6"></a>

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

### Exercise 1.9: Lagrange's equations<a id="sec-2-0-7"></a>

### Exercise 1.10: Higher-derivative Lagrangians<a id="sec-2-0-8"></a>

### Exercise 1.11: Kepler's third law<a id="sec-2-0-9"></a>

### Exercise 1.12: Lagrange's equations (code)<a id="sec-2-0-10"></a>

### Exercise 1.13: Higher-derivative Lagrangians (code)<a id="sec-2-0-11"></a>

### Exercise 1.14: Coordinate-independence of Lagrange equations<a id="sec-2-0-12"></a>

### Exercise 1.15: Equivalence<a id="sec-2-0-13"></a>

This one was a serious doozy. I plan on going through and co

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

### Exercise 1.16: Central force motion<a id="sec-2-0-14"></a>

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

### Exercise 1.17: Bead on a helical wire<a id="sec-2-0-15"></a>

### Exercise 1.18: Bead on a triaxial surface<a id="sec-2-0-16"></a>

### Exercise 1.19: Two-bar linkage<a id="sec-2-0-17"></a>

### Exercise 1.20: Sliding pendulum<a id="sec-2-0-18"></a>

### Exercise 1.21: A dumbbell<a id="sec-2-0-19"></a>

The uneven dumbbell.

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

### Exercise 1.22: Driven pendulum<a id="sec-2-0-20"></a>

### Exercise 1.23: Fill in the details<a id="sec-2-0-21"></a>

### Exercise 1.24: Constraint forces<a id="sec-2-0-22"></a>

### Exercise 1.25: Foucalt pendulum Lagrangian<a id="sec-2-0-23"></a>

### Exercise 1.26: Properties of \\(D\_t\\)<a id="sec-2-0-24"></a>

### Exercise 1.27: Lagrange equations for total time derivatives<a id="sec-2-0-25"></a>

### Exercise 1.28: Total Time Derivatives<a id="sec-2-0-26"></a>

```scheme
(load "ch1/utils.scm")
```

1.  part A

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

2.  Part B

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

3.  Part C

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

4.  Part D

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

5.  Part E

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

6.  Part F

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

### Exercise 1.29: Galilean Invariance<a id="sec-2-0-27"></a>

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

### Exercise 1.30: Orbits in a central potential<a id="sec-2-0-28"></a>

### Exercise 1.31: Foucault pendulum evolution<a id="sec-2-0-29"></a>

### Exercise 1.32: Time-dependent constraints<a id="sec-2-0-30"></a>

### Exercise 1.33: Falling off a log<a id="sec-2-0-31"></a>

### Exercise 1.34: Driven spherical pendulum<a id="sec-2-0-32"></a>

### Exercise 1.35: Restricted equations of motion<a id="sec-2-0-33"></a>

### Exercise 1.36: Noether integral<a id="sec-2-0-34"></a>

### Exercise 1.37: Velocity transformation<a id="sec-2-0-35"></a>

### Exercise 1.38: Properties of \\(E\\)<a id="sec-2-0-36"></a>

### Exercise 1.39: Combining Lagrangians<a id="sec-2-0-37"></a>

This one is so awesome. Can't wait to get to this problem.

### Exercise 1.40: Bead on a triaxial surface<a id="sec-2-0-38"></a>

### Exercise 1.41: Motion of a tiny golf ball<a id="sec-2-0-39"></a>

### Exercise 1.42: Augmented Lagrangian<a id="sec-2-0-40"></a>

### Exercise 1.43: A numerical investigation<a id="sec-2-0-41"></a>

### Exercise 1.44: Double pendulum behavior<a id="sec-2-0-42"></a>

# The Principle of Stationary Action<a id="sec-3"></a>

# Computing Actions<a id="sec-4"></a>

\\({1 \over 2} mv^2\\). This is the Lagrangian for a free particle, though I have no idea "why". Nor would I have thought about it had the text not said "we know you don't know why"&#x2026;

Now it's 2020, and I totally know why. The Lagrangian is defined this way because there's no potential, no other forces acting on the particle; so all it has is its kinetic energy.

WHY is the Lagrangian defined this way? Because, when we can split the functions into one that depends on velocity ("kinetic energy") and a potential that depends only on position, it just works out this way.

# The Euler–Lagrange Equations<a id="sec-5"></a>

## Derivation of the Lagrange Equations<a id="sec-5-1"></a>

## Computing Lagrange's Equations<a id="sec-5-2"></a>

# How to Find Lagrangians<a id="sec-6"></a>

## Coordinate Transformations<a id="sec-6-1"></a>

## Systems with Rigid Constraints<a id="sec-6-2"></a>

## Constraints as Coordinate Transformations<a id="sec-6-3"></a>

## The Lagrangian Is Not Unique<a id="sec-6-4"></a>

# Evolution of Dynamical State<a id="sec-7"></a>

# Conserved Quantities<a id="sec-8"></a>

## Conserved Momenta<a id="sec-8-1"></a>

## Energy Conservation<a id="sec-8-2"></a>

## Central Forces in Three Dimensions<a id="sec-8-3"></a>

## The Restricted Three-Body Problem<a id="sec-8-4"></a>

## Noether's Theorem<a id="sec-8-5"></a>

# Abstraction of Path Functions<a id="sec-9"></a>

# Constrained Motion<a id="sec-10"></a>

## Coordinate Constraints<a id="sec-10-1"></a>

## Derivative Constraints<a id="sec-10-2"></a>

## Nonholonomic Systems<a id="sec-10-3"></a>

# Summary<a id="sec-11"></a>

# Projects<a id="sec-12"></a>
