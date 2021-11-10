## Exercise 1.28: Total Time Derivatives<a id="sec-1-29"></a>

```clojurescript
(nextjournal.env/load! :sicmutils)
```

### part A<a id="sec-1-29-1"></a>

TODO: Port the check-f stuff from utils.scm.

nice, easy to guess.

```clojurescript
(defn FA [m]
  (fn [[_ x]]
    (* m x)))
```

Show the function of t, and confirm that both methods are equivalent.

```clojurescript
(check-f (FA 'm)
         (literal-function 'x))
```

### Part B<a id="sec-1-29-2"></a>

NOT a total time derivative.

Define G directly:

```clojurescript
(defn GB [m]
  (fn [[t _ v_x]]
    (let [GB0 0
          GB1 (* m (cos t))]
      (+ GB0 (* GB1 v_x)))))
```

And show the full G, for fun:

```clojurescript
(let [f (compose (GB 'm) (Gamma (literal-function 'x)))]
  (->tex-equation
   (f 't)))
```

It's easier to confirm that this is not a total time derivative by checking the partials.

```clojurescript
(defn GB-properties [m]
  (let [GB0 (fn [local] 0)
        GB1 (fn [[t]]
              (* m (cos t)))]
    (G-properties
     GB0 GB1 (literal-function 'x))))
```

It's clear here that the second and third tuple entries aren't equal, so we don't have a total time derivative.

```clojurescript
(GB-properties 'm)
```

### Part C<a id="sec-1-29-3"></a>

no problem, we've got a total time derivative on our hands.

```clojurescript
(defn FC [[t x]]
  (* x (cos t)))

(check-f FC (literal-function 'x))

(def GC-properties
  (let [GC0 (fn [[t q]]
              (* -1 q (sin t)))
        GC1 (fn [[t]]
              (cos t))]
    (G-properties
     GC0 GC1 (literal-function 'x))))

GC-properties
```

Boom, the second and third entries are equal, as we'd expect.

### Part D<a id="sec-1-29-4"></a>

This is NOT a total time derivative; you can tell by taking the partials of each side, G0 and G1, as we'll see here.

```clojurescript
(def GD-properties
  (let [GD0 (fn [[t q]]
              (* q (sin t)))
        GD1 (fn [[t]]
              (cos t))]
    (G-properties
     GD0 GD1 (literal-function 'x))))

GD-properties
```

The partials for each side don't match.

### Part E<a id="sec-1-29-5"></a>

This is strange to me, because I thought that this thing had to produce a tuple.

OH, but the secret is that Qdot is also a tuple, so you contract them together.

Here's the function F that we can use to derive it:

```clojurescript
(defn FE [[t [x y]]]
  (* (+ (square x) (square y))
     (cos t)))
```

Boom, total time derivative!

```clojurescript
(check-f FE (up (literal-function 'x)
                (literal-function 'y)))
```

And let's show that we pass the tests by decomposing this into G0 and G1:

```clojurescript
(def GE-properties
  (let [;; any piece of the function without a velocity multiplied.
        GE0 (fn [[t [x y]]]
              (* -1
                 (+ (square x) (square y))
                 (sin t)))

        ;; The pieces multiplied by velocities, split into a down tuple of
        ;; components, one for each of the coordinate components.
        GE1 (fn [[t [x y]]]
              (down
               (* 2 x (cos t))
               (* 2 y (cos t))))]
    (G-properties
     GE0 GE1 (up (literal-function 'x)
                 (literal-function 'y)))))
```

BOOM!

We've recovered F; the partials are equal, and the final matrix is symmetric.

```clojurescript
GE-properties
```

### Part F<a id="sec-1-29-6"></a>

This one is interesting, since the second partial is a tuple. This is not so obvious to me, so first let's check the properties:

```clojurescript
(def GF-properties
  (let [;; any piece of the function without a velocity multiplied.
        GF0 (fn [[t [x y]]]
              (* -1
                 (+ (square x) (square y))
                 (sin t)))

        ;; The pieces multiplied by velocities, split into a down tuple of
        ;; components, one for each of the coordinate components.
        G (fn [[t [x y]]]
            (down
             (+ (cube y) (* 2 x (cos t)))
             (+ x (* 2 y (cos t)))))]
    (G-properties
     GF0 GF1 (up (literal-function 'x)
                 (literal-function 'y)))))
```

AND it looks like we DO have a total time derivative, maybe. We certainly pass the first test here, since the second and third tuple entries are equal.

BUT we fail the second test; the hessian that we get from ((partial 1) G1) is not symmetric.

```clojurescript
GF-properties
```

## Exercise 1.29: Galilean Invariance<a id="sec-1-30"></a>

I'll do this for a single particle, since it's annoying to get the sum going for many; and the Lagrangian is additive, so no problem.

```clojurescript
(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* (/ 1 2) mass (square v)))))

(defn uniform-translate-shift->rect
  [[t [xprime delta_x delta_v]]]
  (+ xprime delta_x (* t delta_v)))

(defn L-translate-shift [m]
  (compose (L-free-particle m)
           (F->C uniform-translate-shift->rect)))
```

First, confirm that if we have a constant, we get what we expected from paper.

```clojurescript
(let [q (up (literal-function 'xprime)
            (fn [_] 'Delta_x)
            (fn [_] 'Delta_v))
      f (compose (L-translate-shift 'm) (Gamma q))]
  (f 't))
```

We can change this a little to see the extra terms; substract off the free particle Lagrangian, to see the extra stuff.

```clojurescript
(let [q (up (literal-function 'xprime)
            (fn [_] 'Delta_x)
            (fn [_] 'Delta_v))
      L (- (L-translate-shift 'm)
           (L-free-particle 'm))
      f (compose L (Gamma q))]
  (f 't))
```

Here's the gnarly version with both entries as actual functions. Can this be a total time derivative? It CANNOT be, because we have a \\((D \Delta\_v(t))^2\\) term in there, and we know that total time derivatives have to be linear in the velocities. The function \\(F\\) would have had to have a velocity in it, which is not allowed.

```clojurescript
(let [q (up (literal-function 'xprime)
            (literal-function 'Delta_x)
            (literal-function 'Delta_v))
      L (- (L-translate-shift 'm)
           (L-free-particle 'm))
      f (compose L (Gamma q))]
  (f 't))
```

Let's simplify by making the \\(\Delta\_v\\) constant and see if there's anything so obvious about \\(\Delta\_x\\).

We know that we have a total derivative when \\(\Delta\_x\\) is constant, and we know that total time derivatives are linear, so let's substract off the total time derivative and see what happens:

```clojurescript
(let [q (fn [dx]
          (up (literal-function 'xprime)
              dx
              (fn [_] 'Delta_v)))
      L (- (L-translate-shift 'm)
           (L-free-particle 'm))
      f (fn [dx]
          (compose L (Gamma (q dx))))]
  ((- (f (literal-function 'Delta_x))
      (f (fn [_] 'Delta_x)))
   't))
```

Take a look. there is a quadratic velocity term in here! We have \\(D \Delta\_x(t) D x'(t)\\). This is not allowed in a total time derivative.

SO, only if the shift and uniform translation are constant do we not affect the Lagrangian value.

# Our Notation<a id="sec-8"></a>

Notation Appendix. This is all about getting cozy with scheme, and with the various idiosyncracies of the tuple and functional notation.

## Exercise 9.1 Chain Rule<a id="sec-8-1"></a>

You're supposed to do these by hand, so I'll do that in the textbook. But here, let's redo them on the machine.

#### Compute \\(\partial\_0 F(x, y)\\) and \\(\partial\_1 F(x, y)\\)

First, let's define the functions we need.

```clojurescript
(defn F [x y]
  (* (square x)
     (cube y)))

(defn G [x y]
  (up (F x y) y))

(defn H [x y]
  (F (F x y) y))
```

You can do this with explicit partials:

```clojurescript
(let [f (down ((partial 0) F) ((partial 1) F))]
  (f 'x 'y))
```

Or with the \\(D\\) symbol:

```clojurescript
((D F) 'x 'y)
```

Or, we could show that they're equivalent this way:

```clojurescript
(let [f (down ((partial 0) F) ((partial 1) F))]
  (- ((D F) 'x 'y)
     (f 'x 'y)))
```

#### Compute \\(\partial\_0 F(F(x, y), y)\\) and \\(\partial\_1 F(F(x, y), y)\\)

\\(H\\) is already that composition, so:

```clojurescript
((D H) 'x 'y)
```

#### Compute \\(\partial\_0 G(x, y)\\) and \\(\partial\_1 G(x, y)\\)

```clojurescript
((D G) 'x 'y)
```

#### Compute \\(DF(a, b)\\), \\(DG(3, 5)\\) and \\(DH(3a^2, 5b^3)\\)

```clojurescript
(up ((D F) 'a 'b)
    ((D G) 3 5)
    ((D H) (* 3 (square 'a)) (* 5 (cube 'b))))
```

## Exercise 9.2: Computing Derivatives<a id="sec-8-2"></a>

A further exercise is to try defining the functions so that they use explicit tuples, so you can compose them:

```clojurescript
(defn F* [[x y]]
  (* (square x) (cube y)))

(defn G* [[_ y :as v]]
  (up (F* v) y))

(def H* (compose F* G*))
```

to be really pro, I'd make a function that takes these as arguments and prints a nice formatted exercise output. Let's do the final exercise, for fun:

```clojurescript
(up ((D F*) (up 'a 'b))
    ((D G*) (up 3 5))
    ((D H*) (up (* 3 (square 'a)) (* 5 (cube 'b)))))
```
