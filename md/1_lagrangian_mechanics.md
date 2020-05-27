  - [Configuration Spaces](#sec-1)
      - [Exercise 1.1, p5 and 1.2, p8](#sec-1-0-1)
  - [Generalized Coordinates](#sec-2)
      - [Exercise 1.3](#sec-2-0-1)
      - [Exercise 1.4](#sec-2-0-2)
      - [Exercise 1.5](#sec-2-0-3)
      - [Exercise 1.6](#sec-2-0-4)
      - [Exercise 1.7](#sec-2-0-5)
      - [Exercise 1.8](#sec-2-0-6)
      - [Exercise 1.9](#sec-2-0-7)
      - [Exercise 1.10](#sec-2-0-8)
      - [Exercise 1.11](#sec-2-0-9)
      - [Exercise 1.12](#sec-2-0-10)
      - [Exercise 1.13](#sec-2-0-11)
      - [Exercise 1.14](#sec-2-0-12)
      - [Exercise 1.15](#sec-2-0-13)
      - [Exercise 1.16](#sec-2-0-14)
      - [Exercise 1.17](#sec-2-0-15)
      - [Exercise 1.18](#sec-2-0-16)
      - [Exercise 1.19](#sec-2-0-17)
      - [Exercise 1.20](#sec-2-0-18)
      - [Exercise 1.21](#sec-2-0-19)
      - [Exercise 1.22](#sec-2-0-20)
      - [Exercise 1.23](#sec-2-0-21)
      - [Exercise 1.24](#sec-2-0-22)
      - [Exercise 1.25](#sec-2-0-23)
      - [Exercise 1.26](#sec-2-0-24)
      - [Exercise 1.27](#sec-2-0-25)
      - [Exercise 1.28](#sec-2-0-26)
      - [Exercise 1.29](#sec-2-0-27)
      - [Exercise 1.30](#sec-2-0-28)
      - [Exercise 1.31](#sec-2-0-29)
      - [Exercise 1.32](#sec-2-0-30)
      - [Exercise 1.33](#sec-2-0-31)
      - [Exercise 1.34](#sec-2-0-32)
      - [Exercise 1.35](#sec-2-0-33)
      - [Exercise 1.36](#sec-2-0-34)
      - [Exercise 1.37](#sec-2-0-35)
      - [Exercise 1.38](#sec-2-0-36)
      - [Exercise 1.39](#sec-2-0-37)
      - [Exercise 1.40](#sec-2-0-38)
      - [Exercise 1.41](#sec-2-0-39)
      - [Exercise 1.42](#sec-2-0-40)
      - [Exercise 1.43](#sec-2-0-41)
      - [Exercise 1.44](#sec-2-0-42)
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

### Exercise 1.1, p5 and 1.2, p8<a id="sec-1-0-1"></a>

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

### Exercise 1.3<a id="sec-2-0-1"></a>

Fermat Optics. It's in the foldede section in the blue notebook. Do it again!

### Exercise 1.4<a id="sec-2-0-2"></a>

### Exercise 1.5<a id="sec-2-0-3"></a>

### Exercise 1.6<a id="sec-2-0-4"></a>

### Exercise 1.7<a id="sec-2-0-5"></a>

### Exercise 1.8<a id="sec-2-0-6"></a>

### Exercise 1.9<a id="sec-2-0-7"></a>

### Exercise 1.10<a id="sec-2-0-8"></a>

### Exercise 1.11<a id="sec-2-0-9"></a>

### Exercise 1.12<a id="sec-2-0-10"></a>

### Exercise 1.13<a id="sec-2-0-11"></a>

### Exercise 1.14<a id="sec-2-0-12"></a>

### Exercise 1.15<a id="sec-2-0-13"></a>

### Exercise 1.16<a id="sec-2-0-14"></a>

### Exercise 1.17<a id="sec-2-0-15"></a>

### Exercise 1.18<a id="sec-2-0-16"></a>

### Exercise 1.19<a id="sec-2-0-17"></a>

### Exercise 1.20<a id="sec-2-0-18"></a>

### Exercise 1.21<a id="sec-2-0-19"></a>

### Exercise 1.22<a id="sec-2-0-20"></a>

### Exercise 1.23<a id="sec-2-0-21"></a>

### Exercise 1.24<a id="sec-2-0-22"></a>

### Exercise 1.25<a id="sec-2-0-23"></a>

### Exercise 1.26<a id="sec-2-0-24"></a>

### Exercise 1.27<a id="sec-2-0-25"></a>

### Exercise 1.28<a id="sec-2-0-26"></a>

### Exercise 1.29<a id="sec-2-0-27"></a>

### Exercise 1.30<a id="sec-2-0-28"></a>

### Exercise 1.31<a id="sec-2-0-29"></a>

### Exercise 1.32<a id="sec-2-0-30"></a>

### Exercise 1.33<a id="sec-2-0-31"></a>

### Exercise 1.34<a id="sec-2-0-32"></a>

### Exercise 1.35<a id="sec-2-0-33"></a>

### Exercise 1.36<a id="sec-2-0-34"></a>

### Exercise 1.37<a id="sec-2-0-35"></a>

### Exercise 1.38<a id="sec-2-0-36"></a>

### Exercise 1.39<a id="sec-2-0-37"></a>

### Exercise 1.40<a id="sec-2-0-38"></a>

### Exercise 1.41<a id="sec-2-0-39"></a>

### Exercise 1.42<a id="sec-2-0-40"></a>

### Exercise 1.43<a id="sec-2-0-41"></a>

### Exercise 1.44<a id="sec-2-0-42"></a>

# The Principle of Stationary Action<a id="sec-3"></a>

# Computing Actions<a id="sec-4"></a>

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
