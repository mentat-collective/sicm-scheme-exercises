;; Exercise 1.28: Total Time Derivatives
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-28.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; part A

;; nice, easy to guess.


(define ((FA m) local)
  (let ((x (coordinate local)))
    (* m x)))


;; Show the function of t, and confirm that both methods are equivalent.


(check-f (FA 'm)
         (literal-function 'x))
;; Part B

;; NOT a total time derivative.

;; Define G directly:


(define ((GB m) local)
  (let* ((t (time local))
         (v_x (velocity local))
         (GB0 0)
         (GB1 (* m (cos t))))
    (+ GB0 (* GB1 v_x))))


;; And show the full G, for fun:


(let ((f (compose (GB 'm) (Gamma (literal-function 'x)))))
  (se (f 't)))



;; It's easier to confirm that this is not a total time derivative by checking the
;; partials.


(define (GB-properties m)
  (let ((GB0 (lambda (local) 0))
        (GB1 (lambda (local)
               (* m (cos (time local))))))
    (G-properties GB0 GB1 (literal-function 'x))))


;; It's clear here that the second and third tuple entries aren't equal, so we
;; don't have a total time derivative.


(se (GB-properties 'm))
;; Part C

;; no problem, we've got a total time derivative on our hands.


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


;; Boom, the second and third entries are equal, as we'd expect.


(se GC-properties)
;; Part D

;; This is NOT a total time derivative; you can tell by taking the partials
;; of each side, G0 and G1, as we'll see here.


(define GD-properties
  (let ((GD0 (lambda (local)
               (* (coordinate local)
                  (sin (time local)))))
        (GD1 (lambda (local)
               (cos (time local)))))
    (G-properties GD0 GD1 (literal-function 'x))))


;; The partials for each side don't match.


(se GD-properties)
;; Part E

;; This is strange to me, because I thought that this thing had to produce a tuple.

;; OH, but the secret is that Qdot is also a tuple, so you contract them together.

;; Here's the function F that we can use to derive it:


(define (FE local)
  (let* ((t (time local))
         (q (coordinate local))
         (x (ref q 0))
         (y (ref q 1)))
    (* (+ (square x) (square y))
       (cos t))))


;; Boom, total time derivative!


(check-f FE (up (literal-function 'x)
                (literal-function 'y)))


;; And let's show that we pass the tests by decomposing this into G0 and G1:


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


;; BOOM!

;; We've recovered F; the partials are equal, and the final matrix is symmetric.


(se GE-properties)
;; Part F

;; This one is interesting, since the second partial is a tuple. This is not so
;; obvious to me, so first let's check the properties:


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


;; AND it looks like we DO have a total time derivative, maybe. We certainly pass
;; the first test here, since the second and third tuple entries are equal.

;; BUT we fail the second test; the hessian that we get from ((partial 1) G1) is
;; not symmetric.


(se GF-properties)
