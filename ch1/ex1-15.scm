;; Exercise 1.15: Equivalence
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-15.scm :comments org
;; :END:

;; NOTE - I have a strong suspicion here that we can show that what is actually
;; going on is that we end up with a total time derivative that we can ignore. The
;; final terms at the end that cancel... why is it that they work out the way they
;; do? It would be nice to try and use the time-derivative test machinery we built
;; to take a look there.

;; This one was a serious doozy. I think that this exercise can be a great way to
;; show off the computer algebra system, and show off the steps that I go through
;; to make a proof.

;; But I also want to pull back and stare at the formula. What is going on? What is
;; the meaning of the extra terms? If we can say, for example, that they're a total
;; time derivative, looking at the future, that would be great. There has to be a
;; reason that the Lagrangian doesn't change.

;; The same thing happens when you look at a new Lagrangian and see a "fictitious
;; force term" for, say, centrifugal force. There is something going on here.


;; Checking that composition distributes over multiplication...


(define f (literal-function 'f))
(define g (literal-function 'g))
(define h (literal-function 'h))


;; looks good! These are the same expression.


((compose (* f g) h) 't)
((* (compose f h) (compose g h)) 't)


;; This is the general form of a path transformation; big surprise, this is very
;; close to the code on page 46. I'm going to keep my version, since I don't want
;; to get too confused, here.


(define ((F->C F) local)
  (let ((t (time local))
        (x (coordinate local))
        (v (velocity local)))
    (up t
        (F t x)
        (+ (((partial 0) F) t x)
           (* (((partial 1) F) t x)
              v)))))


;; Here's a literal function we can play with.


(define F*
  (literal-function 'F (-> (X Real Real) Real)))


;; Okay, boom, this is the literal function.


(define q-prime
  (literal-function 'q-prime))


;; This is the manual generation of q from q-prime.


(define ((to-q F) qp)
  (lambda (t) (F t (qp t))))


;; We can check that these are now equal. This uses C to get us to q


((compose (F->C F*) (Gamma q-prime)) 't)


;; And this does it by passing in q manually.


((Gamma ((to-q F*) q-prime)) 't)


;; I can convert the proof to code, no problem, by showing that these sides are equal.

;; YES!! the final step of my proof was the note that these are equal. THIS IS
;; HUGE!!!


((compose (lambda (x) (ref x 1)) ((partial 1) (F->C F*)) (Gamma q-prime)) 't)
((compose (lambda (x) (ref x 2)) ((partial 2) (F->C F*)) (Gamma q-prime)) 't)


;; Just for fun, note that this successfully pushes things inside gamma.


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



;; trying again. get a function:


(define q
  ;; time to x y.
  (literal-function 'q (-> Real (UP Real Real))))

(define (C local)
  (up (time local)
     (square (coordinate local))
     (velocity local)))

((compose C (Gamma q)) 't)
