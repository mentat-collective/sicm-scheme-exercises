;; Notation Appendix. This is all about getting cozy with scheme.

(define (F x y)
  (* (square x) (cube y)))

(define (G x y)
  (up (F x y) y))

(define (H x y)
  (F (F x y) y))


;; Exercise 9.1
;;
;; you're supposed to do these by hand, so I'll do that in the textbook. But
;; here, let's redo them on the machine, since this is the first part of
;; exercise 9.1.
;;
;; a. compute the d0 and d1.
;;
;; You can do this with explicit partials:


(let ((f (down ((partial 0) F) ((partial 1) F))))
  (f 'x 'y))

;; or with the D symbol:
((D F) 'x 'y)

;; compute d0F(F(x, y), y) and d1F(F(x, y), y)
;;
;; H is already that composition, so:
((D H) 'x 'y)

;; c. same for G
((D G) 'x 'y)

;; d.
((D F) 'a 'b)
((D G) 3 5)
((D H) (* 3 (square 'a)) (* 5 (cube 'b)))


;; 9.2: computing derivatives
;;
;; A further exercise is to try defining the functions so that they use explicit
;; tuples, so you can compose them:


(define (F* v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (* (square x) (cube y))))

(define (G* v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (up (F* v) y)))

(define H* (compose F* G*))

;; to be really pro, I'd make a function that takes these as arguments and
;; prints a nice formatted exercise output.
