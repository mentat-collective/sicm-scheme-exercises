(load "ch1/utils.scm")

(define (F* v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (* (square x) (cube y))))

(define (G* v)
  (let ((x (ref v 0))
        (y (ref v 1)))
    (up (F* v) y)))

(define H* (compose F* G*))

(->tex-equation
 (up ((D F*) (up 'a 'b))
     ((D G*) (up 3 5))
     ((D H*) (up (* 3 (square 'a)) (* 5 (cube 'b))))))
