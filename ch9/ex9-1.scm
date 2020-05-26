(load "ch1/utils.scm")

(define (F x y)
  (* (square x)
     (cube y)))

(define (G x y)
  (up (F x y) y))

(define (H x y)
  (F (F x y) y))

(let ((f (down ((partial 0) F) ((partial 1) F))))
  (->tex-equation
   (f 'x 'y)))

(->tex-equation
 ((D F) 'x 'y))

(let ((f (down ((partial 0) F) ((partial 1) F))))
  (->tex-equation
   (- ((D F) 'x 'y)
      (f 'x 'y))))

(->tex-equation
 ((D H) 'x 'y))

(->tex-equation
 ((D G) 'x 'y))

(->tex-equation
 (up ((D F) 'a 'b)
     ((D G) 3 5)
     ((D H) (* 3 (square 'a)) (* 5 (cube 'b)))))
