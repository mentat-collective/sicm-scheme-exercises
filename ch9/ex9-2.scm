;; Exercise 9.2: Computing Derivatives
;;      :PROPERTIES:
;;      :header-args+: :tangle ch9/ex9-2.scm :comments org
;;      :END:


(load "ch1/utils.scm")



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



;; #+RESULTS:
;; : #| F* |#
;; :
;; : #| G* |#
;; :
;; : #| H* |#

;; to be really pro, I'd make a function that takes these as arguments and prints a
;; nice formatted exercise output. Let's do the final exercise, for fun:


(->tex-equation
 (up ((D F*) (up 'a 'b))
     ((D G*) (up 3 5))
     ((D H*) (up (* 3 (square 'a)) (* 5 (cube 'b))))))
