;; Exercise 1.13: Higher-derivative Lagrangians (code)
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-13.scm :comments org
;; :END:


(load "ch1/utils.scm")
;; Part A: Acceleration-dependent Lagrangian Implementation

;; Here we go:


(define ((Lagrange-equations3 L) q)
  (let ((state-path (Gamma q 4)))
    (+ ((square D) (compose ((partial 3) L) state-path))
       (- (D (compose ((partial 2) L) state-path)))
       (compose ((partial 1) L) state-path))))
;; Part B: Applying HO-Lagrangians

;; Lagrangian from the problem:


(define ((L-1-13 m k) local)
  (let ((x (coordinate local))
        (a (acceleration local)))
    (- (* -1/2 m x a)
       (* 1/2 k (square x)))))


;; #+RESULTS:
;; : #| L-1-13 |#

;; Testing, with a factor of $-1$ to make it look nice:


(->tex-equation
 (- (((Lagrange-equations3 (L-1-13 'm 'k))
      (literal-function 'x)) 't)))
;; Part C: Generalized Lagrange Equations

;; The more general version:


;; Returns n copies of elems appended to each other.
(define (cycle n elems)
  (apply append (make-list n elems)))

;; Returns n total elements generated from an infinite cycle of elems.
(define (alternating n elems)
  (let* ((l (length elems))
         (times (quotient (+ n (-1+ l)) l)))
    (list-head (cycle times elems) n)))

(define ((Lagrange-equations* L n) q)
  (let ((state-path (Gamma q (1+ n))))
    (define (term i)
      ((expt D i)
       (compose ((partial (1+ i)) L) state-path)))
    (let ((terms (map term (iota n))))
      (fold-left (lambda (acc pair)
                   (+ acc (apply * pair)))
                 0
                 (zip (alternating n '(1 -1))
                      (reverse terms))))))


;; #+RESULTS:
;; : #| cycle |#
;; :
;; : #| alternating |#
;; :
;; : #| Lagrange-equations* |#

;; Check that we get the same result:


(->tex-equation
 (- (((Lagrange-equations* (L-1-13 'm 'k) 3)
      (literal-function 'x)) 't)))
