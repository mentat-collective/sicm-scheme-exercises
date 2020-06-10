;; Orbital Motion
;; :PROPERTIES:
;; :header-args+: :tangle ch1/orbital-motion.scm :comments org
;; :END:

;; Page 31.


(load "ch1/utils.scm")
(define ((L-orbital mass mu) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (+ (* 1/2 mass (square qdot))
       (/ mu (sqrt (square q))))))

(define q2
  (up (literal-function 'xi)
      (literal-function 'eta)))


;; To test:


((compose ((partial 1) (L-orbital 'm 'mu)) (Gamma q2)) 't)
