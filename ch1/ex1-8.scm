;; Exercise 1.8: Implementing Delta
;; :PROPERTIES:
;; :header-args+: :tangle ch1/ex1-8.scm :comments org
;; :END:

;; This FEELS weird... but we want something that is... well, let's see.


(define (((delta eta) f) q)
  (let (g (lambda (eps)
            (f (q + (* eps eta)))))
    ((D g) 0)))



;; Why does this work? WELL... we need a way to force the limit in.

;; this is a PATH function, remember. This takes a path function, then passes it
;; into $\Gamma$, and composes THAT with F. F is a function from the local tuple to
;; some output variable. You can imagine it as the Langrangian, for example.

;; The local tuple type defined here can take any number of path components.


(define (f q)
  (let* ((Local (Up Real (UP* Real) (UP* Real)))
         (F (literal-function 'F (-> Local Real))))
    (compose F (Gamma q))))



;; This is a path function that returns a 2d path; we can use this as an example.


(define q (literal-function 'q (-> Real (Up Real Real))))
