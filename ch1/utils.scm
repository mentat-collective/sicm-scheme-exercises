;; Utilities I want to share between exercises.

;; Printing

(load "ch1/exdisplay.scm")

;; string utilities! can't believe I have to write this.

(define (replace-all haystack needle replacement)
  (let ((haystack (string->list haystack))
        (replacement (reverse
                      (string->list replacement)))
        (needle-len (string-length needle)))
    (let loop ((haystack haystack) (acc '()))
      (cond ((null? haystack)
             (list->string (reverse acc)))

            ((string-prefix? needle (list->string haystack))
             (loop (list-tail haystack needle-len)
                   (append replacement acc)))

            (else
             (loop (cdr haystack) (cons (car haystack) acc)))))))

;; Generates a properly formatted string of LaTeX.
(define (->tex* expr)
  (let* ((tex-string (expression->tex-string
                      ((prepare-for-printing expr simplify))))
         (len (string-length tex-string)))
    (substring tex-string 10 (- len 3))))

;; Prints the string as a LaTeX code block.
(define (->write-tex tex-string)
  (write-string
   (string-append "\\[ " tex-string " \\]")))

;; Prints the TeX representation of the supplied expression to the screen.
(define (->tex expr)
  (->write-tex (->tex* expr)))

;; Prints an equation code block containing the expression as LaTeX.
(define (->tex-equation* expr #!optional label)
  (string-append
   "\\begin{equation}\n"
   (->tex* expr)
   (if (default-object? label)
       ""
       (string-append "\n\\label{" label "}"))
   "\n\\end{equation}"))

(define (->tex-equation expr #!optional label)
  (write-string (->tex-equation* expr label)))

;; Lagrangian helpers

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

;; This function lets us change a function that only transforms coordinates into
;; a function that can transform an entire path of (up t, q, Dq).
;;
;; I THINK this overrides an internal definition, so we may want to delete it.
;; We'll see.
(define ((F->C-limited F) local)
  (up (time local)
      (F local)
      (+ (((partial 0) F) local)
         (* (((partial 1) F) local)
            (velocity local)))))

;; Total time derivatives!
;;
;; These came from exercise 1.28.

;; Let's make a function that can make a G, so we can confirm that we've got the
;; right equation at all.
(define (G G0 G1)
  (+ G0 (* G1 Qdot)))

;; And another here, to show off the properties G is supposed to have, so we can
;; compare.
(define (G-properties G0 G1 q)
  (let ((full-G (G G0 G1))
        (path (lambda (G)
                (let ((f (compose G (Gamma q))))
                  (f 't)))))
    (up (path full-G)
        (path ((partial 0) G1))
        (path ((partial 1) G0))
        (path ((partial 1) G1)))))

;; Then, some functions to generate a G from the book, given an F.
(define (check-f1 F q)
  ((D (compose F (Gamma q))) 't))

;; And an alternative way to calculate the same thing.
(define (check-f2 F q)
  (let ((DtF (+ ((partial 0) F)
                (* ((partial 1) F) Qdot))))
    ((compose DtF (Gamma q)) 't)))

(define (check-f F q)
  (se (up (check-f1 F q)
          (check-f2 F q))))
