(load "lisp-unit")

(defvar *Arrows* nil)

(defun reset-arrows ()
    (setq *Arrows* nil))

(defun is-sequence? (f) (typep f 'cons))

(lisp-unit:define-test is-sequence?-test
  (lisp-unit:assert-equal nil (is-sequence? 5))
  (lisp-unit:assert-equal nil (is-sequence? '()))
  (lisp-unit:assert-equal nil (is-sequence? ()))
  (lisp-unit:assert-equal t (is-sequence? '(1)))
  (lisp-unit:assert-equal t (is-sequence? '(1 2)))
  (lisp-unit:assert-equal t (is-sequence? (list 1 3 4 5 6))))

(lisp-unit:run-tests :all)
(lisp-unit:use-debugger t)


(defun get-common-head-inner (acc f g)
  (cond ((not f) (list acc f g))
	((not g) (list acc f g))
	((= (car f) (car g)) (get-common-head-inner (push (car f) acc) (cdr f) (cdr g)))
	(t (list acc f g))))

(defun get-common-head (f g)
  (get-common-head-inner nil f g))

(defun get-common-tail (f g)
  (get-common-head-inner nil (reverse f) (reverse g)))

(defun get-common-head-and-tail (f g)
  (let ((x (get-common-head f g)))
    (let ((y (get-common-tail (second x) (third x))))
      (list (reverse (first x)) (list (reverse (second y)) (reverse (third y))) (first y)))))

(get-common-head (list 0 3 4 5 6) (list 0 5 6))
(get-common-tail (list 0 3 4 5 6) (list 0 5 6))
(get-common-head-and-tail (list 0 3 4 5 6) (list 0 5 6))

(defun is-arrow? (f) (member f *Arrows* :test #'equal))

(defun push-to-arrows (f)
  (cond ((is-arrow? f) ())
	(t (setq *Arrows* (push f *Arrows*)))))

(defun add-arrow (f)
  (cond ((not (is-sequence? f)) (push-to-arrows f) )
	(t (reduce
	    (lambda (x y)
	      (progn (push-to-arrows (list x y))
		     (add-arrow x)
		     (add-arrow y)
		     y)) f))))

(defun is-generated-arrow? (x y)
  (let ((difference (second (get-common-head-and-tail x y))))
    (is-arrow? difference)))

(defun check-sequence (f)
  (cond ((not (typep f 'cons)) (is-arrow? f))
	(t (reduce
	    (lambda (x y)
	      (cond ((not x) nil)
		    ((is-generated-arrow? x y) y)
		    (t nil)) ) f ))))

(defun prove-sequence (f)
  (cond ((not (check-sequence f)) nil)
	(t (push-to-arrows (list (first f) (first (last f)))))))

(get-common-head '(1 2 3 4 5) '(1 2 3 6 4 5))
(get-common-tail '(1 2 3 4 5) '(1 2 3 6 4 5))
(get-common-head-and-tail '(1 2 9 8 4 5) '(1 2 12 12 6 4 5))
(get-common-head-and-tail '(1 2) '(1 2))
(get-common-head '(1 2) '(1 2))

(reset-arrows)
(add-arrow (list (list 1 2) (list 3 4)))
(add-arrow (list (list 3 4) '()))
*Arrows*
(is-generated-arrow? (list 0 3 4 5 6) (list 0 5 6))
(check-sequence (list (list 0 1 2 5 6) (list 0 3 4 5 6) (list 0 5 6)))
(prove-sequence (list (list 0 1 2 5 6) (list 0 3 4 5 6) (list 0 5 6)))
(is-arrow? (list (list 0 1 2 5 6) (list 0 5 6)))
