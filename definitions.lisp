(defvar *Arrows* nil)

(defun reset-arrows ()
    (setq *Arrows* nil))

(defun is-arrow? (f)
  (member f *Arrows* :test #'equal))

(defun push-to-arrows (f)
  (cond ((is-arrow? f) ())
	(t (setq *Arrows* (push f *Arrows*)))))

(defun add-arrow (f)
  (cond ((not (typep f 'cons)) (push-to-arrows f) )
	(t (reduce
	    (lambda (x y)
	      (progn (push-to-arrows (list x y))
		     (add-arrow x)
		     (add-arrow y)
		     y)) f))))

(defun check-arrow (f)
  (cond ((not (typep f 'cons)) (is-arrow? f))
	(t (reduce
	    (lambda (x y)
	      (cond ((not x) nil)
		    ((is-arrow? (list x y)) y)
		    (t nil)) ) f ))))

(reset-arrows)
(add-arrow 0)
(add-arrow (list 0 1 2))
(add-arrow (list (list 0 2 1) (list 0 3 1) (list 0 4 1)))
(check-arrow (list 0 4 1))
(list *Arrows*)

(defvar *Equalities* nil)
(defvar *Composable-Arrows* nil)

(defun reset-category ()
  (progn (setq *Equalities* nil)
	 (setq *Composable-Arrows* nil)))

(defun is-arrow (f)
  (member (list f) *Composable-Arrows* :test #'equal))

(defun add-arrow (f)
  (cond ((is-arrow f) ())
	(t (progn (setq *Equalities* (push (list f) *Equalities*))
		  (setq *Composable-Arrows* (push (list f) *Composable-Arrows*))))))

(defun add-arrows (arrows)
  (dolist (arrow arrows) (add-arrow arrow)))

(defun equivalence-class (f)
  (find-if (lambda (x) (member f x :test #'equal) ) *Equalities*))

(defun are-equal (f g)
  (member f (equivalence-class g) :test #'equal))

(defun set-equal (f g)
  (let ((x (concatenate 'list (equivalence-class f) (equivalence-class g))))
    (progn (setq *Equalities* (remove (equivalence-class f) *Equalities*))
	   (setq *Equalities* (remove (equivalence-class g) *Equalities*))
	   (setq *Equalities* (push x *Equalities*)))))

(defun is-composable (f)
  (cond ((member f *Composable-Arrows* :test #'equal) t)
	(t nil)))


(defun check-composite (f)
  (reduce (lambda (x y) (cond ((not x) nil)
			      ((is-composable (list x y)) y)
			      (t nil)))f))

(defun suppose (f g)
  (progn (assert-composite f)
	 (assert-composite g)
	 (set-equal f g)))

(defun check (f g)
  (cond ((not (check-composite f)) (format nil "~A not arrow" f))
	((not (check-composite g)) (format nil "~A not arrow" g))
	((are-equal f g) t)
	(t nil)))

(reset-category)
(assert-composite (list 0 1 2 3 4))
(is-composable (list 1 2))
(check-composite (list 2 3))
(add-arrows '(0 1 2))
(list *Equalities*)
(list *Composable-Arrows*)
(are-equal '(0 1) '(0 2))
(equivalence-class '(0 2))
(suppose (list 0 1) (list 0 2))
(check '(0 1) '(0 2))
(check (list 0) (list  1))
