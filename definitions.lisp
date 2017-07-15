(defvar *Equalities* nil)
(defvar *Composable-Arrows* nil)

(defun new-category () (progn (setq *Equalities* nil)
			      (setq *Composable-Arrows* nil)))

(defun is-arrow (f) (member (list f) *Composable-Arrows* :test #'equal))

(defun add-arrow (f)
  (cond ((is-arrow f) ())
	(t (progn (setq *Equalities* (push (list f) *Equalities*))
		  (setq *Composable-Arrows* (push (list f) *Composable-Arrows*))))))

(defun add-arrows (arrow-list) (dolist (arrow arrow-list) (add-arrow arrow)))

(defun equivalence-class (f) (find-if (lambda (x) (member f x :test #'equal) ) *Equalities*))

(defun are-equal (f g) (member f (equivalence-class g) :test #'equal))

(defun set-equal (f g)
  (let ((x (concatenate 'list (equivalence-class f) (equivalence-class g))))
    (progn (setq *Equalities* (remove (equivalence-class f) *Equalities*))
	   (setq *Equalities* (remove (equivalence-class g) *Equalities*))
	   (setq *Equalities* (push x *Equalities*)))))

(defun is-composable (f) (cond ((member f *Composable-Arrows* :test #'equal) t)
			       (t nil)))
(defun assert-composite (f) (reduce (lambda (x y) (progn (setq *Composable-Arrows* (push (list x y) *Composable-Arrows*))
							 y)) f))
(defun check-composite (f) (reduce (lambda (x y) (cond ((not x) nil)
						       ((is-composable (list x y)) y)
						       (t nil)))f))

(defun suppose (f g)
  (progn (add-arrow f)
	 (add-arrow g)
	 (set-equal f g)))

(defun check (f g) (cond ((not (member (list f) *Composable-Arrows* :test #'equal)) (format nil "~A not arrow" f))
			   ((not (member (list g) *Composable-Arrows* :test #'equal)) (format nil "~A not arrow" g))
			   ((are-equal f g) t)
			   (t nil)))

(new-category)
(assert-composite (list 0 1 2 3 4))
(is-composable (list 1 2))
(check-composite (list 1 2 3 4))
(add-arrows '(0 1 2))
(suppose 0 1)
(list *Equalities*)
(list *Composable-Arrows*)
(are-equal '(0 1) '(0 2))
(equivalence-class '(0 2))
(suppose '(0 1) '(0 2))
(check '(0 1) '(0 2))
(check 0 1)
