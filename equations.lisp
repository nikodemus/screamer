;;; -*- Mode: LISP; Package: (SCREAMS :USE CL :COLON-MODE :EXTERNAL); Base: 10; Syntax: Ansi-common-lisp -*-

;;; LaHaShem HaAretz U'Mloah

#||#(in-package :screamer-user)

#-(or poplog akcl)
(screamer:define-screamer-package :screams (:use :iterate))

#-(or poplog akcl)
(in-package :screams)

#+(or poplog akcl)
(use-package :iterate)

(defvar *infinity* 1e38)

(defvar *fuzz* 1e-6)

(defun make-interval (low high) (cons low high))

(defun low (interval) (car interval))

(defun high (interval) (cdr interval))

(defun size (interval) (- (high interval) (low interval)))

(defun small? (interval) (< (size interval) *fuzz*))

(defun disjoint? (interval1 interval2)
 (or (< (high interval1) (low interval2))
     (> (low interval1) (high interval2))))

(defun intersects? (interval1 interval2) (not (disjoint? interval1 interval2)))

(defun add-interval (interval1 interval2)
 (make-interval (+ (low interval1) (low interval2))
		(+ (high interval1) (high interval2))))

(defun multiply-interval (interval1 interval2)
 (let ((l1 (low interval1))
       (h1 (high interval1))
       (l2 (low interval2))
       (h2 (high interval2)))
  (let ((b1 (* l1 l2))
	(b2 (* l1 h2))
	(b3 (* h1 l2))
	(b4 (* h1 h2)))
   (make-interval (min b1 b2 b3 b4) (max b1 b2 b3 b4)))))

(defun compute-interval (expression environment)
 (cond ((numberp expression) (make-interval expression expression))
       ((symbolp expression) (cdr (assoc expression environment)))
       ((eq (car expression) '+)
	(reduce #'add-interval
		(mapcar #'(lambda (expression)
			   (compute-interval expression environment))
			(rest expression))))
       ((eq (car expression) '*)
	(reduce #'multiply-interval
		(mapcar #'(lambda (expression)
			   (compute-interval expression environment))
			(rest expression))))))

(defun impossible? (equation environment)
 (disjoint? (compute-interval (second equation) environment)
	    (compute-interval (third equation) environment)))

(defun variables-of (expression)
 (cond ((numberp expression) nil)
       ((member expression '(+ * =)) nil)
       ((null expression) nil)
       ((atom expression) (list expression))
       (t (append (variables-of (car expression))
		  (variables-of (cdr expression))))))

(defun make-environment (variables interval)
 (cond ((null variables) nil)
       ((member (first variables) (rest variables))
	(make-environment (rest variables) interval))
       (t (cons (cons (first variables) interval)
		(make-environment (rest variables) interval)))))

(defun a-half-interval (interval)
 (let ((midpoint (/ (+ (low interval) (high interval)) 2)))
  (either (make-interval (low interval) midpoint)
	  (make-interval midpoint (high interval)))))

(defun copy-all (tree)
 (if (atom tree) tree (cons (copy-all (car tree)) (copy-all (cdr tree)))))

(defun biggest-cell (environment)
 (if (null (rest environment))
     (first environment)
     (let ((big-cell (biggest-cell (rest environment))))
      (let ((interval1 (cdr (first environment)))
	    (interval2 (cdr big-cell)))
       (if (> (size interval1) (size interval2))
	   (first environment)
	   big-cell)))))

(defun refine-environment (environment equations)
 (let ((cell (biggest-cell environment)))
  (cond ((small? (cdr cell)) (copy-all environment))
	(t (local (setf (cdr cell) (a-half-interval (cdr cell))))
	   (if (some #'(lambda (equation) (impossible? equation environment))
		     equations)
	       (fail))
	   (refine-environment environment equations)))))

(defun solve (equations)
 (refine-environment
  (make-environment (variables-of equations)
		    (make-interval (- *infinity*) *infinity*))
  equations))

(defun dam-test ()
 (for-effects (print (solve '((= (+ (* x x) (* y y)) (* z z))
			      (= x (+ 5 y))
			      (= z (+ x y 3)))))))

(defun dam-testa ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*))
	  (z (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (andv (=v (+v (*v x x) (*v y y)) (*v z z))
		    (=v x (+v 5 y))
		    (=v z (+v x y 3))))
     (list x y z))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun eq4 () (for-effects (print (solve '((= (+ x x x x) 3))))))

(defun eq4a ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (=v (+v x x x x) 3.0))
     x)
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun eq3 () (for-effects (print (solve '((= (* 4 x) 3))))))

(defun eq2 ()
 (for-effects
  (print (solve '((= (+ x x y y y) 17)
		  (= (+ x x x x x x x y y y y) 27))))))

(defun eq2a ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (andv (=v (+v x x y y y) 17.0)
		    (=v (+v x x x x x x x y y y y) 27.0)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun eq1 ()
 (for-effects
  (print (solve '((= (+ (* 2 x) (* 3 y)) 17) (= (+ (* 7 x) (* 4 y)) 27))))))

(defun equation1 ()
 (for-effects
  (print
   (solution
    (let ((x (an-integer-betweenv -1000000 1000000))
	  (y (an-integer-betweenv -1000000 1000000)))
     (assert! (andv (=v (+v (*v 2 x) (*v 3 y)) 17)
		    (=v (+v (*v 7 x) (*v 4 y)) 27)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation1a ()
 ;; note: This uses ratnums which eventually get turned into flonums.
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv -1000000 1000000))
	  (y (a-real-betweenv -1000000 1000000)))
     (assert! (andv (=v (+v (*v 2 x) (*v 3 y)) 17)
		    (=v (+v (*v 7 x) (*v 4 y)) 27)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation1b ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (andv (=v (+v (*v 2 x) (*v 3 y)) 17)
		    (=v (+v (*v 7 x) (*v 4 y)) 27)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation1c ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (andv (=v (+v (*v 2.0 x) (*v 3.0 y)) 17.0)
		    (=v (+v (*v 7.0 x) (*v 4.0 y)) 27.0)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation2 ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (=v (*v x x) 4.0))
     x)
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation3 ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (andv (=v (+v x x y y y) 17.0)
		    (=v (+v x x x x x x x y y y y) 27.0)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation3a ()
 ;; note: This uses ratnums which eventually get turned into flonums.
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv -1000000 1000000))
	  (y (a-real-betweenv -1000000 1000000)))
     (assert! (andv (=v (+v x x y y y) 17)
		    (=v (+v x x x x x x x y y y y) 27)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation3b ()
 (for-effects
  (print
   (solution
    (let ((x (an-integer-betweenv (- *infinity*) *infinity*))
	  (y (an-integer-betweenv (- *infinity*) *infinity*)))
     (assert! (andv (=v (+v x x y y y) 17)
		    (=v (+v x x x x x x x y y y y) 27)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation3c ()
 (for-effects
  (print
   (solution
    (let ((x (an-integer-betweenv -1000000 1000000))
	  (y (an-integer-betweenv -1000000 1000000)))
     (assert! (andv (=v (+v x x y y y) 17)
		    (=v (+v x x x x x x x y y y y) 27)))
     (list x y))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation4 ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv -1000000 1000000)))
     (assert! (=v (+v x x x x) 8.0))
     x)
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation4a ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (=v (+v x x x x) 3))
     x)
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation4b ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*)))
     (assert! (=v (+v x x x x) 8.0))
     x)
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun equation4c ()
 (for-effects
  (print
   (solution
    (let ((x (an-integer-betweenv -1000000 1000000)))
     (assert! (=v (+v x x x x) 8.0))
     x)
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun nonlinear1 ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*))
	  (z (a-real-betweenv (- *infinity*) *infinity*)))
     (assert!
      (andv (orv (=v (+v (*v 4 x x y) (*v 7 y z z) (*v 6 x x z z)) 2)
		 (=v (+v (*v 3 x y) (*v 2 y y) (*v 5 x y z)) -4))
	    (>=v (*v (+v x y) (+v y z)) -5)))
     (list x y z))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

(defun nonlinear2 ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv (- *infinity*) *infinity*))
	  (y (a-real-betweenv (- *infinity*) *infinity*))
	  (z (a-real-betweenv (- *infinity*) *infinity*)))
     (assert!
      (andv (=v (+v (*v 4 x x y) (*v 7 y z z) (*v 6 x x z z)) 2)
	    (=v (+v (*v 3 x y) (*v 2 y y) (*v 5 x y z)) -4)
	    (>=v (*v (+v x y) (+v y z)) -5)))
     (list x y z))
    (reorder #'range-size
	     #'(lambda (x) (< x *fuzz*))
	     #'>
	     #'divide-and-conquer-force)))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
