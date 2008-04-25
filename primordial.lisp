;;; -*- Mode: LISP; Package: (PRIMORDIAL :USE CL :COLON-MODE :EXTERNAL); Base: 10; Syntax: Ansi-common-lisp -*-

;;; LaHaShem HaAretz U'Mloah

;;; This file contains a set of functions for testing Screamer prior to
;;; release. Run the function (PRIME-ORDEAL). If it returns T and doesn't
;;; produce any error messages then Screamer is probably OK.

;;; CMU CommonLisp has a bug with DEFPACKAGE.

#-(or poplog akcl cmu) (in-package :screamer-user)

#-(or poplog akcl cmu)
(screamer:define-screamer-package :primordial (:use :iterate))

#+cmu
(defpackage :primordial
 (:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p)
 (:use :cl :screamer :iterate))

(in-package :primordial)

#+(or poplog akcl)
(shadowing-import
 '(screamer::defun screamer::multiple-value-bind screamer::y-or-n-p))

#+(or poplog akcl) (use-package '(:cl :screamer :iterate))

(defun equal-set? (x y)
 (and (subsetp x y :test #'equal) (subsetp y x :test #'equal)))

(defun attacks (qi qj distance)
 (or (= qi qj) (= qi (+ qj distance)) (= qi (- qj distance))))

(defun check-queens (queen queens &optional (distance 1))
 (unless (null queens)
  (if (attacks queen (first queens) distance) (fail))
  (check-queens queen (rest queens) (1+ distance))))

(defun n-queens (n &optional queens)
 (if (= (length queens) n)
     queens
     (let ((queen (an-integer-between 1 n)))
      (check-queens queen queens)
      (n-queens n (cons queen queens)))))

(defun test1 () (= (length (all-values (n-queens 4))) 2))

(defun test2 () (= (length (all-values (n-queens 8))) 92))

(defun a-bit () (either 0 1))

;;; note: DOLIST and DOTIMES work nondeterministically because PUSH doesn't
;;;       destructively modify the list that is being collected so each list
;;;       returned as a nondeterministic value is available after backtracking.
;;; note: Tests 3 through 6 are commented out since they all contain
;;;       nondeterministc DOTIMES and DOLIST which don't work under CMU
;;;       Common Lisp and I don't have time to figure out why.

#+comment
(defun test3-internal (n)
 (local
  (let (collection)
   (dotimes (i n) (push (either 0 1) collection))
   collection)))

#+comment
(defun test3 ()
 (equal-set? (all-values (test3-internal 2)) '((0 0) (1 0) (0 1) (1 1))))

#+comment
(defun test4-internal (n)
 (local (let (collection)
	 (dotimes (i n) (push (a-bit) collection))
	 collection)))

#+comment
(defun test4 ()
 (equal-set? (all-values (test3-internal 2)) '((0 0) (1 0) (0 1) (1 1))))

#+comment
(defun test5-internal (list)
 (local (let (collection)
	 (dolist (e list) (push (either 0 1) collection))
	 collection)))

#+comment
(defun test5 ()
 (equal-set? (all-values (test3-internal 2)) '((0 0) (1 0) (0 1) (1 1))))

#+comment
(defun test6-internal (list)
 (local (let (collection)
	 (dolist (e list) (push (a-bit) collection))
	 collection)))

#+comment
(defun test6 ()
 (equal-set? (all-values (test3-internal 2)) '((0 0) (1 0) (0 1) (1 1))))

;;; Problems with LOOP:
;;;  1. Symbolics implementation of LOOP expands directly into RPLACD without
;;;     going through SETF. This foils the LOCAL declaration.
;;;  2. Results disappear upon ALL-VALUES internal backtracking.
;;;  3. Lucid expands LOOP into a MULTIPLE-VALUE-CALL.

#+comment
(defun test7-internal (n) (local (loop repeat n collect (either 0 1))))

#+comment
(defun test7 ()
 (equal-set? (all-values (test7-internal 2)) '((1 1) (0 1) (1 0) (0 0))))

#+comment
(defun test8-internal (n) (local (loop repeat n collect (a-bit))))

#+comment
(defun test8 ()
 (equal-set? (all-values (test8-internal 2)) '((1 1) (0 1) (1 0) (0 0))))

;;; Problems with ITERATE:
;;;  1. Beta conversion of (let ((#:foo nil)) (setf foo 'bar)) is unsound.
;;;  2. Results disappear upon ALL-VALUES internal backtracking.

#+comment
(defun test9-internal (n)
 (local (iterate (repeat n) (collect (either 0 1)))))

#+comment
(defun test9 ()
 (equal-set? (all-values (test9-internal 2)) '((1 1) (0 1) (1 0) (0 0))))

#+comment
(defun test10-internal (n) (local (iterate (repeat n) (collect (a-bit)))))

#+comment
(defun test10 ()
 (equal-set? (all-values (test10-internal 2)) '((1 1) (0 1) (1 0) (0 0))))

#+comment
(defun bar (n)
 (local (LET* ((COUNT789)
	       (RESULT788)
	       (END-POINTER790)
	       (TEMP791))
	      (BLOCK NIL
		     (TAGBODY
		      (SETQ COUNT789 N)
		      LOOP-TOP-NIL
		      (IF (<= COUNT789 0) (GO LOOP-END-NIL))
		      (PROGN
		       (SETQ TEMP791 (LIST (A-BIT)))
		       (IF TEMP791
			   (SETQ END-POINTER790
				 (IF RESULT788
				     (SETF (CDR END-POINTER790) TEMP791)
				     (SETQ RESULT788 TEMP791))))
		       RESULT788)
		      LOOP-STEP-NIL
		      (SETQ COUNT789 (1- COUNT789))
		      (GO LOOP-TOP-NIL)
		      LOOP-END-NIL)
		     RESULT788))))

(defun test11 ()
 (let ((x (make-variable)))
  (assert! (numberpv x))
  (and (known? (numberpv x))
       (not (known? (notv (numberpv x))))
       (not (known? (realpv x)))
       (not (known? (notv (realpv x))))
       (not (known? (integerpv x)))
       (not (known? (notv (integerpv x)))))))

(defun test12 ()
 (let ((x (make-variable)))
  (assert! (notv (numberpv x)))
  (and (known? (andv (notv (numberpv x))
		     (notv (realpv x))
		     (notv (integerpv x))))
       (not (known? (numberpv x)))
       (not (known? (realpv x)))
       (not (known? (integerpv x))))))

(defun test13 ()
 (let ((x (make-variable)))
  (assert! (realpv x))
  (and (known? (andv (numberpv x) (realpv x)))
       (not (known? (notv (numberpv x))))
       (not (known? (notv (realpv x))))
       (not (known? (integerpv x)))
       (not (known? (notv (integerpv x)))))))

(defun test14 ()
 (let ((x (make-variable)))
  (assert! (notv (realpv x)))
  (and (known? (andv (notv (realpv x)) (notv (integerpv x))))
       (not (known? (numberpv x)))
       (not (known? (notv (numberpv x))))
       (not (known? (realpv x)))
       (not (known? (integerpv x))))))

(defun test15 ()
 (let ((x (make-variable)))
  (assert! (integerpv x))
  (and (known? (andv (integerpv x) (realpv x) (numberpv x)))
       (not (known? (notv (numberpv x))))
       (not (known? (notv (realpv x))))
       (not (known? (notv (integerpv x)))))))

(defun test16 ()
 (let ((x (make-variable)))
  (assert! (notv (integerpv x)))
  (and (known? (notv (integerpv x)))
       (not (known? (numberpv x)))
       (not (known? (realpv x)))
       (not (known? (notv (numberpv x))))
       (not (known? (notv (realpv x))))
       (not (known? (integerpv x))))))

(defun test17 ()
 (let ((x (make-variable)))
  (assert! (numberpv x))
  (assert! (notv (realpv x)))
  (and (known? (andv (numberpv x) (notv (realpv x)) (notv (integerpv x))))
       (not (known? (notv (numberpv x))))
       (not (known? (realpv x)))
       (not (known? (integerpv x))))))

(defun test18 ()
 (let ((x (make-variable)))
  (assert! (numberpv x))
  (assert! (notv (integerpv x)))
  (and (known? (andv (numberpv x) (notv (integerpv x))))
       (not (known? (notv (numberpv x))))
       (not (known? (realpv x)))
       (not (known? (notv (realpv x))))
       (not (known? (integerpv x))))))

(defun test19 ()
 (let ((x (make-variable)))
  (assert! (realpv x))
  (assert! (notv (integerpv x)))
  (and (known? (andv (numberpv x) (realpv x) (notv (integerpv x))))
       (not (known? (notv (numberpv x))))
       (not (known? (notv (realpv x))))
       (not (known? (integerpv x))))))

(defun test20 ()
 (let ((x (make-variable)))
  (null (all-values (assert! (numberpv x)) (assert! (notv (numberpv x)))))))

(defun test21 ()
 (let ((x (make-variable)))
  (null (all-values (assert! (realpv x)) (assert! (notv (realpv x)))))))

(defun test22 ()
 (let ((x (make-variable)))
  (null (all-values (assert! (integerpv x)) (assert! (notv (integerpv x)))))))

(defun test23 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x)))
	(p3 (realpv x))
	(p4 (notv (realpv x)))
	(p5 (integerpv x))
	(p6 (notv (integerpv x))))
  (assert! p1)
  (and (known? p1)
       (not (known? p2))
       (not (known? p3))
       (not (known? p4))
       (not (known? p5))
       (not (known? p6)))))

(defun test24 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x)))
	(p3 (realpv x))
	(p4 (notv (realpv x)))
	(p5 (integerpv x))
	(p6 (notv (integerpv x))))
  (assert! p2)
  (and (not (known? p1))
       (known? p2)
       (not (known? p3))
       (known? p4)
       (not (known? p5))
       (known? p6))))

(defun test25 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x)))
	(p3 (realpv x))
	(p4 (notv (realpv x)))
	(p5 (integerpv x))
	(p6 (notv (integerpv x))))
  (assert! p3)
  (and (known? p1)
       (not (known? p2))
       (known? p3)
       (not (known? p4))
       (not (known? p5))
       (not (known? p6)))))

(defun test26 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x)))
	(p3 (realpv x))
	(p4 (notv (realpv x)))
	(p5 (integerpv x))
	(p6 (notv (integerpv x))))
  (assert! p4)
  (and (not (known? p1))
       (not (known? p2))
       (not (known? p3))
       (known? p4)
       (not (known? p5))
       (known? p6))))

(defun test27 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x)))
	(p3 (realpv x))
	(p4 (notv (realpv x)))
	(p5 (integerpv x))
	(p6 (notv (integerpv x))))
  (assert! p5)
  (and (known? p1)
       (not (known? p2))
       (known? p3)
       (not (known? p4))
       (known? p5)
       (not (known? p6)))))

(defun test28 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x)))
	(p3 (realpv x))
	(p4 (notv (realpv x)))
	(p5 (integerpv x))
	(p6 (notv (integerpv x))))
  (assert! p6)
  (and (not (known? p1))
       (not (known? p2))
       (not (known? p3))
       (not (known? p4))
       (not (known? p5))
       (known? p6))))

(defun test29 ()
 (let* ((x (make-variable))
	(p1 (numberpv x))
	(p2 (notv (numberpv x))))
  (null (all-values (assert! p1) (assert! p2)))))

(defun test30 ()
 (let ((x (make-variable)))
  (null
   (all-values
    (assert!
     (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
    (assert!
     (memberv x '(d e f 4 5 6 4.0 5.0 6.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))))))

(defun test31 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(a b c 4 5 6 4.0 5.0 6.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))
  (and (known? (andv (notv (numberpv x))
		     (notv (realpv x))
		     (notv (integerpv x))))
       (not (known? (numberpv x)))
       (not (known? (realpv x)))
       (not (known? (integerpv x))))))

(defun test32 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(d e f 1 2 3 4.0 5.0 6.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))
  (and (known? (andv (numberpv x) (realpv x) (integerpv x)))
       (not (known? (notv (numberpv x))))
       (not (known? (notv (realpv x))))
       (not (known? (notv (integerpv x)))))))

(defun test33 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(d e f 4 5 6 1.0 2.0 3.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))
  (and (known? (andv (numberpv x) (realpv x) (notv (integerpv x))))
       (not (known? (notv (numberpv x))))
       (not (known? (notv (realpv x))))
       (not (known? (integerpv x))))))

(defun test34 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(d e f 4 5 6 4.0 5.0 6.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (and (known? (andv (numberpv x) (notv (realpv x)) (notv (integerpv x))))
       (not (known? (notv (numberpv x))))
       (not (known? (realpv x)))
       (not (known? (integerpv x))))))

(defun test35 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(b c 2 3 2.0 3.0 #c(2 0.0) #c(3 0.0)))))

(defun test36 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (numberpv x))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(2 3 2.0 3.0 #c(2 0.0) #c(3 0.0)))))

(defun test37 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (notv (numberpv x)))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(b c))))

(defun test38 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (realpv x))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(2 3 2.0 3.0))))

(defun test39 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (notv (realpv x)))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(b c #c(2 0.0) #c(3 0.0)))))

(defun test40 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (integerpv x))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(2 3))))

(defun test41 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (notv (integerpv x)))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(b c 2.0 3.0 #c(2 0.0) #c(3 0.0)))))

(defun test42 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (numberpv x))
  (assert! (notv (integerpv x)))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(2.0 3.0 #c(2 0.0) #c(3 0.0)))))

(defun test43 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (numberpv x))
  (assert! (notv (realpv x)))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(#c(2 0.0) #c(3 0.0)))))

(defun test44 ()
 (let ((x (make-variable)))
  (assert!
   (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
  (assert!
   (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
  (assert! (realpv x))
  (assert! (notv (integerpv x)))
  (equal-set? (all-values (solution x (static-ordering #'linear-force)))
	      '(2.0 3.0))))

(defun test45 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(d e f 4 5 6 4.0 5.0 6.0 #c(4 0.0) #c(5 0.0) #c(6 0.0)))))
  (null (all-values (assert! p1) (assert! p2)))))

(defun test46 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(a b c 4 5 6 4.0 5.0 6.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))
	(p3 (numberpv x))
	(p4 (notv (numberpv x)))
	(p5 (realpv x))
	(p6 (notv (realpv x)))
	(p7 (integerpv x))
	(p8 (notv (integerpv x))))
  (assert! p1)
  (assert! p2)
  (and (not (known? p3))
       (known? p4)
       (not (known? p5))
       (known? p6)
       (not (known? p7))
       (known? p8))))

(defun test47 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(d e f 1 2 3 4.0 5.0 6.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))
	(p3 (numberpv x))
	(p4 (notv (numberpv x)))
	(p5 (realpv x))
	(p6 (notv (realpv x)))
	(p7 (integerpv x))
	(p8 (notv (integerpv x))))
  (assert! p1)
  (assert! p2)
  (and (known? p3)
       (not (known? p4))
       (known? p5)
       (not (known? p6))
       (known? p7)
       (not (known? p8)))))

(defun test48 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(d e f 4 5 6 1.0 2.0 3.0 #c(4 0.0) #c(5 0.0) #c(6 0.0))))
	(p3 (numberpv x))
	(p4 (notv (numberpv x)))
	(p5 (realpv x))
	(p6 (notv (realpv x)))
	(p7 (integerpv x))
	(p8 (notv (integerpv x))))
  (assert! p1)
  (assert! p2)
  (and (known? p3)
       (not (known? p4))
       (known? p5)
       (not (known? p6))
       (not (known? p7))
       (known? p8))))

(defun test49 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(d e f 4 5 6 4.0 5.0 6.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p3 (numberpv x))
	(p4 (notv (numberpv x)))
	(p5 (realpv x))
	(p6 (notv (realpv x)))
	(p7 (integerpv x))
	(p8 (notv (integerpv x))))
  (assert! p1)
  (assert! p2)
  (and (known? p3)
       (not (known? p4))
       (not (known? p5))
       (known? p6)
       (not (known? p7))
       (known? p8))))

(defun test50 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(b c 2 3 2.0 3.0 #c(2 0.0) #c(3 0.0)))))
  (assert! p1)
  (assert! p2)
  (known? p3)))

(defun test51 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(2 3 2.0 3.0 #c(2 0.0) #c(3 0.0))))
	(p4 (numberpv x)))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test52 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(b c)))
	(p4 (notv (numberpv x))))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test53 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(2 3 2.0 3.0)))
	(p4 (realpv x)))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test54 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(b c #c(2 0.0) #c(3 0.0))))
	(p4 (notv (realpv x))))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test55 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(2 3)))
	(p4 (integerpv x)))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test56 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(b c 2.0 3.0 #c(2 0.0) #c(3 0.0))))
	(p4 (notv (integerpv x))))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test57 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(2.0 3.0 #c(2 0.0) #c(3 0.0))))
	(p4 (andv (numberpv x) (notv (integerpv x)))))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test58 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(#c(2 0.0) #c(3 0.0))))
	(p4 (andv (numberpv x) (notv (realpv x)))))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test59 ()
 (let* ((x (make-variable))
	(p1
	 (memberv x '(a b c 1 2 3 1.0 2.0 3.0 #c(1 0.0) #c(2 0.0) #c(3 0.0))))
	(p2
	 (memberv x '(b c d 2 3 4 2.0 3.0 4.0 #c(2 0.0) #c(3 0.0) #c(4 0.0))))
	(p3 (memberv x '(2.0 3.0)))
	(p4 (andv (realpv x) (notv (integerpv x)))))
  (assert! p1)
  (assert! p2)
  (assert! p4)
  (known? p3)))

(defun test60 ()
 (let* ((x (make-variable))
	(p1 (notv (memberv x '(a b c d))))
	(p2 (memberv x '(c d e f g h)))
	(p3 (notv (memberv x '(g h i j))))
	(p4 (andv (memberv x '(e f)) (notv (memberv x '(a b c d g h i j z))))))
  (assert! p1)
  (assert! p2)
  (assert! p3)
  (known? p4)))

(defun test61 ()
 (let* ((x (make-variable))
	(p1 (notv (memberv x '(a b c d))))
	(p2 (memberv x '(c d g h)))
	(p3 (notv (memberv x '(g h i j)))))
  (null (all-values (assert! p1) (assert! p2) (assert! p3)))))

(defun test62 ()
 (let ((x (an-integer-betweenv 0 4))
       (y (an-integer-betweenv 8 12))
       (z (a-member-ofv '(-1 6 13))))
  (assert! (<=v x z y))
  (known? (memberv z '(6)))))

(defun test63 ()
 (let* ((x (make-variable))
	(y (make-variable))
	(z (make-variable))
	(p1 (andv (integerpv x) (>=v x 0) (<=v x 4)))
	(p2 (andv (integerpv y) (>=v y 8) (<=v y 12)))
	(p3 (memberv z '(-1 6 13)))
	(p4 (<=v x z y))
	(p5 (andv (memberv z '(6)) (notv (memberv z '(-1 13))))))
  (assert! p1)
  (assert! p2)
  (assert! p3)
  (assert! p4)
  (known? p5)))

(defun test64-internal (n)
 (let ((q (make-array (list n n)))
       (top t))
  (iterate (for i from 0 below n)
	   (iterate (for j from 0 below n)
		    (setf (aref q i j) (make-variable))
		    (assert! (booleanpv (aref q i j)))))
  (iterate (for i from 0 below n)
	   (for row = nil)
	   (iterate (for j from 0 below n)
		    (setf row (orv row (aref q i j))))
	   (setf top (andv top row)))
  (iterate (for j from 0 below n)
	   (for column = nil)
	   (iterate (for i from 0 below n)
		    (setf column (orv column (aref q i j))))
	   (setf top (andv top column)))
  (iterate
   (for i from 0 below n)
   (iterate
    (for j from 0 below n)
    (iterate
     (for k from 0 below n)
     (if (/= j k)
	 (setf top (andv top (notv (andv (aref q i j) (aref q i k))))))
     (if (/= i k)
	 (setf top (andv top (notv (andv (aref q i j) (aref q k j))))))
     (if (and (/= k 0) (< (+ i k) n) (< (+ j k) n))
	 (setf top
	       (andv top (notv (andv (aref q i j) (aref q (+ i k) (+ j k)))))))
     (if (and (/= k 0) (< (+ i k) n) (>= (- j k) 0))
	 (setf top
	       (andv top (notv (andv (aref q i j) (aref q (+ i k) (- j k)))))))
     (if (and (/= k 0) (>= (- i k) 0) (< (+ j k) n))
	 (setf top
	       (andv top (notv (andv (aref q i j) (aref q (- i k) (+ j k)))))))
     (if (and (/= k 0) (>= (- i k) 0) (>= (- j k) 0))
	 (setf top
	       (andv top (notv (andv (aref q i j)
				     (aref q (- i k) (- j k))))))))))
  (assert! top)
  (length
   (all-values
    (solution
     (iterate outer
	      (for i from 0 below n)
	      (iterate (for j from 0 below n)
		       (in outer (collect (aref q i j)))))
     (static-ordering #'linear-force))))))

(defun test64 () (= (test64-internal 8) 92))

(defun test65-internal (n)
 (let ((q (make-array (list n n))))
  (iterate (for i from 0 below n)
	   (iterate (for j from 0 below n)
		    (setf (aref q i j) (make-variable))
		    (assert! (booleanpv (aref q i j)))))
  (iterate (for i from 0 below n)
	   (for row = nil)
	   (iterate (for j from 0 below n)
		    (setf row (orv row (aref q i j))))
	   (assert! row))
  (iterate (for j from 0 below n)
	   (for column = nil)
	   (iterate (for i from 0 below n)
		    (setf column (orv column (aref q i j))))
	   (assert! column))
  (iterate
   (for i from 0 below n)
   (iterate
    (for j from 0 below n)
    (iterate
     (for k from 0 below n)
     (if (/= j k)
	 (assert! (notv (andv (aref q i j) (aref q i k)))))
     (if (/= i k)
	 (assert! (notv (andv (aref q i j) (aref q k j)))))
     (if (and (/= k 0) (< (+ i k) n) (< (+ j k) n))
	 (assert! (notv (andv (aref q i j) (aref q (+ i k) (+ j k))))))
     (if (and (/= k 0) (< (+ i k) n) (>= (- j k) 0))
	 (assert! (notv (andv (aref q i j) (aref q (+ i k) (- j k))))))
     (if (and (/= k 0) (>= (- i k) 0) (< (+ j k) n))
	 (assert! (notv (andv (aref q i j) (aref q (- i k) (+ j k))))))
     (if (and (/= k 0) (>= (- i k) 0) (>= (- j k) 0))
	 (assert! (notv (andv (aref q i j) (aref q (- i k) (- j k)))))))))
  (length
   (all-values
    (solution
     (iterate outer
	      (for i from 0 below n)
	      (iterate (for j from 0 below n)
		       (in outer (collect (aref q i j)))))
     (static-ordering #'linear-force))))))

(defun test65 () (= (test65-internal 8) 92))

(defun test66 ()
 (let ((x (an-integer-betweenv 1 4))
       (y (a-member-ofv '(5 7))))
  (known? (funcallv #'(lambda (x y z) (< x y z)) x y 10))))

(defun test67 ()
 (let ((x (an-integer-betweenv 1 4))
       (y (a-member-ofv '(5 7))))
  (known? (notv (funcallv #'(lambda (x y z) (> x y z)) x y 10)))))

(defun test68 ()
 (let ((w (an-integer-betweenv 1 4))
       (x (a-member-ofv '(5 7)))
       (y (a-member-ofv '(8 10))))
  (known? (applyv #'(lambda (w x y z) (< w x y z)) w x (list y 11)))))

(defun test69 ()
 (let ((w (an-integer-betweenv 1 4))
       (x (a-member-ofv '(5 7)))
       (y (a-member-ofv '(8 10))))
  (known? (notv (applyv #'(lambda (w x y z) (> w x y z)) w x (list y 11))))))

(defun prime-ordeal ()
 (let ((bug? nil))
  (unless (test1) (format t "~% Test 1 failed") (setf bug? t))
  (unless (test2) (format t "~% Test 2 failed") (setf bug? t))
  (unless (test11) (format t "~% Test 11 failed") (setf bug? t))
  (unless (test12) (format t "~% Test 12 failed") (setf bug? t))
  (unless (test13) (format t "~% Test 13 failed") (setf bug? t))
  (unless (test14) (format t "~% Test 14 failed") (setf bug? t))
  (unless (test15) (format t "~% Test 15 failed") (setf bug? t))
  (unless (test16) (format t "~% Test 16 failed") (setf bug? t))
  (unless (test17) (format t "~% Test 17 failed") (setf bug? t))
  (unless (test18) (format t "~% Test 18 failed") (setf bug? t))
  (unless (test19) (format t "~% Test 19 failed") (setf bug? t))
  (unless (test20) (format t "~% Test 20 failed") (setf bug? t))
  (unless (test21) (format t "~% Test 21 failed") (setf bug? t))
  (unless (test22) (format t "~% Test 22 failed") (setf bug? t))
  (unless (test23) (format t "~% Test 23 failed") (setf bug? t))
  (unless (test24) (format t "~% Test 24 failed") (setf bug? t))
  (unless (test25) (format t "~% Test 25 failed") (setf bug? t))
  (unless (test26) (format t "~% Test 26 failed") (setf bug? t))
  (unless (test27) (format t "~% Test 27 failed") (setf bug? t))
  (unless (test28) (format t "~% Test 28 failed") (setf bug? t))
  (unless (test29) (format t "~% Test 29 failed") (setf bug? t))
  (unless (test30) (format t "~% Test 30 failed") (setf bug? t))
  (unless (test31) (format t "~% Test 31 failed") (setf bug? t))
  (unless (test32) (format t "~% Test 32 failed") (setf bug? t))
  (unless (test33) (format t "~% Test 33 failed") (setf bug? t))
  (unless (test34) (format t "~% Test 34 failed") (setf bug? t))
  (unless (test35) (format t "~% Test 35 failed") (setf bug? t))
  (unless (test36) (format t "~% Test 36 failed") (setf bug? t))
  (unless (test37) (format t "~% Test 37 failed") (setf bug? t))
  (unless (test38) (format t "~% Test 38 failed") (setf bug? t))
  (unless (test39) (format t "~% Test 39 failed") (setf bug? t))
  (unless (test40) (format t "~% Test 40 failed") (setf bug? t))
  (unless (test41) (format t "~% Test 41 failed") (setf bug? t))
  (unless (test42) (format t "~% Test 42 failed") (setf bug? t))
  (unless (test43) (format t "~% Test 43 failed") (setf bug? t))
  (unless (test44) (format t "~% Test 44 failed") (setf bug? t))
  (unless (test45) (format t "~% Test 45 failed") (setf bug? t))
  (unless (test46) (format t "~% Test 46 failed") (setf bug? t))
  (unless (test47) (format t "~% Test 47 failed") (setf bug? t))
  (unless (test48) (format t "~% Test 48 failed") (setf bug? t))
  (unless (test49) (format t "~% Test 49 failed") (setf bug? t))
  (unless (test50) (format t "~% Test 50 failed") (setf bug? t))
  (unless (test51) (format t "~% Test 51 failed") (setf bug? t))
  (unless (test52) (format t "~% Test 52 failed") (setf bug? t))
  (unless (test53) (format t "~% Test 53 failed") (setf bug? t))
  (unless (test54) (format t "~% Test 54 failed") (setf bug? t))
  (unless (test55) (format t "~% Test 55 failed") (setf bug? t))
  (unless (test56) (format t "~% Test 56 failed") (setf bug? t))
  (unless (test57) (format t "~% Test 57 failed") (setf bug? t))
  (unless (test58) (format t "~% Test 58 failed") (setf bug? t))
  (unless (test59) (format t "~% Test 59 failed") (setf bug? t))
  (unless (test60) (format t "~% Test 60 failed") (setf bug? t))
  (unless (test61) (format t "~% Test 61 failed") (setf bug? t))
  (unless (test62) (format t "~% Test 62 failed") (setf bug? t))
  (unless (test63) (format t "~% Test 63 failed") (setf bug? t))
  (unless (test64) (format t "~% Test 64 failed") (setf bug? t))
  (unless (test65) (format t "~% Test 65 failed") (setf bug? t))
  (unless (test66) (format t "~% Test 66 failed") (setf bug? t))
  (unless (test67) (format t "~% Test 67 failed") (setf bug? t))
  (unless (test68) (format t "~% Test 68 failed") (setf bug? t))
  (unless (test69) (format t "~% Test 69 failed") (setf bug? t))
  (if bug? (error "Screamer has a bug")))
 t)

;;; Tam V'Nishlam Shevah L'El Borei Olam
