;;;; Example "Sudoku"
;;;;
;;;; Using Screamer to solve a sudoku puzzle.

(eval-when (:compile-toplevel :load-toplevel)
  (require :screamer))

(in-package :screamer-user)

;;;; Utility definitions
;;;;
;;;; VAR-INTEGER-LIST returns a list of COUNT variables constrained to be
;;;; between MIN and MAX.
;;;;
;;;; ALL-DIFFERENTV returns a variable constrained to be true if variables in
;;;; the list received all have different values.

(defun var-integer-list (count min max)
  (loop for i below count
        collect (an-integer-betweenv min max)))

(defun all-differentv (x &rest xs)
  ;; Functionally the same as (apply #'/=v list), but faster.
  (labels ((all-different (x xs)
             (if (null xs)
                 t
                 (andv (notv (=v x (car xs)))
                       (all-different x (cdr xs))
                       (all-different (car xs) (cdr xs))))))
    (all-different x xs)))

;;;; Utility functions for Sudoku CSP modelling

(defun chunk (size list)
  (declare (type (integer 0 *) size))
  (let (chunks)
    (loop for l = list then (subseq l size)
          while l do (push (subseq l 0 size) chunks)
          finally (return (nreverse chunks)))))

(defun matrix-transpose (matrix)
  (when matrix
    (apply #'mapcar #'list matrix)))

(defun rows (list &optional (size 9))
  (chunk size list))

(defun columns (list &optional (size 9))
  (matrix-transpose (chunk size list)))

(defun boxes (list &optional (size 3))
  (flet ((flatten (list)
           (apply #'append list)))
    (flatten (mapcar (lambda (x)
                       (mapcar #'flatten
                               (matrix-transpose x)))
                     (chunk size
                            (chunk size
                                   (chunk size
                                          list)))))))

;;;; the CSPs

(defparameter *sudoku-problem-1*
  '(_ _ 8   _ _ _   6 _ _
    _ 4 _   9 _ 2   _ 5 _
    _ _ _   6 4 8   _ _ _

    _ 3 9   _ 2 _   1 7 _
    _ 1 _   _ _ _   _ 3 _
    _ 8 5   _ 1 _   2 6 _

    _ _ _   2 8 7   _ _ _
    _ 6 _   1 _ 4   _ 8 _
    _ _ 2   _ _ _   5 _ _))

(defparameter *sudoku-problem-2*
  '(_ 2 6   _ _ _   8 1 _
    3 _ _   7 _ 8   _ _ 6
    4 _ _   _ 5 _   _ _ 7

    _ 5 _   1 _ 7   _ 9 _
    _ _ 3   9 _ 5   1 _ _
    _ 4 _   3 _ 2   _ 5 _

    1 _ _   _ 3 _   _ _ 2
    5 _ _   2 _ 4   _ _ 9
    _ 3 8   _ _ _   4 6 _))

(defparameter *sudoku-problem-3*
  '(_ _ 2   3 _ _   7 _ _
    _ _ 4   _ _ 9   _ _ _
    6 _ _   _ _ _   _ 5 _

    _ 7 _   _ _ 2   _ 6 _
    _ _ 3   7 _ _   4 _ _
    _ 1 _   _ _ _   _ 2 _

    _ 3 _   _ _ _   _ _ 9
    _ _ _   4 _ _   6 _ _
    _ _ 5   _ _ 8   2 _ _))

(defun sudoku (grid)
  (let ((vars (mapcar (lambda (x)
                        (if (numberp x)
                            x
                            (an-integer-betweenv 1 9)))
                      grid)))
    (one-value
     (solution
      (dolist (list (append (rows vars)
                            (columns vars)
                            (boxes vars))
               vars)
        (assert! (apply 'all-differentv list)))
      (reorder #'domain-size
               #'(lambda (x) (declare (ignore x)) nil)
               #'<
               #'linear-force)))))

(defun run ()
  (loop repeat 1
        do (sudoku *sudoku-problem-1*)
           (sudoku *sudoku-problem-2*)
           (sudoku *sudoku-problem-3*)))



