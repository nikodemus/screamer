;;;; Copyright 2010, Nikodemus Siivola <nikodemus@sb-studio.net>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :screamer-user)

(define-screamer-package :screamer-tests
  (:use :cl :hu.dwim.stefil))

(in-package :screamer-tests)

(defsuite (test-screamer :in root-suite) ()
  (run-child-tests))

(in-suite test-screamer)

(defun eval-when/ct ()
  (let ((x (either :a :b)))
    (declare (ignorable x))
    (or (eval-when (:compile-toplevel)
          x)
        t)))

(defun eval-when/lt ()
  (let ((x (either :a :b)))
    (declare (ignorable x))
    (or (eval-when (:load-toplevel)
          x)
        t)))

(defun eval-when/ex ()
  (let ((x (either :a :b)))
    (or (eval-when (:execute)
          x)
        t)))

(deftest eval-when.situations ()
  (is (equal '(t t) (all-values (eval-when/ct))))
  (is (equal '(t t) (all-values (eval-when/lt))))
  (is (equal '(:a :b) (all-values (eval-when/ex)))))
