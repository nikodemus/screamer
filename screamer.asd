;;;; Screamer
;;;; A portable efficient implementation of nondeterministic Common Lisp
;;;;
;;;; Written by:
;;;;
;;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;;
;;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;;; Copyright 1993 University of Toronto. All rights reserved.
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

(defsystem :screamer
  :serial t
  :licence "MIT"
  :description "Nondeterministic programming and constraint propagation."
  :author "Jeffrey Mark Siskind & David Allen McAllester"
  :maintainer "Nikodemus Siivola <nikodemus@random-state.net>"
  :components
  ((:file "package")
   (:file "screamer")))

(defsystem :screamer-tests
  :serial t
  :licence "MIT"
  :description "Tests for Screamer"
  :author "Jeffrey Mark Siskind & David Allen McAllester"
  :maintainer "Nikodemus Siivola <nikodemus@random-state.net>"
  :depends-on (:screamer :iterate :hu.dwim.stefil)
  :components ((:file "primordial")
               (:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :screamer))))
  (load-system :screamer-tests)
  (funcall (intern (string '#:test-screamer) :screamer-tests)))
