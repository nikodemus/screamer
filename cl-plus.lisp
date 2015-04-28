;;; This package definition and exporting approach permits incremental
;;; additions to the CL+ package by a local user without compiler warnings
;;; (which can otherwise result in compilation being aborted).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (make-package '#:cl+ :use '(#:cl))))

(in-package #:cl+)

(export 'defconst)
(defmacro defconst (name initial-value &optional doc)
  "Only evaluates INITIAL-VALUE when NAME is unbound to a value."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (if (boundp ',name)
	',name
	(defconstant ,name ,initial-value ,@(when doc (list doc))))))
