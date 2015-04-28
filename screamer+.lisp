;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ===================================================
;;;              ===                 SCREAMER+                   ===
;;;              ===  increasing the expressiveness of SCREAMER  ===
;;;              ===================================================
;;;
;;;                  Copyright 1998-2000 University of Aberdeen
;;;                  
;;; This source code may be used cost-free for non-commercial use. However,
;;; I request that you provide me with a short description of how you are
;;; using the software so that I can build up a profile of applications.
;;; You are free to modify and extend the source code, but please report any 
;;; bugs found (together with any fixes), so that the quality of the code can
;;; be improved. 
;;;
;;; You may not distribute the code without prior consent from me.
;;;
;;; This software represents "work in progress" and is not (guaranteed to be) 
;;; bug-free!
;;;
;;; And remember...
;;;      'Sometimes a scream is better than a thesis'...
;;;
;;; Happy Screaming!
;;;
;;; Simon White, February 2000                          swhite@csd.abdn.ac.uk
;;;
;;; Department of Computing Science
;;; King's College
;;; University of Aberdeen
;;; Aberdeen AB24 3UE
;;; Scotland, UK.
;;;
;;; Description:        This file contains functions which are arguably
;;;                     missing from the original implementation of SCREAMER
;;;                     The functions in this file are backwards compatible
;;;                     with those of SCREAMER; i.e., all SCREAMER programs
;;;                     should also work with SCREAMER+. However, the functions
;;;                     which are exclusive to SCREAMER+ often have superior
;;;                     propagation properties than those of SCREAMER.
;;;
;;; Changes Log:
;;; ------------
;;; 02/04/1998          First version.
;;; 23/09/1998          Added 'carefully' macro
;;; 07/10/1998          Consistent with draft TR "Constraint Handling in Common
;;;                     LISP"
;;; 10/11/1998          Added not-equalv, which has improved propagation
;;;                     properties over (notv (equalv ))
;;; 19/02/1999          Fixed bug in somev, everyv, not-anyv, not-everyv
;;;                     (My use of 'push' on the local variable had meant
;;;                     that it retained its value from one call to the next.
;;;                     This disturbed the search for solutions.)
;;; 11/03/1999          Added new versions of variables-in and apply-substitution
;;;                     which cope with arrays and objects as well as conses
;;; 18/03/1999          Added extra case in nthv which constrains the index if
;;;                     z and the list become bound first.
;;; 19/03/1999          Added eqv and funcallgv
;;; 22/03/1999          Simplified and bugfixed at-mostv and at-leastv
;;;                     Added setq-domains
;;; 19/05/1999          Changed definition of constraint-fn slightly, so
;;;                     that it returns a function definition, rather than
;;;                     a closure, if possible
;;; 30/07/1999          Improved commenting, so that the propagation powers
;;;                     of each function is clear. Added restv.
;;;                     Changed package concept to be compatible with CLtL2
;;;                     Now the SCREAMER+ package holds SCREAMER+/SCREAMER
;;; 02/08/1999          Extended the type concept of SCREAMER. Now a variable
;;;                     can be known to be of any type
;;; 05/09/1999          Repaired bug in ifv
;;; 17/02/2000          Changed defpackage so that symbol names are consistent
;;;                     across packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I discovered some useful, undocumented features of SCREAMER too
;;;
;;; e.g. when-failing
;;; (when-failing
;;;  ((format t "Sol is ~a ~%" sol))
;;;  (one-value (solution sol (static-ordering #'linear-force)))
;;;  )
;;;
;;; count-failures
;;;
;;; best-value
;;; e.g. 
;;; USER(652): (setq a (an-integer-betweenv 1 10))
;;; [57733 integer 1:10 enumerated-domain:(1 2 3 4 5 6 7 8 9 10)]
;;; USER(653): USER(653): (best-value (solution a (static-ordering #'linear-force)) (-v a))
;;; (1 -1)
;;; USER(654):
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Home directory of SCREAMER+ on UNIX
;;; You may need to change this
#+unix (defconstant *screamer+-path* "/home/akt/local/software/screamer+v1.0")

;;; Home directory of SCREAMER+ on PC (assumes NOT unix => PC)
;;; You may need to change this
#-unix (defconstant *screamer+-path* "C:/Allegro/Screamer+v1.0")

;;; Derive path of SCREAMER assuming it is in a subdirectory of the
;;; SCREAMER+ directory
(defconstant *screamer-path* 
  (concatenate 'string *screamer+-path* "/screamer/screamer")
  )
 
;;; Stops LISP complaining that BOOLEAN is redefined by Screamer
(setq excl:*enable-package-locked-errors* nil)

;;; Screamer+ requires screamer
(unless (find-package :SCREAMER)
  (load *screamer-path*)
  )

(provide :screamer+)

(defpackage :screamer+
  (:use :common-lisp :screamer)
  (:export
   ;; --- Export SCREAMER+ Functions ---
   :listpv :conspv :symbolpv :stringpv :typepv
   :a-listv :a-consv :a-symbolv :a-stringv :a-typed-varv
   ;; Boolean
   :impliesv :not-equalv
   ;; Expression
   :ifv :make-equal
   ;; Lists
   :carv :cdrv :consv :firstv :secondv :thirdv :fourthv :restv :nthv :subseqv
   :lengthv :appendv :make-listv :all-differentv
   ;; Sets and Bags
   :set-equalv :subsetpv :intersectionv :unionv :bag-equalv
   ;; Arrays
   :make-arrayv :arefv
   ;; Objects
   :make-instancev :classpv :slot-valuev :class-ofv :class-namev
   :slot-exists-pv :reconcile
   ;; Higher Order Functions
   :funcallinv :mapcarv :maplistv :everyv :somev :noteveryv :notanyv
   :at-leastv :at-mostv :exactlyv :constraint-fn
   ;; Miscellaneous
   :formatv :*enumeration-limit*
   ;; Some other useful stuff not published as part of SCREAMER+
   :carefully :slot-names-of :objectp
   ;; More recently added
   :eqv :funcallgv :setq-domains
   ;; --- Export SCREAMER Functions ---
   ;; Perhaps I don't need to export all the lower-level ones
   :either :local :global :for-effects :local-output
   :multiple-value-call-nondeterministic :nondeterministic-function?
   :funcall-nondeterministic :apply-nondeterministic :unwind-trail
   :a-boolean :an-integer :a-member-of
   :an-integer-above :an-integer-below :an-integer-between
   :possibly? :necessarily? :known? :decide
   :solution :all-values :one-value :best-value :print-values :ith-value
   :fail :when-failing :count-failures :boolean :booleanp
   :assert! :make-variable :count-trues :count-truesv
   :numberpv :realpv :integerpv :booleanpv :memberv
   :=v :<v :<=v :>v :>=v :/=v :+v :-v :*v :/v :minv :maxv
   :a-booleanv :an-integerv :an-integer-abovev :an-integer-belowv
   :an-integer-betweenv :a-realv :a-real-abovev :a-real-belowv
   :a-real-betweenv :a-numberv :a-member-ofv :notv :andv :orv
   :funcallv :applyv :equalv :bound? :ground? :value-of
   :apply-substitution :template :domain-size :range-size
   :reorder :static-ordering :linear-force :divide-and-conquer-force
   :define-screamer-package :purge :unwedge-screamer 
   :*dynamic-extent?* :*iscream?* :*minimum-shrink-ratio* :*strategy*
   :*maximum-discretization-range* :*screamer-version*
   )
  ;; Import some unexported symbols from SCREAMER
  (:import-from :screamer 
   :known?-true :known?-false :variable-enumerated-domain
   :variable-enumerated-antidomain :assert!-equalv :assert!-constraint
   :assert!-memberv-internal :variable? :attach-noticer!
   )
  (:shadowing-import-from :screamer 
   :defun :multiple-value-bind :y-or-n-p
   )
  )

(in-package :screamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prevents the production of warnings as a result of loading this patch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+allegro
(setq excl:*redefinition-warnings* 
  (remove :operator excl:*redefinition-warnings*))

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This patch enables the search for solutions which are objects

;;; Redefine this function so that 'solution' labels constraint
;;; variables found in objects as well as in conses
;;; value-of has already been applied to x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Determines whether a variable is a standard CLOS object or not
(defmacro objectp (var)
  `(typep ,var 'standard-object)
  )


(defun variables-in (x &aux slots)
  (typecase x
   (cons
    (append (variables-in (value-of (car x))) (variables-in (cdr x))))
   (standard-object
    (setq slots (slot-names-of x))
    (append (variables-in (mapcar #'(lambda(y) (slot-value x y)) slots))) 
    )
   (array
    (get-array-variables x)
    )
   (variable (list x))
   (t nil)
   )
  )

;;; Deprecated version 
;(defun variables-in (x &aux slots)
;  (cond
;   ((consp x)
;    (append (variables-in (value-of (car x))) (variables-in (cdr x))))
;   ((objectp x)
;    (setq slots (slot-names-of x))
;    (append (variables-in (mapcar #'(lambda(y) (slot-value x y)) slots))) 
;    )
;   ((arrayp x)
;    (get-array-variables x)
;    )
;   ((screamer::variable? x) (list x))
;   (t nil)
;   )
;  )

;;; This should collect the values of all cells of a multi-dimensional
;;; array.
;;; Acc is an accumulator for collecting the values of the cells

(defun get-array-variables (array)
  (do* (
	(dims (array-dimensions array))
	(len (length dims))
	(copydims (make-list len :initial-element 0))
	(acc nil)
	(brand-new t)
	)
      
      ((and (every #'zerop copydims) (not brand-new)) acc)

    ;; For some reason SCREAMER is order-sensitive, so I need to use
    ;; append rather than cons
    (setq acc (nconc acc (list (apply #'aref (cons array copydims)))))
    (setq copydims (milometer copydims dims))
    (setq brand-new nil)
    )
  )

;;; This function increments the least significant digit in m
;;; If the digit then equals the base (maximum) given by the
;;; respective element in maxima, it is reset to zero and the
;;; next significant digit is incremented.
;;; USER(960): (milometer '(1 2) '(10 10))
;;; (1 3)
;;; USER(952): (milometer '(2 3 4) '(5 5 5))
;;; (2 4 0)
;;; USER(959): (milometer '(4 4 4) '(5 5 5))
;;; (0 0 0)

(defun milometer (m maxima)
  (when (null m) (return-from milometer nil))
  (when (not (= (length m) (length maxima)))
    (error "2 lists supplied to milometer must be the same length")
    )
  (let (
	(c m)
	)
    ;; Changed append to nconc 8/7/00 
    (setq c (nconc (butlast c) (list (1+ (car (last c))))))
    (if (= (car (last c)) (car (last maxima)))
	(append (milometer (butlast m) (butlast maxima)) '(0))
      c
      )
    )
  )

;;; This function is also used by solution to return the values
;;; found by the search. If objects were explored by the search
;;; NEW instances of the same type are generated and returned.

(defun apply-substitution (x &aux retobj)
  (let ((val (value-of x)))
    ;; Changed from a cond to a typecase, 8/7/00
    (typecase val
      (cons
       (cons (apply-substitution (car val)) (apply-substitution (cdr val)))
       )
      (standard-object
       (setq retobj (make-instance (class-name (class-of val))))
       (copy-slots val retobj)
       retobj 
       )
      (array
       (setq retobj (make-array (array-dimensions val)))
       (copy-cells val retobj)
       retobj
       )
      (t val)
      )
    )
  )

;;; Used by apply-substitution

(defun copy-slots (from to)
  (declare (standard-object from to))
  (dolist (s (slot-names-of from))
     (setf (slot-value to s) (value-of (slot-value from s)))
  )
)      

(defun copy-cells (from to)
  (do* (
	(dims (array-dimensions from))
	(len (length dims))
	(copydims (make-list len :initial-element 0))
	(brand-new t)
	)
      
      ((and (every #'zerop copydims) (not brand-new)) to)

    (setf (apply #'aref (cons to copydims)) (value-of (apply #'aref (cons from copydims))))
    (setq copydims (milometer copydims dims))
    (setq brand-new nil)
    )
  )

;;; This version of funcallv uses ground? to test the boundness of its arguments
;;; instead of bound?

(defun funcallgv (f &rest x)
 (let ((f (value-of f)))
  (if (variable? f)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
  (unless (functionp f)
   (error "The first argument to FUNCALLV must be a deterministic function"))
  (if (every #'ground? x)
      (apply f (mapcar #'value-of x))
      (let ((z (make-variable)))
       (assert!-constraint
	#'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
       (dolist (argument x)
	(attach-noticer!
	 #'(lambda ()
	    (if (every #'ground? x)
		(assert!-equalv z (apply f (mapcar #'value-of x)))))
	 argument))
       z))))


(defun slot-names-of (obj)
  (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
	  (clos::class-slots (class-of obj))
	  )
  )

;;; The function definition of objects-equal has been moved to gen_util.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF PATCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :screamer+)

;;; A variable holding the number of the SCREAMER+ version
(defvar *screamer+-version* "0.1")

;;; When propagating values, sometimes the derived enumerated domains can
;;; have very big domain sizes. This variable sets a limit on the size of
;;; propagated enumerated domains to prevent escalation.
(defvar *enumeration-limit* 100)

;;; A shorthand
(defmacro setq-domains (vars vals &aux (res nil))
  ;; Replaced append with an nconc 8/7/00
  (dolist (var vars) (setq res (nconc (list var vals) res)))
  (cons 'setq res))

;;; An extension to the SCREAMER variable definition extends the allowable
;;; types

(defstruct (variable+ 
	    (:print-function print-variable+)
	    (:include screamer::variable)
	    (:constructor make-variable+)
	    )
  ;; Stores the type of the variable if known
  nonnumber-type
  )

(defun make-variable (&optional (name nil name?))
 (let ((variable
	(make-variable+ :name (if name? name (incf screamer::*name*)))
	))
   (setf (variable+-value variable) variable)
   
   variable)
 )

(defun print-variable+ (x stream print-level)
  (declare (ignore print-level))
;  (format stream "[~S]" (screamer::variable-name x))

 (let ((x (value-of x)))
  (cond
   ((variable? x)
    (if (and (not (equal (variable-enumerated-domain x) t))
	     (not (null (variable-enumerated-antidomain x))))
	(error "This shouldn't happen"))
    (format stream "[~S" (screamer::variable-name x))
    (format stream "~A"
	    (cond 
	     ((variable-type-known? x) (format nil " ~(~a~)" (variable-get-type x)))
	     ((screamer::variable-boolean? x) " Boolean")
	     ((screamer::variable-integer? x) " integer")
	     ((screamer::variable-real? x)
	      (if (screamer::variable-noninteger? x) " noninteger-real" " real"))
	     ((screamer::variable-number? x)
	      (cond ((screamer::variable-nonreal? x) " nonreal-number")
		    ((screamer::variable-noninteger? x) " noninteger-number")
		    (t " number")))
	     ((screamer::variable-nonnumber? x) " nonnumber")
	     ((screamer::variable-nonreal? x) " nonreal")
	     ((screamer::variable-noninteger? x) " noninteger")
	     (t "")))
    (if (screamer::variable-real? x)
	(if (screamer::variable-lower-bound x)
	    (if (screamer::variable-upper-bound x)
		(format stream " ~D:~D"
			(screamer::variable-lower-bound x) (screamer::variable-upper-bound x))
		(format stream " ~D:" (screamer::variable-lower-bound x)))
	    (if (screamer::variable-upper-bound x)
		(format stream " :~D" (screamer::variable-upper-bound x)))))
    (if (and (not (equal (screamer::variable-enumerated-domain x) t))
	     (not (screamer::variable-boolean? x)))
	(format stream " enumerated-domain:~S"
		(screamer::variable-enumerated-domain x)))
    (if (not (null (screamer::variable-enumerated-antidomain x)))
	(format stream " enumerated-antidomain:~S"
		(screamer::variable-enumerated-antidomain x)))
    (format stream "]"))
    (t (format stream "~S" x))))

    )

(defmethod variable-type-known? ((x variable+))
  (not (null (variable+-nonnumber-type x)))
  )

(defmethod variable-get-type ((x variable+))
  (variable+-nonnumber-type x)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This macro added by Simon White 23/9/98 to aid robustness
;;; The idea is that any LISP forms are supplied and if they cause
;;; an error a warning is produced instead of diving straight into the
;;; LISP debugger.
;;;
;;; eg.
;;; > (carefully (+ 1 2))
;;; 3
;;; > (carefully (skjfksdj))
;;; Warning: (SKJFKSDJ ) failed
;;; NIL
;;; > (carefully (+ 'junk 1))
;;; Warning: (+ 'JUNK 1 ) failed
;;; NIL
;;; >
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro carefully (&body forms)
  `(let (retval)
     
     (setq retval (carefully-evaluate ,(car forms)))
     (if (> ,(length forms) 1)
	 (carefully ,@(rest forms))
       retval)
     )
  )

(defmacro carefully-evaluate (form)
  `(let (error-found)
     (setq error-found (multiple-value-list (ignore-errors ,form)))
     (if (and (= (length error-found) 2)
	      (null (car error-found))
	      (typep (second error-found) (find-class 'error))
	      )
	 (warn "~s failed" (quote ,form))
       (apply #'values error-found)
       )
     )
  )


(defun slot-names-of (obj)
  (mapcar #'(lambda(x) (slot-value x 'CLOS::NAME))
	  (clos::class-slots (class-of obj))
	  )
  )

;;; The function definition of objects-equal has been moved to gen_util.lisp

;;; Determines whether a variable is a standard CLOS object or not
(defun objectp (var)
  (if (typep var 'standard-object) t nil)
  )

;;; Checks to see if the enumerated domain slot is a non-empty list
(defun enumerated-domain-p (var)
  (and (variable-enumerated-domain var)
       (listp (variable-enumerated-domain var))
       )
  )

;;; Checks to see if the enumerated antidomain slot is a non-empty list
(defun enumerated-antidomain-p (var)
  (and (variable-enumerated-antidomain var)
       (listp (variable-enumerated-antidomain var))
       )
  )

;;; A simple version of eqv for atoms
;;; PROPAGATION PROPERTIES: as for funcallv

(defun eqv (x y) (funcallv #'eq x y))

;;; This was not included in the standard SCREAMER distribution
;;; The following generates the truth table for implication
;;; > (setq p (a-booleanv))
;;; [3558 Boolean]
;;; > (setq q (a-booleanv))
;;; [3559 Boolean]
;;; > (setq r (a-booleanv))
;;; [3560 Boolean]
;;; > (assert! (equalv r (impliesv p q)))
;;; NIL
;;; > (all-values (solution (list p '=> q 'is r) (static-ordering #'linear-force)))
;;; ((T => T IS T) (T => NIL IS NIL) (NIL => T IS T) (NIL => NIL IS T))
;;;
;;; PROPAGATION PROPERTIES: as for other logical functions

(defun impliesv (p q)
  (orv (notv p) q)
  )

;;; A useful macro for assignment of SCREAMER constraint variables
;;; Returns true if the assignment succeeds, and evaluates retval if it fails
;;; I have taken care for the original variable to retain its value
;;; if the assignment fails, and for 'value' to be evaluated only once
;;;
;;; PROPAGATION PROPERTIES: as for equalv, except that no propagation occurs
;;; if the equality assertion fails.

(defmacro make-equal (var value &optional (retval '(fail)))
  `(if (possibly? (equalv ,var ,value))
       (progn
	 (assert! (equalv ,var ,value))
	 (values ,var)
	 )
     (progn
       (warn "(make-equal ~s ~s) failed~%  ~s = ~s; ~s = ~s"
	     (quote ,var) (quote ,value)
	     (quote ,var) ,var
	     (quote ,value) ,value)
       (values ,retval)
       )
     )
  )


(defmacro ifv (condition exp1 &optional (exp2 nil))
  ;; If the condition is bound then there is no need to create additional
  ;; constraint variables
  `(let ((c ,condition))
     (assert! (booleanpv c))
     (if (bound? c)
	 (if (known?-true c)
	     ,exp1
	   ,exp2)
       (let (
	     (z (make-variable))
	     )
      
	 (attach-noticer!
	  #'(lambda()
	      ;; Change 31/8/99, SAW
	      ;; Used to check that z is not bound too, but that seemed wrong
	      (when (bound? c)
		(if (equal (value-of c) t)
		    (assert!-equalv z ,exp1)
		  (assert!-equalv z ,exp2)
		  )
		)
	      )
	  c
	  )
	 z)
      
       )
     )
  )

#|
(defun ifv-internal (condition e1 &optional (e2 t))
  (let (
	(con condition)
	(z (make-variable))
	)
      
     (attach-noticer!
      #'(lambda()
	  (when (eq (value-of con) t)
	    (assert!-equalv z (eval e1))
	    )
	  (when (null (value-of con))
	    (assert!-equalv z (eval e2))
	    )
	  )
      con
      )
     z)
  )

;;; This version of ifv seems to work!
;;; More straightforward versions didn't work, but I don't know why
(defmacro ifv (c x &optional (y t y?))
  (if y?
      `(ifv-internal ,c (funcall #'quote-up (value-of ,x)) (funcall #'quote-up (value-of ,y)))
    `(ifv-internal ,c (funcall #'quote-up (value-of ,x)))
    )
  )

(defun quote-up (f &rest args)
  (cond
   ((= (length args) 0)
    `(quote ,f))
   (t
    `(,f ,@args))
   )
  )



(defmacro ifv (condition exp1 &optional (exp2 t))
  ;; If the condition is bound then there is no need to create additional
  ;; constraint variables
  `(let (
	 (con ,condition)
	 (z (make-variable))
	 )
      
     (attach-noticer!
      #'(lambda()
	  ;; Change 31/8/99, SAW
	  ;; Used to check that z is not bound too, but that seemed wrong
	  (when (known?-true con)
	    (assert!-equalv z ,exp1)
	    )
	  (when (known?-false con)
	    (assert!-equalv z ,exp2)
	    )
	  )
      con
      )
     z)
  )


(defun restrict-true (val)
  (if (variable? val)
      (screamer::restrict-true! val)
    (if val t (fail))
    )
  )

(defmacro ifv (condition exp1 &optional (exp2 t))
  ;; If the condition is bound then there is no need to create additional
  ;; constraint variables
  `(let (
	 (con ,condition)
	 (z (make-variable))
	 )
      
     (attach-noticer!
      #'(lambda()
	  ;; Change 31/8/99, SAW
	  ;; Used to check that z is not bound too, but that seemed wrong
	  (when (known?-true con)
	    (restrict-true (equalv z (value-of ,exp1)))
	    )
	  (when (known?-false con)
	    (restrict-true (equalv z (value-of ,exp2)))
	    )
	  )
      con
      )
     z)
  )

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: consv
;;;
;;; A function returning a variable constrained to be the cons of x and y 
;;; Since this function is so important I've tried to maximise the propagation
;;;
;;; Suppose z is constrained to be the cons of x and y; i.e., z == (cons x y)
;;; Then:
;;; - if y is bound at the time of function invocation, then z is immediately
;;;   bound to the cons of x and y (regardless of whether x is bound).
;;;
;;; - if z becomes bound, then the values of x and y are derived.
;;;
;;; - if x and y become bound, then z is derived
;;;
;;; - if z has an enumerated domain, then the appropriate enumerated domains
;;;   are propagated to x and y.
;;;
;;; - if x becomes bound and y has an enumerated domain, then the possible
;;;   values are propagated to z
;;;
;;; - if x has an enumerated domain and y has an enumerated domain, then
;;;   possible values for z are computed, subject to the size of the 
;;;   cross-product being less than *enumeration-limit*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consv (x y)
  (if (and (bound? x) (bound? y))
      (cons (value-of x) (value-of y))
    (let (
	  (z (make-variable))
	  mem
	  )
      
      (screamer::attach-noticer!
       #'(lambda()
           (when (bound? z)
              (assert! (equalv x (car (value-of z))))
              (assert! (equalv y (cdr (value-of z))))
              )
	   (when (and (not (bound? z))
		      (enumerated-domain-p z)
		      )
	     (assert! (memberv x (mapcar #'car (variable-enumerated-domain z))))
	     (assert! (memberv y (mapcar #'cdr (variable-enumerated-domain z))))
	     )
           )
       z)

      (screamer::attach-noticer!
       #'(lambda()
	   (when (and (bound? x)
		      (not (bound? y))
		      (enumerated-domain-p y)
		      )
	     (setq mem (mapcar #'(lambda(g) (cons (value-of x) g)) (variable-enumerated-domain y)))
	     (assert! (memberv z mem))
	     )
	   (when (and (bound? x)
		      (bound? y)
		      )
	     (assert! (equalv z (cons (value-of x) (value-of y))))
	     )
	   )
       x)
      
      (if (bound? y)
	  (assert! (equalv z (cons (value-of x) (value-of y))))
	(screamer::attach-noticer!
	 #'(lambda()
	     (when (and (not (bound? y)) (enumerated-domain-p y))
	       (if (bound? x)
		   (assert! (memberv z (mapcar #'(lambda(g) (cons (value-of x) g)) (variable-enumerated-domain y))))
		 (when (and (enumerated-domain-p x) 
			    ;; put a limit on the propagation
			    (< (* (domain-size x) (domain-size y)) 
			       *enumeration-limit*)
			    )
		   (assert! (memberv z (funcross-product #'cons (variable-enumerated-domain x) (variable-enumerated-domain y))))
		   )
		 )
	       )
	     (when (and (not (bound? x))
			(bound? y)
			(enumerated-domain-p x)
			)
	       (assert! (memberv z (funcross-product #'cons (variable-enumerated-domain x) (list (value-of y)))))
	       )
	     (when (and (bound? x) (bound? y))
	       (assert! (equalv z (cons (value-of x) (value-of y))))
	       )
	     )
	 y)
	)
      
      z)
    )
  )

;;; The next two functions are used by consv

(defun funcross-product (f x y)
  (cond 
   ((null x) nil)
   (t (append (funcross-pair f (car x) y) (funcross-product f (cdr x) y)))
   )
  )

;;; x is a single item and y a list of lists
;;; the function returns a list of lists in which x is consed onto the front
;;; of every element of y

(defun funcross-pair (f x y)
  (mapcar #'(lambda(g) (funcall f (value-of x) g)) y)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: carv
;;;
;;; Suppose z is constrained to be the car of x; i.e. z == (car x)
;;; Then:
;;; - If x is bound at the time of function invocation, z becomes bound.
;;;
;;; - As soon as x becomes bound to be a cons, z becomes bound (if x is not a
;;;   cons then the constraint fails.)
;;;
;;; - If x has an enumerated domain, then the possible values are propagated
;;;   to z.
;;;
;;; - If z becomes bound and x is unbound, then x is constrained to be the
;;;   cons of z and some unknown tail.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun carv (el)
  (if (bound? el)
      (car (value-of el))
    (let (
	  (z (make-variable))
	  )

;;; I need to deal with the antidomains sometime too!
;    (when (and (not (bound? el)) (enumerated-antidomain-p el))
;      (assert! (notv (memberv z (mapcar #'car (variable-enumerated-antidomain el)))))
;      )

      (screamer::attach-noticer!
       #'(lambda()
	   ;; el is not bound but it does have an enumerated domain
	   (when (and (not (bound? el)) (enumerated-domain-p el))
	     (assert! (memberv z (mapcar #'car (variable-enumerated-domain el))))
	     )

	   ;; check that it is a cons before trying to take the car
	   (when (and (bound? el)
		      (or (listp (value-of el))
			  (consp (value-of el)))
		      )
	     (assert! (equalv z (car (value-of el))))
	     )
	   )
       el)

      (screamer::attach-noticer!
       #'(lambda()
	   (when (and (bound? z) (not (bound? el)))
	     (assert! (equalv (value-of el) (consv (value-of z) (make-variable))))
	     )
	   )
       z)
      
      z) ; the variable z is returned
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: cdrv
;;;
;;; Suppose z is constrained to be the cdr of x; i.e., z == (cdr x)
;;; Then..
;;; - If x is bound at the time of function invocation, z becomes bound.
;;;
;;; - If x has an enumerated domain, then the cdrs of the enumerated domain
;;;   values are propagated to z.
;;;
;;; - If the cdr of x becomes bound, then z becomes bound.
;;;
;;; - If z becomes bound, then x is constrained to be the cons of some unknown
;;;   constraint variable with z.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cdrv (el)
  (if (bound? el)
      (cdr (value-of el))
    (let (
	  (z (make-variable))
	  )
      
      (screamer::attach-noticer!
       #'(lambda()
	   ;; propagate possible values
	   (when (and (not (bound? el)) (enumerated-domain-p el))
	     (assert! (memberv z (mapcar #'cdr (variable-enumerated-domain el))))
	     )
	   
	   (when (and (or (listp (value-of el))
			  (consp (value-of el))
			  )
		      (bound? (cdr (value-of el)))
		      )
	     (assert! (equalv z (cdr (value-of el)))))
	   )
       el)
      (screamer::attach-noticer!
       #'(lambda()
	   (when (and (bound? z) (not (bound? el)))
	     (assert! (equalv (value-of el) (consv (make-variable) (value-of z))))
	     )
	   )
       z)
      
      z
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some variables for constraining values in positions of lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro firstv (el) `(carv ,el))
(defmacro secondv (el) `(nthv 1 ,el))
(defmacro thirdv (el) `(nthv 2 ,el))
(defmacro fourthv (el) `(nthv 3 ,el))

(defmacro restv (el) `(cdrv ,el))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: nthv
;;;
;;; This function creates a variable which is constrained to be the nth value
;;; of some list. The first element of the list has index 0
;;;
;;; Suppose z == (nth n x)
;;; Then...
;;; - If both n and x are bound, z is derived.
;;;
;;; - As soon as x becomes bound, its nth value is asserted to be equal to z
;;;
;;; - If x is a list, and the nth value of x has an enumerated domain, then
;;;   those domain values are propagated to z.
;;;
;;; - If x itself has an enumerated domain, and every nth value of the
;;;   enumerated domain is bound, then those values are computed and 
;;;   propagated to the enumerated domain of z.
;;;
;;; - If x and z are bound, but n is unbound, then the enumerated domain of n 
;;;   is computed to be one of the indices for which (nth n x) = z
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nthv (n el)
  (if (and (bound? n) (bound? el))
      (nth (value-of n) (value-of el))
    (let (
	  (z (make-variable))
	  )
      
      (screamer::attach-noticer!
       #'(lambda()
	   (when (bound? n)
	     ;; propagate possible values
	     ;; the list is bound and the nth element has an enumerated domain
	     (when (and (listp (value-of el))
			(not (bound? (nth (value-of n) (value-of el))))
			(enumerated-domain-p (nth (value-of n) (value-of el)))
			)
	       (assert!-memberv-internal z (variable-enumerated-domain (nth (value-of n) (value-of el))))
	       )
	     ;; the list is unbound, but the nth elements in the enumerated 
	     ;; domain are all bound
	     (when (and (not (bound? el))
			(enumerated-domain-p el)
			(every #'(lambda(x) (and (listp x) (bound? (nth (value-of n) x)))) 
			       (variable-enumerated-domain el))
			)
	       (assert!-memberv-internal z (mapcar #'(lambda(x) (nth (value-of n) x)) (variable-enumerated-domain el)))
	       )
	     ;; propagate the actual value
	     (when (listp (value-of el))
	       (assert!-equalv z (nth (value-of n) (value-of el)))
	       )
	     )
	   )
       (cons n (value-of el)))
      (screamer::attach-noticer!
       #'(lambda()
	   (when (and (bound? el)
		      (bound? n)
		      (or (bound? z) 
			  (and (not (bound? z)) (enumerated-domain-p z))
			  )
		      )
	     (assert!-equalv (nth (value-of n) (value-of el)) z)
	     )
	   ;; Added this part 18/3/99
	   ;; It constrains the domain of indices for n when z is already bound
	   (when (and (bound? el) (not (bound? n)) (bound? z))
	     (do* (
		   (index 0 (1+ index))
		   (current (nth index el) (nth index el))
		   (acc nil)
		   )
	       ((= index (length el))
		(assert!-memberv-internal n acc))
	       (if (bound? current)
		   (when (equal (value-of (nth index el)) (value-of z))
		     (setq acc (nconc acc (list index)))
		     )
		 (if (not (enumerated-domain-p current))
		     (setq acc (nconc acc (list index)))
		   (when (not (known?-false (memberv z (variable-enumerated-domain current))))
		     (setq acc (nconc acc (list index)))
		     )
		   )
		 )
	       )
	     )
	   )
       z)
      
      z
      )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-listv
;;;
;;; This function creates a list constrained to be of length n
;;; Suppose z is constrained to be the make-listv of n; 
;;; i.e., z == (make-listv n)
;;; Then...
;;; - If n is already bound at function invocation time, then a list of length
;;;   n is computed (with the given initial element)
;;;
;;; - As soon as n becomes bound, z is bound to a list of length n
;;;
;;; - If z becomes bound first, then n becomes bound to the length of z
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-listv (n &key (initial-element '(make-variable)) &aux (acc nil))
  (if (bound? n)
      (progn
	(dotimes (c (value-of n))
	  ;; Changed append to nconc 8/7/00
	  (setq acc (nconc acc (list (eval initial-element) )))
	  )
	acc
	)
    (let (
	  (z (make-variable))
	  (done nil)
	  )

      (screamer::attach-noticer!
       #'(lambda()
	   (let ((acc nil))
	     (when (and (bound? n) (not done))
	       (setq done t)
	       (dotimes (c (value-of n))
		 ;; Changed append to nconc 8/7/00
		 (setq acc (nconc acc (list (make-variable))))
		 )
	       (assert! (equalv z acc))
	       )))
       n)
      
      (screamer::attach-noticer!
       #'(lambda()
	   (when (bound? z)
	     (assert! (equalv n (length (value-of z))))
	     )
	   )
       z)
      z
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: arefv
;;;
;;; This function returns the value contained in a constrained array at
;;; the given subscripts.
;;; It does not generate any new constraint variables, but may return
;;; a constraint variable if that is what is contained in the array.
;;;
;;; To assign a value to a cell in a constrained array, just use
;;; (assert! (equalv (arefv <array> <subscripts>) <value>))
;;;
;;; PROPAGATION PROPERTIES: As for applyv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arefv (array &rest subscripts)
  (applyv #'aref array subscripts)
  )

;;; This function is used by make-arrayv to generate the initial
;;; contents for an array of the given size, such that
;;; each cell is filled with an unbound constraint variable

(defun generate-dimension (dims &aux (acc nil))
  (cond
   ((numberp dims) (dotimes (c dims) (push (make-variable) acc)) acc)
   ((zerop (car dims)) nil)
   ((= (length dims) 1)
    (generate-dimension (car dims))
    )
   (t (cons 
       (generate-dimension (cdr dims))
       (generate-dimension (cons (1- (car dims)) (cdr dims)))
       )
      )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-arrayv
;;;
;;; This function returns a variable constrained to be an array of the
;;; given dimensions. The argument to this function need not be bound
;;; at invocation time. 
;;;
;;; Suppose z == (make-arrayv x);
;;; Then...
;;; - If the dimensions x are ground at function invocation time, an array 
;;;   of those dimensions are created.
;;; 
;;; - If the dimensions x become ground, an array is created
;;;
;;; Note that funcallv would not work because dimensions is a list, and I
;;; need to wait until all its elements are bound before creating the array;
;;; not just until the list is bound as funcallv and applyv would.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-arrayv (dimensions &key (element-type t))
  (let (z acc vald)

    (if (ground? dimensions)
	(progn
	   (setq vald (apply-substitution dimensions))
	   (setq acc (generate-dimension vald))
	   (setq z (make-array vald
			       :element-type element-type
			       :initial-contents acc
			       :adjustable t
			       )
		 ) ; possible return value
	   )
	(progn
	  (setq z (make-variable))
	  (attach-noticer!
	   #'(lambda()
	       (when (and (bound? dimensions) (every #'bound? (value-of dimensions)))
		 (setq vald (apply-substitution dimensions))
		 (setq acc (generate-dimension vald))
		 (assert! (equalv z 
				  (make-array vald
					      :element-type element-type
					      :initial-contents acc
					      :adjustable t
					      )
				  )
			  )
		 )
	       )
	   dimensions)
	  z) ; possible return value
	)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: appendv
;;;
;;; Creates a list constrained to be the append of two other lists
;;; Suppose z is constrained to be the append of x and y; 
;;; i.e., z == (append x y)
;;; Then...
;;; - If x and y become bound, z becomes bound.
;;;
;;; - If x and z become bound, y becomes bound.
;;;
;;; - If y and z become bound, x becomes bound.
;;;
;;; - If x and y have enumerated domains and the product of their domain sizes
;;;   is less than *enumeration-limit*, then the possible values are 
;;;   propagated to z.
;;;
;;; - If x has an enumerated domain and y is bound, then the possible values
;;;   are propagated to z.
;;;
;;; - If y has an enumerated domain and x is bound, then the possible values
;;;   are propagated to z.
;;;
;;; - If z is bound and x and y are unbound, then the possible domain values
;;;   for x and y are computed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun appendv (x y &rest r)
  (if (and (bound? x) (bound? y) (every #'bound? r))
      (apply #'append (cons (append (value-of x) (value-of y)) r))
    (let (
	  (z (make-variable))
	  noticer
	  )
      
      (setq noticer
	    #'(lambda()
		(cond
		 ;; x and y are both bound
		 ((and (bound? x) (bound? y))
		  (assert! (equalv z (append (value-of x) (value-of y))))
		  )
		 ;; x and z are both bound
		 ((and (bound? x) (not (bound? y)) (bound? z))
		  (assert! (equalv y (nthcdr (length (value-of x)) (value-of z))))
		  )
		 ;; y and z are both bound
		 ((and (not (bound? x)) (bound? y) (bound? z))
		  (assert! (equalv x (butlast (value-of z) (length (value-of y)))))
		  )
		 ;; Both x and y have enumerated domains
		 ((and (not (bound? x)) (enumerated-domain-p x)
		       (not (bound? y)) (enumerated-domain-p y)
		       (< (* (domain-size x) (domain-size y)) *enumeration-limit*)
		       )
		  (assert! (memberv z (funcross-product #'append 
							(variable-enumerated-domain x) 
							(variable-enumerated-domain y))
				    )
			   )
		  )
		 ;; x has an enumerated domain and y is bound
		 ((and (not (bound? x)) (enumerated-domain-p x) (bound? y))
		  (assert! (memberv z (funcross-product #'append
							(variable-enumerated-domain x)
							(list (value-of y)))
				    )
			   )
		  )
		 ;; x is bound and y has an enumerated domain
		 ((and (bound? x) (not (bound? y)) (enumerated-domain-p y))
		  (assert! (memberv z (funcross-product #'append
							(list (value-of x))
							(variable-enumerated-domain y))
				    )
			   )
		  )
		 
		 
		 ) ; cond
		) ; lambda
	    )
      (screamer::attach-noticer! noticer x)
      (screamer::attach-noticer! noticer y)
      
      (screamer::attach-noticer!
       #'(lambda()
	   (cond
	    ((and (bound? x) (not (bound? y)) (bound? z))
	     (assert! (equalv y (nthcdr (length (value-of x)) (value-of z))))
	     )
	    ((and (not (bound? x)) (bound? y) (bound? z))
	     (assert! (equalv x (butlast (value-of z) (length (value-of y)))))
	     )
	    ;; Both x and y have enumerated domains
	    ((and (not (bound? x)) (enumerated-domain-p x)
		  (not (bound? y)) (enumerated-domain-p y)
		  (< (* (domain-size x) (domain-size y)) *enumeration-limit*)
		  )
	     (assert! (memberv z 
			       (funcross-product #'append 
						 (variable-enumerated-domain x) 
						 (variable-enumerated-domain y)
						 )
			       )
		      )
	     )
	    ;; x has an enumerated domain and y is bound
	    ((and (not (bound? x)) (enumerated-domain-p x) (bound? y))
	     (assert! (memberv z
			       (funcross-product #'append
						 (variable-enumerated-domain x)
						 (list (value-of y))
						 )
			       )
		      )
	     )
	    ;; x is bound and y has an enumerated domain
	    ((and (bound? x)
		  (not (bound? y)) (enumerated-domain-p y)
		  )
	     (assert! (memberv z
			       (funcross-product #'append
						 (list (value-of x))
						 (variable-enumerated-domain y)
						 )
			       )
		      )
	     )
	    ) ; cond
	   
	   (when (and (not (bound? x)) (not (bound? y)) (bound? z)
		      (not r) ; there are only two arguments
		      (< (length (value-of z)) (1- *enumeration-limit*))
		      )
	     (do* (
		   (val (value-of z))
		   (n (length val) (1- n))
		   (fronts nil)
		   (backs nil)
		   )
		 
		 ((< n 0) 
		  (assert! (memberv x fronts))
		  (assert! (memberv y backs))
		  )
	       
	       (push (subseq val 0 n) fronts)
	       (push (subseq val n) backs)
	       )
	     )
	   )
       z)
      
      (if r 
	  ;; recursively apply the binary version so that any number of arguments
	  ;; can be supplied
	  (apply #'appendv (cons z r)) 
	z ; must return the new variable
	)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: mapcarv
;;;
;;; As soon as the arguments el become bound, the variable returned by this 
;;; function becomes bound to the mapcar of the function f applied to el
;;;
;;; Suppose z is constrained to be the result of applying f to successive
;;; elements of x, i.e., z == (mapcarv f x1 ... xn)
;;; Then...
;;; - If f and x1...xn  are bound at function invocation time, the
;;;   value of z is computed and returned.
;;;
;;; - When the lists x1...xn become bound, z becomes bound to the mapcar
;;;   of f applied to those agruments. Note that the elements of x1...xn
;;;   do not have to become bound, only the lists themselves. Since f is
;;;   a constraint function, rather than a Common LISP function it deals
;;;   with the case when the elements are unbound.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapcarv (f &rest el)
  (if (and (bound? f) (every #'bound? el))
      (apply #'mapcar (cons (value-of f) (mapcar #'value-of el)))
    (let (
	  (z (make-variable))
	  )
      
      (dolist (d el)
	;; Perhaps I should just assert the lengths to be equal, rather than
	;; creating a new list of constraint variables just for this purpose?
        (when (and (bound? d) (not (bound? z)))
	  (assert! (equalv z (make-listv (lengthv d))))
	  )
        (screamer::attach-noticer!
         #'(lambda()
             ;; check that it is a list 
             (when (and (every #'bound? el) (bound? f)) 
	       (assert! (equalv z (apply #'mapcar 
					 (cons (value-of f) (mapcar #'value-of el)))))
	       )
             )
         d)
        )
      z) ; the variable z is returned
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: maplistv
;;;
;;; Suppose z == (maplist f x1 ... xn)
;;; Then...
;;; - If f and x1...xn  are bound at function invocation time, the
;;;   value of z is computed and returned.
;;;
;;; - When the lists x1...xn become bound, z becomes bound to the mapcar
;;;   of f applied to those agruments. Note that the elements of x1...xn
;;;   do not have to become bound, only the lists themselves. Since f is
;;;   a constraint function, rather than a Common LISP function it deals
;;;   with the case when the elements are unbound.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maplistv (f &rest el)
  (if (and (bound? f) (every #'bound? el))
      (apply #'maplist (cons (value-of f) (mapcar #'value-of el)))
    (let (
	  (z (make-variable))
	  )
      
      (dolist (d el)
        (when (and (bound? d) (not (bound? z)))
	  (assert! (equalv z (make-listv (lengthv d))))
	  )
        (screamer::attach-noticer!
         #'(lambda()
             ;; check that it is a list 
             (when (and (every #'bound? el) (bound? f)) 
	       (assert! (equalv z (apply #'maplist 
					 (cons (value-of f) (mapcar #'value-of el)))))
	       )
             )
         d)
        )
      z) ; the variable z is returned
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: constraint-fn
;;;
;;; This function takes a function which works only on bound arguments, and
;;; returns a similar function which works with either bound arguments or
;;; arguments which are constraint variables.
;;;
;;; If a function already exists with the same name as the supplied function
;;; appended with the suffix 'v', then this function is returned. Otherwise
;;; a lambda function is constructed and returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun constraint-fn (f)
  (let (
	;; Recover the name of the function if possible
	(fn-name (third (multiple-value-list (function-lambda-expression f))))
	cfn-name
	)
    ;(format t "Function name is ~a~%" fn-name)
    (setq cfn-name (read-from-string (format nil "~av" fn-name) nil nil))
    ;(format t "Constraint function name is ~a~%" cfn-name)
    (if (fboundp cfn-name)
	(symbol-function cfn-name)
      (function
       (lambda(&rest args)
	 (value-of (applyv (value-of f) args))
	 )
       )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: funcallinv
;;;
;;; This is a propagation-enhanced version of funcallv
;;; Suppose z == (funcallinv f g x1 ... xn), where g is the inverse of f
;;; Then...
;;; - If f is bound and every element of x1...xn is ground, then the function f
;;;   is applied to those elements and the result is returned.
;;; 
;;; - As soon as all of x1...xn are bound, then f is applied to it, as 
;;;   for funcallv.
;;;
;;; - If the inverse function g is non-nil, then x1...xn is constrained to be 
;;;   the result of applying g to z. Therefore as soon as z becomes bound, 
;;;   x1...xn would also become bound.
;;;
;;; - If the function f has a single argument x1, and x1 has an enumerated
;;;   domain, then those values are propagated to the enumerated domain of z
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun funcallinv (f inverse &rest el)
  (if (and (bound? f) (every #'ground? el))
      (apply (value-of f) (mapcar #'apply-substitution el))
    (let (
	  (z (make-variable))
	  )
      
      (assert! (equalv z (applyv f el)))
      (when inverse (assert! (equalv el (listv (funcallv inverse z)))))
      
      (when (= (length el) 1)
      (screamer::attach-noticer!
       #'(lambda()
	   (when (and (not (bound? (car el)))
		      (enumerated-domain-p (car el))
		      )
	     (assert! (memberv z (mapcar f (variable-enumerated-domain (car el)))))
	     )
	   )
       el)
      )
      
      z
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: listv (deprecated)
;;; Returns a variable which is constrained to be a list of the arguments.
;;; The variable is constructed from the arguments, whether they are bound
;;; or not.
;;;
;;; *** This is no different to list !! ***
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun listv (&rest args)
  (apply #'list args)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions: listpv, conspv, symbolpv, stringpv
;;;
;;; Some functions for constraining elements to be of a specific type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro listpv (el) `(typepv ,el 'list))
(defmacro conspv (el) `(typepv ,el 'cons))
(defmacro symbolpv (el) `(typepv ,el symbol))
(defmacro stringpv (el) `(typepv ,el 'string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: typepv
;;;
;;; Constrains a variable to indicate whether an element is of the supplied
;;; type.
;;; Suppose z == (typepv x type)
;;; Then...
;;; - If x and type are bound at function invocation time, the result is 
;;;   computed and returned.
;;;
;;; - As soon as x and type are known, their value is propagated to z.
;;;
;;; - If z is true and type is bound, then the type of x is recorded for
;;;   future use.
;;; 
;;; - If z is true and type is bound, and x has an enumerated domain, then the
;;;   domain of x is reduced to conform to the type specification.
;;; 
;;; Note that once the (generalised) type of a variable is set, it cannot be
;;; changed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun typepv (el type)
  (cond
   ((and (bound? el) (bound? type))
    (typep (value-of el) (value-of type))
    )
   ((variable-type-known? el)
    ;; Returns whether the known type of el is a subtype of (or equal to) type
    (nth-value 0 (subtypep (variable-get-type el) type))
    )
   (t
    (let ((z (make-variable))
	  noticer
	  )
      
      (assert! (equalv z (funcallv #'typep el type)))
      (setq noticer
	#'(lambda()
	    (when (and (known?-true z)
		       (bound? type)
		       (not (bound? el))
		       )
	      
	      (when (variable? el)
		(assert (subtypep (value-of type) t)) ; an error if not true
		(setf (variable+-nonnumber-type el) (value-of type))
		)
	      
	      (when (enumerated-domain-p el)
		(assert! 
		 (memberv el
			  (remove-if-not
			   #'(lambda(x) (or (typep x type) (variable? x)))
			   (variable-enumerated-domain el)
			   )
			  )
		 )
		)
	      )
	    )
	)
      (screamer::attach-noticer! noticer el)
      (screamer::attach-noticer! noticer z)
      (screamer::attach-noticer! noticer type)
      
      z)
    )
   )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions: a-listv, a-consv, a-symbolv, a-stringv
;;;
;;; Some functions & macros for generating variables of a specific type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro a-listv () '(a-typed-varv 'list))
(defmacro a-consv () '(a-typed-varv 'cons))
(defmacro a-symbolv () '(a-typed-varv 'symbol))
(defmacro a-stringv () '(a-typed-varv 'string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: a-typed-varv
;;;
;;; Returns a constraint-variable constrained to be of the specified type.
;;; Suppose z == (a-typed-varv type),
;;; Then z must become bound by other means, but if it becomes bound to
;;; something other than the specified type, a failure is generated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun a-typed-varv (type)
  (let (
	(z (make-variable))
	)

    (assert! (typepv z type))
    z
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: formatv
;;; Generates "Format daemons"!
;;; Suppose z == (formatv stream c-string arg1 arg2 ... argn)
;;; Then...
;;; - If the arguments to formatv are all bound at function invocation time,
;;;   then format is called as normal.
;;;
;;; - Provided z is non-nil, as soon as the arguments become bound, format
;;;   is applied and the results output to the given stream.
;;;
;;; - If z becomes bound to nil, then the format daemon is switched off
;;;   and no output is ever produced, even if the arguments subsequently
;;;   become bound.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun formatv (&rest args)
  (if (every #'ground? args)
      (apply #'format (mapcar #'(lambda(x)
				  (if (stringp x)
				      x
				    (apply-substitution  x)
				    ))
			      args))
    (let (
	  (z (make-variable))
	  (daemon-active t) ; used for inter-noticer communication
	  retval
	  )
      
      (dolist (a args)
	(screamer::attach-noticer!
	 #'(lambda()
	     (when (and (every #'ground? args) 
			(not (bound? z))
			daemon-active
			)
	       (setq daemon-active nil)
	       (setq retval (apply #'format args))
	       (assert! (equalv z retval))
	       )
	     )
	 a)
	)


      (screamer::attach-noticer!
       #'(lambda()
	   (when (and (bound? z) (null (value-of z)))
	     (setq daemon-active nil)
	     )
	   )
       z)
      
      z)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions: everyv, somev, noteveryv, notanyv
;;;
;;; Creates a variable which applies the constraint function f to each of
;;; v as soon as v is bound. The variable becomes bound
;;; to nil if the test is nil for any v. 
;;; Otherwise, a non-nil value is returned.
;;; Note that v does not have to be bound at the time of function invocation
;;; 
;;; Suppose z == (everyv f x);
;;; Then...
;;; - When the list x becomes bound, z is constrained to the logical AND
;;;   of f applied to each of the elements of x.
;;;
;;; - If z is known to be true, and x is bound, then f is applied to each
;;;   element of x and its result asserted to be true.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun everyv (f v)
  (let (
	(z (a-booleanv))
	val
	(v (value-of v))
	)

    (screamer::attach-noticer!
     #'(lambda()
	 (when (bound? v)
	   (setq val nil)
	   (dolist (x (value-of v))
	     (push (funcall f x) val)
	     )
	   (assert!-equalv z (apply #'andv val))
	   )
	 )
     v)

    (screamer::attach-noticer!
     #'(lambda()
	 (when (and (known?-true z) (bound? v))
	   (dolist (x (value-of v))
	     (assert! (funcall f x))
	     )
	   )
	 )
     z)
    z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: notanyv
;;; Suppose z == (notanyv f x);
;;; Then...
;;; - When the list x becomes bound, z is constrained to the logical AND
;;;   of the negation of f applied to each of the elements of x.
;;;
;;; - If z is known to be true, and x is bound, then f is applied to each
;;;   element of x and its result asserted to be false.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notanyv (f v)
  (let (
	(z (a-booleanv))
	val
	)

    (screamer::attach-noticer!
     #'(lambda()
	 (when (bound? v)
	   (setq val nil)
	   (dolist (x (value-of v))
	     (push (notv (funcall f x)) val)
	     )
	   (assert! (equalv z (apply #'andv val)))
	   )
	 )
     v)

 (screamer::attach-noticer!
     #'(lambda()
	 (when (and (known?-true z) (bound? v))
	   (dolist (x (value-of v))
	     (assert! (notv (funcall f x)))
	     )
	   )
	 )
     z)    
    z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: somev
;;; Suppose z == (somev f x);
;;; Then...
;;; - z is constrained to be the logical OR of f applied to each element of x.
;;;
;;; - If z is known to be false (nil), then NOT f is applied to each of x 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun somev (f v)
  (let (
	(z (a-booleanv))
	disjunction
	)

    (screamer::attach-noticer!
     #'(lambda()
	 (when (bound? v)
	   (setq disjunction nil)
	   (dolist (x (value-of v))
		   (push (funcall f x) disjunction)
		   )
	   (assert! (equalv z (apply #'orv disjunction))) ; changed 28/2/99
	   )
	 )
     v)

    (screamer::attach-noticer!
     #'(lambda()
	 (when (and (known?-false z) (bound? v))
	   (dolist (x (value-of v))
	     (assert! (notv (funcall f x)))
	     )
	   )
	 )
     z)

    z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: noteveryv
;;; Suppose z == (noteveryv f x);
;;; Then...
;;; - z is constrained to be the logical AND of NOT f applied to each 
;;;   element of x.
;;;
;;; - If z is known to be false (nil), then f is applied to each of x 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noteveryv (f v)
  (let (
	(z (a-booleanv))
	conjunction
	)

    (screamer::attach-noticer!
     #'(lambda()
	 (when (bound? v)
	   (setq conjunction nil)
	   (dolist (x (value-of v))
		   (push (funcall f x) conjunction)
		   )
	   (assert! (equalv (notv z) (apply #'andv conjunction)))
	   )
	 )
     v)

     (screamer::attach-noticer!
     #'(lambda()
	 (when (and (known?-false z) (bound? v))
	   (dolist (x (value-of v))
	     (assert! (funcall f x))
	     )
	   )
	 )
     z)
    z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This returns a variable constrained to have the value 1 if the argument
;;; is t and 0 if the argument is nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reifyv (b)
  (cond 
   ((known?-true b) 1)
   ((known?-false b) 0)
   (t
    (let* ((z (an-integer-betweenv 0 1)))
    
      (attach-noticer!
       #'(lambda()
	   (when (known?-true b) 
             (assert!-equalv z 1)
             )
	   (when (known?-false b)
             (assert!-equalv z 0)
             )
	   )
       b)
      z
      )
    )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro: at-mostv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This version of at-mostv works but is suboptimal
;;;
;;;(defun at-mostv (n f &rest x)
;;;  (let* (
;;;	 (z (a-booleanv))
;;;	 (vals (apply #'mapcarv (cons f x)))
;;;	 truths 
;;;	  )
;;;
;;;    (setq truths (apply #'+v (mapcar #'reifyv vals)))
;;;    (assert!-equalv z (<=v truths n))
;;;    z
;;;    )
;;;  )

;;; Changed to backquote syntax 1/8/99
(defmacro at-mostv (n f &rest x)
  `(at-mostv-internal ,n ,f ,@x)
   )

 
(defun at-mostv-internal (n f &rest x)
  (declare (integer n))
   (let* (
	  (z (a-booleanv))
	  (countup 0)
	  (noes 0)
	  (shortest (apply #'min (mapcar #'length x)))
	  (known-list (make-list shortest :initial-element nil))
	  )
     (declare (integer countup noes shortest))
      
     (do* (
 	   (cdrs x (mapcar #'cdr cdrs))
 	   (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 	   (c 0 (1+ c))
 	   )
 	 ((some #'null cdrs) t)
       
       (let (
	     ;;(d (nth c x))
 	     (cars cars)
 	     (c c)
 	     temp
 	     )
 	 (screamer::attach-noticer!
 	  #'(lambda()
 	      (when (every #'bound? cars)
 		(setq temp (apply f cars))
 		(when (bound? temp)
 		  (when (null (nth c known-list))
 		    (local (setf (nth c known-list) t))
 		    )
 		  (if (equal (value-of temp) t)
 		      (progn
 			(local (setq countup (1+ countup)))
 			(when (> countup n)
 			  (assert! (notv z))
 			  )
 			)
 		    ;; temp must be nil
 		    (local (setq noes (1+ noes)))
 		    )
 		  )

 		(when (and (<= (- shortest noes) n)
 			   (not (known?-true z))
 			   )
 		  (assert! z)
 		  )

 		;; Short cut
 		(when (and (= countup n)
 			   (known?-true z)
 			   )
 		  (do* (
 			(cdrs x (mapcar #'cdr cdrs))
 			(cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 			(q 0 (1+ q))
 			)
 		      ((some #'null cdrs) t)
 
 		    (when (null (nth q known-list))
		      (assert! (notv (apply f cars)))
 		      )
 		    )
 		  ) ; when

 		)
 	      )
 	  cars)
 	 )
        )


      (screamer::attach-noticer!
       #'(lambda()
 	  (when (bound? z)
 	    (when (and (= countup n)
 		       (known?-true z)
 		       )
 	      (do* (
 		    (cdrs x (mapcar #'cdr cdrs))
 		    (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 		    (q 0 (1+ q))
 		    )
 		  ((some #'null cdrs) t)
 		
 		(when (null (nth q known-list))
 		  (assert! (notv (apply f cars)))
 		  )
 		)
 	      ) ; when
 	    ) ; when
 	  )
       z)

     z)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro: at-leastv
;;;
;;; For historical reasons, calls to at-leastv are translated to calls
;;; to at-leastv-internal
;;; n must be bound at invocation time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This version of at-leastv works but is suboptimal 
;;; 
;;;(defun at-leastv (n f &rest x)
;;;  (let* (
;;;	 (z (a-booleanv))
;;;	 (vals (apply #'mapcarv (cons f x)))
;;;	 truths 
;;;	  )
;;;
;;;    (setq truths (apply #'+v (mapcar #'reifyv vals)))
;;;    (assert!-equalv z (>=v truths n))
;;;    z
;;;    )
;;;  )
 
;;; Changed to backquote syntax 1/8/99
(defmacro at-leastv (n f &rest x)
   `(at-leastv-internal ,n ,f ,@x)
   )
;;; 
;;; 
;;; 

(defun at-leastv-internal (n f &rest x)
  (declare (integer n) (function f))
;   (local
    (let* (
 	  (z (a-booleanv))
 	  (countup 0)
 	  (noes 0)
 	  (shortest (apply #'min (mapcar #'length x)))
 	  (known-list (make-list shortest :initial-element nil))
	   )
      (declare (integer countup) (integer noes) (integer shortest))
      
      (do* (
 	   (cdrs x (mapcar #'cdr cdrs))
 	   (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 	   (c 0 (1+ c))
 	   )
 	 ((some #'null cdrs) t)
        
        (let (
 	     ;(d (nth c x))
 	     (cars cars)
 	     (c c)
 	     temp
 	     )
 	 (screamer::attach-noticer!
 	  #'(lambda()
 	      (when (every #'bound? cars)
 		(setq temp (apply f cars))
 		(when (bound? temp)
 		  (when (null (nth c known-list))
 		    (local (setf (nth c known-list) t))
 		    )
 		    (if (equal (value-of temp) t)
 			(progn
 			  (local (incf countup))
 			  (when (>= countup n)
 			    (screamer::assert!-true z)
 			    )
 			  )
 		      (local (incf noes))
 		      )
 		    )
 		;; If the number of unknowns plus the yesses is < n
 		;; then at-leastv is not satisfiable
 		(when (and (< (- shortest noes) n)
 			   (not (known?-false z))
 			   )
 		  (screamer::assert!-false z) ; (notv z))
 		  )
 		;; Short cut
 		(when (and (= (- shortest noes) n)
 			   (known?-true z)
 			   )
 		  (do* (
 			(cdrs x (mapcar #'cdr cdrs))
 			(cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 			(q 0 (1+ q))
 			)
 		      ((some #'null cdrs) t)
 		    
 		    (when (null (nth q known-list))
 		      (assert! (apply f cars))
 		      )
 		    )
 		  ) ; when
 		)
 	      )
 	  cars)
 	 )
        )

      (screamer::attach-noticer!
       #'(lambda()
 	  (when (and (= (- shortest noes) n)
 		     (known?-true z)
 		     )
 	    (do* (
 		  (cdrs x (mapcar #'cdr cdrs))
 		  (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 		  (q 0 (1+ q))
 		  )
 		((some #'null cdrs) t)
 	      
 	      (when (null (nth q known-list))
 		(assert! (apply f cars))
 		)
 	      )
 	    ) ; when
 	  )
       z)
     
     z)
;    )
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro: exactlyv
;;;
;;; This macro combines the functionality of at-leastv and at-mostv to
;;; arrive at the functionality of exactlyv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This definition of exactlyv works but is suboptimal
;;;
;;;(defun exactlyv (n f &rest x)
;;;  (let* (
;;;	 (z (a-booleanv))
;;;	 (vals (apply #'mapcarv (cons f x)))
;;;	 truths 
;;;	  )
;;;
;;;    (setq truths (applyv #'+ (mapcar #'reifyv vals)))
;;;    (assert!-equalv z (equalv truths n))
;;;    z
;;;    )
;;;  )


;;; ;;; Changed to backquote syntax 1/8/99
(defmacro exactlyv (n f &rest x)
  `(exactlyv-internal ,n ,f ,@x)
  )

(defun exactlyv-internal (n f &rest x)
  (declare (integer n))
    (let* (
 	  (z (a-booleanv))
 	  (countup 0)
 	  (noes 0)
 	  (shortest (apply #'min (mapcar #'length x)))
 	  (known-list (make-list shortest :initial-element nil))
 	  )
      
      (do* (
 	   (cdrs x (mapcar #'cdr cdrs))
 	   (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 	   (c 0 (1+ c))
 	   )
 	 ((some #'null cdrs) t)
        
        (let (
 	     ;(d (nth c x))
 	     (cars cars)
 	     (c c)
 	     temp
 	     )
 	 (screamer::attach-noticer!
 	  #'(lambda()
 	      (when (every #'bound? cars)
 		(setq temp (apply f cars))
 		(when (bound? temp)
 		  (when (null (nth c known-list))
 		    (local (setf (nth c known-list) t))
 		    )
 		    (if (equal (value-of temp) t)
 			(progn
 			  (local (setq countup (1+ countup)))
 			  (when (>= countup n)
 			    (screamer::assert!-true z)
 			    )
			  ;; From at-mostv
			  (when (> countup n)
			    (screamer::assert!-false z) ;(notv z))
			    )
 			  )
 		      (local (setq noes (1+ noes)))
 		      )
 		    )
 		;; If the number of unknowns plus the yesses is < n
 		;; then at-leastv is not satisfiable
 		(when (and (< (- shortest noes) n)
 			   (not (known?-false z))
 			   )
 		  (screamer::assert!-false z) ; (notv z))
 		  )
		;; From at-mostv
		(when (and (<= (- shortest noes) n)
 			   (not (known?-true z))
 			   )
 		  (screamer::assert!-true z)
 		  )
 		;; Short cut
 		(when (and (= (- shortest noes) n)
 			   (known?-true z)
 			   )
 		  (do* (
 			(cdrs x (mapcar #'cdr cdrs))
 			(cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 			(q 0 (1+ q))
 			)
 		      ((some #'null cdrs) t)
 		    
 		    (when (null (nth q known-list))
 		      (assert! (apply f cars))
 		      )
 		    )
 		  )			; when
		;; From at-mostv
		(when (and (>= countup n)
 			   (known?-true z)
 			   )
 		  (do* (
 			(cdrs x (mapcar #'cdr cdrs))
 			(cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 			(q 0 (1+ q))
 			)
 		      ((some #'null cdrs) t)
 
 		    (when (null (nth q known-list))
 		      (assert! (notv (apply f cars)))
 		      )
 		    )
 		  ) ; when
 		)
 	      )
 	  cars)
 	 )
        )
 
#|
      (screamer::attach-noticer!
       #'(lambda()
 	  (when (and (= (- shortest noes) n)
 		     (known?-true z)
 		     )
 	    (do* (
 		  (cdrs x (mapcar #'cdr cdrs))
 		  (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 		  (q 0 (1+ q))
 		  )
 		((some #'null cdrs) t)
 	      
 	      (when (null (nth q known-list))
 		(assert! (apply f cars))
 		)
 	      )
 	    ) ; when
 	  )
       z)
|#
      (screamer::attach-noticer!
       #'(lambda()
 	  (when (bound? z)
 	    (when (and (= countup n)
 		       (known?-true z)
 		       )
 	      (do* (
 		    (cdrs x (mapcar #'cdr cdrs))
 		    (cars (mapcar #'car cdrs) (mapcar #'car cdrs))
 		    (q 0 (1+ q))
 		    )
 		  ((some #'null cdrs) t)
 		
 		(when (null (nth q known-list))
 		  (assert! (notv (apply f cars)))
 		  )
 		)
 	      ) ; when
 	    ) ; when
 	  )
       z)

     z)
;    )
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Returns a variable constrained to be the subseq of a sequence

(defun subseqv (x n &optional (q nil) )
  (let (
        (z (make-variable))
        noticer 
        (done nil)
        )

      (if q
	  (assert! (equalv z (funcallv #'subseq x n q)))
	(assert! (equalv z (funcallv #'subseq x n)))
	)
      (setq noticer
	    #'(lambda ()
		(when (and (bound? x) (bound? n) (bound? q) 
			   (not (bound? z)) (not done))
		  (if (not (null q))
		      (assert! (equalv z (subseq (value-of x) (value-of n) (value-of q))))
		    (assert! (equalv z (subseq (value-of x) (value-of n))))
		    )
		  (setq done t)
		  )
		)
	    )
      (attach-noticer!
       #'(lambda()  
	   (when (and (bound? z) 
		      (listp (value-of z)) ; if z is a list, then so is x 
		      (bound? x) (bound? n) (bound? q) (not done))
	     (do (
		  (zn 0 (1+ zn))
		  (s (value-of n) (1+ s))
		  (c (length (value-of z)) (1- c))
		  )
		 
		 ((= c 0) t)
	       (assert! (equalv (nth zn (value-of z)) (nth s (value-of x)))) 
	       )
	     (setq done t)
	     )
	   )
       z)
      
      (attach-noticer! noticer x)
      (attach-noticer! noticer n)
      (attach-noticer! noticer q)
      z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: lengthv
;;;
;;; This function constrains a variable to be the length of a list
;;; I could provide an optional argument which says what type of
;;; argument should be created if z becomes bound first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lengthv (el &optional (is-list t))
  (let (
	(z (an-integer-abovev 0))
	)

    (assert!-equalv z (funcallv #'length el))
    (screamer::attach-noticer!
     ;; Propagate possible values
     #'(lambda()
	 (when (and (not (bound? el)) (enumerated-domain-p el))
	   (assert!-memberv-internal z (mapcar #'length (variable-enumerated-domain el)))
	   )
	 )
     el)

;;; This function could also propagate from an unbound domain of z to el
    (when is-list
      (screamer::attach-noticer!
       #'(lambda()
	   ;; The (not (enumerated-domain-p el)) condition seems to be necessary
	   ;; because of a problem in SCREAMER which disallows, say,
	   ;; (head one) | (head two) to be unified with the variable ([1] [2])
	   (when (and (bound? z) (not (bound? el)) (not (enumerated-domain-p el)))
	     (assert!-equalv el (make-listv (value-of z)))
	     )
	   )
       z)
      )
    
    z)
  )

;;; This function has 2 arguments: a variable and a list of other variables
(defun all-different2 (x xs)
  (if (null xs)
      t
    ;;    (andv (notv (equalv x (car xs)))
    (andv (notv (funcallv #'equal x (car xs)))
	  (all-different2 x (cdr xs))
	  (all-different2 (car xs) (cdr xs))
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: all-differentv
;;;
;;; This function can be used to constrain its (symbolic) arguments
;;; to all be different.
;;; This function has a variable number of arguments, but at least one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-differentv (x &rest xs)
  (all-different2 x xs)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --- Sets ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: a-set-ofv
;;;
;;; Models a set as a list
;;; Each member of the set must produce t when the test is applied to it.
;;; Test is a deterministic function which returns a boolean value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-set-ofv (test)
  (let (
	(z (a-listv))
	)

    (assert! (everyv test z))
    z ; return the variable
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: a-set-of-instancesv
;;;
;;; Creates a set of instances of a particular CLOS class
;;; as soon as the length of the list is known, the instances are created.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-set-of-instancesv (cname)
  (let* (
	 (z (make-variable))
	 (n (lengthv z))
	 (done nil)
	 )

    (screamer::attach-noticer!
     #'(lambda()
	 (let ((acc nil))
	   (when (and (bound? n) (not done))
	     (setq done t)
	     (dotimes (c (value-of n))
	       (setq acc (append acc (list (make-instancev cname))))
	       )
	     (assert! (equalv z acc))
	     )))
     n)
    
    (screamer::attach-noticer!
     #'(lambda()
	 (when (bound? z)
	   (assert! (equalv n (length (value-of z))))
	   )
	 )
     z)
    z
    )
  )


;;; This function counts the number of times that the element a occurs in the list x

(defun count-occurrences (a x)
  (let ((count 0))
    (dolist (d x)
       (when (equal a d) (incf count))
       )
     count)
  )



;;; This function returns a variable which is constrained to have the same
;;; elements as the variable or value x

(defun a-reorderingv(x)
  (let (
        (z (make-variable))
        occurs valx
        )

    (attach-noticer!
     #'(lambda()
	 (when (bound? x)
	   (setq valx (value-of x))
	   (assert! (equalv z (make-listv (length valx))))
	   
	   (dolist (d (value-of z)) 
	     (assert! (memberv d valx))
	     ) 
	   (dolist (d valx)
	     (let ((d d))
	       (setq occurs (count-occurrences d valx))
	       (assert! (at-leastv occurs (constraint-fn #'(lambda(y) (equal y d))) (value-of z)))
	       )
	     )
	   )
	 )
     x)
    
    z)
  )

;;; This function returns a variable which is constrained to return a list
;;; containing the distinct elements of x. The argument x can be either a
;;; value or a constraint variable at the time of function invocation.

(defun members-ofv (x)
  (let (
        (z (make-variable))
        )

     (attach-noticer!
       #'(lambda()
           (when (and (bound? x) (every #'bound? (value-of x)))
              (do* (
                    (dec (apply-substitution x) (cdr dec))
                    (curr (car dec) (car dec))
                    (vals nil)
                    )
                  ((endp dec) 
                   (assert! (equalv z vals))
                   )
                 (pushnew (value-of curr) vals :test #'equal)
                 )
              )
           )
       x)
     z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: set-equalv
;;;
;;; This function returns a boolean variable constrained to indicate whether
;;; the lists x and y have the same members
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-equalv (x y)
  (let (
	(z (a-booleanv))
	noticer
	)
    
    (flet (;; This is just a function which can be used by 'sort' to
	   ;; derive some well-defined ordering for any known values x and y
	   (strcmp(x y)
		  (numberp (string> (format nil "~s" x)
				    (format nil "~s" y)))
		  )
	   )

      (setq noticer
	    #'(lambda()
		(when (and (bound? x) (bound? y))
		  (assert!
		   (equalv z
			   (equalv
			    (sort
			     (value-of (members-ofv (apply-substitution x)))
			     #'strcmp)
			    (sort
			     (value-of (members-ofv (apply-substitution y)))
			     #'strcmp)
			    )
			   )
		   )
		  )
		)
	    )
      (attach-noticer! noticer x)
      (attach-noticer! noticer y)
      )
    z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: bag-equalv
;;;
;;; This function returns a variable constrained to indicate whether the
;;; bags x and y are equal. Two bags are equal when they contain the same
;;; numbers of each of their elements.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bag-equalv (x y)
  (if (and (bound? x) (bound? y)
           (not (= (length x) (length y)))
           )
     nil
     (let (
           (z (a-booleanv))
           (condition1 nil)
           (condition2 nil)
           occurs noticer
           )
        
        
        (setq noticer
          #'(lambda()
              (when (and (bound? z) (known?-true z)
                         (bound? x) (not (bound? y))
                         )
                 (assert! (equalv y (make-listv (length (value-of x)))))
                 )
              (when (and (bound? z) (known?-true z)
                         (bound? y) (not (bound? x))
                         )
                 (assert! (equalv x (make-listv (length (value-of y)))))
                 )
              (when (and (bound? x) (bound? y)
                         (bound? z) (known?-true z)
                         )
                 (when (every #'bound? (value-of x))
                    (dolist (d (value-of y))
                       (assert! (memberv d (value-of x)))
                       )
                    )
                 (when (every #'bound? (value-of y))
                    (dolist (d (value-of x))
                       (assert! (memberv d (value-of y)))
                       )
                    )
                 )
              )
          )
        
        (attach-noticer! noticer x)
        (attach-noticer! noticer y)
        (attach-noticer! noticer z)
        
        (attach-noticer!
          #'(lambda()
              (when (and (bound? x) (bound? y) (every #'bound? (value-of x)))
                 (dolist (d (value-of x))
                    (let ((d d))
                       (setq occurs (count-occurrences (value-of d) (value-of x)))
                       (push 
                         (exactlyv occurs (constraint-fn #'(lambda(w) (equal w d))) (value-of y))
                         condition1)
                       ;(push (everyv #'(lambda(w) (memberv w (value-of x))) y) condition1)
                       )
                    )
                 (assert! (equalv z (apply #'andv condition1)))
                 )
              )
          y)
        
        (attach-noticer!
          #'(lambda()
              (when (and (bound? x) (bound? y) (every #'bound? (value-of y)))
                 (dolist (d (value-of y))
                    (let ((d d))
                       (setq occurs (count-occurrences d (value-of y)))
                       (push 
                         (exactlyv occurs (constraint-fn #'(lambda(w) (equal w d))) (value-of x))
                         condition2)
                       ;(push (everyv #'(lambda(w) (memberv w (value-of y))) x) condition2)
                       )
                    )
                 (assert! (equalv z (apply #'andv condition2)))
                 )
              )
          x)
        
        z)
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: a-subset-ofv
;;;
;;; This function returns a variable which is constrained to be a subset
;;; of the argument x
;;; NOTE: it is only wise to use this for small lists!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-subset-ofv (x)
  (let (
	(z (make-variable))
	valx
	)

    (attach-noticer!
     #'(lambda()
	 (when (and (bound? x) (every #'bound? x))
	   (setq valx (apply-substitution x))
	   (assert! (memberv z (all-values (solution (a-subset-of valx) (static-ordering #'linear-force)))))
	   
	   )
	 )
     x)
    
    z)
  )


;;; This function taken from one of the SCREAMER papers

(defun a-subset-of (x)
  (if (null (value-of x))
      nil
    (let (
	  (y (a-subset-of (cdr x)))
	  )
      (either (cons (car x) y) y)
      )
    )
  )

;;; This function also taken from one of the SCREAMER papers

(defun a-partition-of (x)
  (if (null x)
      nil
    (let (
	  (y (a-partition-of (cdr x)))
	  )
      (either (cons (list (car x)) y)
	      (let (
		    (z (a-member-of y))
		    )
		(cons (cons (car x) z)
		      (remove z y :test #'equal :count 1)
		      )
		)
	      )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: subsetpv
;;;
;;; This function returns a boolean variable constrained to indicate
;;; whether x is a subset of y.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subsetpv (x y)
  (let ((y y) (x x))
    (everyv #'(lambda(d) (memberv d y)) x)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: intersectionv
;;;
;;; This function returns a variable constrained to be the intersection
;;; of the two sets x and y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun intersectionv (x y)
  (let (
	(z (make-variable))
	noticer
	)
    
    (setq noticer
	  #'(lambda()
	      (when (and (ground? x) (ground? y))
		(assert! (equalv z (intersection
				    (apply-substitution x)
				    (apply-substitution y)
				    :test #'equal)
				 ))
		)
	      )
	  )
    (attach-noticer! noticer x)
    (attach-noticer! noticer y)
    (attach-noticer!
     #'(lambda()
	 (when (and (ground? z)
		    (not (and (ground? x) (ground? y)))
		    )
	   (assert! (everyv #'(lambda(m) (memberv m x)) z))
	   (assert! (everyv #'(lambda(m) (memberv m y)) z))
	   )
	 )
     z)
    z
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: unionv
;;;
;;; This function returns a variable constrained to the union of the two
;;; sets x and y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			   
(defun unionv (x y)
  (let (
	(z (make-variable))
	noticer
	)
    
    (setq noticer
	  #'(lambda()
	      (when (and (ground? x) (ground? y))
		(assert! (equalv z (union
				    (apply-substitution x)
				    (apply-substitution y)
				    :test #'equal)
				 ))
		)
	      )
	  )
    (attach-noticer! noticer x)
    (attach-noticer! noticer y)
    (attach-noticer!
     #'(lambda()
	 (when (and (ground? z)
		    (not (and (ground? x) (ground? y)))
		    )
	   (assert! (everyv #'(lambda(m) (memberv m z)) x))
	   (assert! (everyv #'(lambda(m) (memberv m z)) y))
	   )
	 )
     z)
    z
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     --- CONSTRAINTS AND OBJECTS ---
;;;
;;; This part contains screamer+ functions which are related to the fusing
;;; of constraints and CLOS objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: make-instancev
;;;
;;; This function can be used to create object instances as soon as the
;;; class of the object becomes known
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instancev (&rest args)
  (if (every #'bound? args)
      (apply #'make-instance (mapcar #'value-of args))
    (let ((z (make-variable)))
      (if (bound? (car args))
	  (assert! (equalv z (apply #'make-instance (mapcar #'value-of args))))
	(progn
	  (screamer::attach-noticer!  
	   #'(lambda() 
	       (when (and (bound? (car args)) (not (bound? z)))
		 (assert! (equalv z 
				  (apply #'make-instance (mapcar #'value-of args))))
		 )
	       )
	   (car args))
	  (screamer::attach-noticer!
	   #'(lambda()
	       (when (and (bound? z) (not (bound? (car args))))
		 (let (
		       (object-type (class-name (class-of (value-of z))))
		       temp
		       )
		   (assert! (equalv (car args) object-type))
		   (setq temp (apply #'make-instance (mapcar #'value-of args)))
		   (reconcile-objects temp (value-of z))
		   )
		 )
	       )
	   z)
	  )
	)
      z)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: classpv
;;;
;;; A function for dealing with object classes 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun classpv (obj name)
   (let (z domain-reducer)
      
      (setq z (equalv
                (funcallv #'(lambda(x) (class-name (class-of x))) obj)
                name))  
   
      ;; This noticer tries to reduce the domain of obj if z is bound 
      (setq domain-reducer
       #'(lambda()
           (when (and (bound? name)
                      (bound? z)
                      (not (bound? obj))
                      (enumerated-domain-p obj)
                      )
              (setf (variable-enumerated-domain obj)
                    (if (value-of z)
                       (remove-if-not 
                         #'(lambda(x) (or (typep x name) (typep x 'screamer::variable)))
                         (variable-enumerated-domain obj)
                         )
                       (remove-if 
                         #'(lambda(x) (typep x name))
                         (variable-enumerated-domain obj)
                         )
                       )
                    )
              )
           )
        )
      
      (screamer::attach-noticer! domain-reducer obj)
      (screamer::attach-noticer! domain-reducer z)
   
      z)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: class-namev
;;;
;;; Constrains a variable to be the class name of a class
;;; Note that given an object instance, you need first to retrieve its
;;; class using class-ofv, and then its name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun class-namev (obj)
   (let (
         (z (make-variable))
         )
      
      (screamer::attach-noticer!
       #'(lambda()
           (when (and (not (bound? obj))
                      (enumerated-domain-p obj)
                      )
              (assert! (memberv z (mapcar #'class-name (variable-enumerated-domain obj))))
              )
           (when (bound? obj)
              (make-equal z (class-name (value-of obj)))
              )
           )
       obj)
      
      z)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: class-ofv
;;;
;;; Constrains a variable to be the class of an object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun class-ofv (obj)
   (let (
         (z (make-variable))
         )
      
      (screamer::attach-noticer!
       #'(lambda()
           (when (and (not (bound? obj))
                      (enumerated-domain-p obj)
                      )
              (assert! (memberv z (mapcar #'class-of (variable-enumerated-domain obj))))
              )
           (when (bound? obj)
              (make-equal z (class-of (value-of obj)))
              )
           )
       obj)
      
      z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: slot-exists-pv
;;;
;;; Returns a variable constrained to be a boolean indicating the existence
;;; of a slot in the given object.
;;; Note that the variable is not automatically bound to true. If this is
;;; required, it must be done by the calling function. The function has been
;;; implemented this way for 2 reasons: firstly, to closely model the
;;; behaviour of the analogue LISP function slot-exists-p, and secondly,
;;; because it may sometimes be useful to constrain a slot not to exist in
;;; a particular object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slot-exists-pv (obj slotname)
   (let (
         (z (make-variable))
         noticer
         )
      
      (setq noticer
        #'(lambda() 
            (when (and (bound? obj)
                       (bound? slotname)
                       )
               (assert! (equalv z (slot-exists-p (value-of obj) (value-of slotname))))
               )
            (when (and (not (bound? obj))
                       (enumerated-domain-p obj)
                       (bound? slotname)
                       )
               (assert! (memberv z (mapcar #'(lambda(x) (slot-exists-p x (value-of slotname)))
                                     (variable-enumerated-domain obj)))
                 )
               )
            (when (and (not (bound? obj))
                       (enumerated-domain-p obj)
                       (bound? z)
                       )
               (dolist (d (variable-enumerated-domain obj))
                  (when (notv (equal (slot-exists-p d (value-of slotname)) (value-of z)))
                     (assert! (notv (memberv obj (list d))))
                     )
                  )
               )
            )
        )
      (screamer::attach-noticer! noticer obj)
      (screamer::attach-noticer! noticer slotname)
      (screamer::attach-noticer! noticer z) 
      z)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: reconcile
;;;
;;; Tests the mutual compatibility of two variables by trying to make them
;;; equal. This works for objects as well as simple (constraint) variables
;;;
;;; The function returns a boolean variable which comments on the
;;; compatibility of the two variables.
;;;
;;; Note that calling the function has side effects on the two variables
;;;
;;; *** Perhaps I should combine this function with make-equal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reconcile (var1 var2)
  (cond 
   ((equal var1 var2) t)
   ((and (objectp var1) (objectp var2))
    (reconcile-objects var1 var2)
    )
   (t
;    (and (variable? var1)
;	 (variable? var2)
;	 (not (objectp (value-of var1)))
;	 (not (objectp (value-of var2)))
;	 )
    ;;; Try to make the two variables equal
    (catch 'fail
      (assert! (equalv var1 var2))
      t
      )
    )
;   ((and (variable? var1)
;	 (bound? var1))
;    (reconcile (value-of var1) var2)
;    )
;   ((and (variable? var2)
;	 (bound? var2))
;    (reconcile (value-of var2) var1)
;    )
;   (t nil)
   )
  )


;;; Forces the slot values of two objects (or screamer variables containing 
;;; objects) to be equal
;;; If the slots themselves contain objects, then the process is carried out
;;; recursively until all slots have been reconciled, or a fail has been 
;;; generated

(defun reconcile-objects (objvar1 objvar2)
  (let (
	(obj1-slots (a-listv))
	(obj2-slots (a-listv))
	(obj1 objvar1)
	(obj2 objvar2)
	)
    (if (variable? objvar1)
	(setq obj1 (value-of objvar1))
      (setq obj1 objvar1)
      )
    (if (variable? objvar2)
	(setq obj2 (value-of objvar2))
      (setq obj2 objvar2)
      )
    
    (setq obj1-slots (funcallv #'slot-names-of obj1))
    (setq obj2-slots (funcallv #'slot-names-of obj2))
    ;; The slot names should be the same
    (assert! (equalv obj1-slots obj2-slots))

    ;; If slots are unbound at the object level, then create unbound
    ;; SCREAMER variables to fill the slots
    (dolist (s (value-of obj1-slots))
      (when (not (slot-boundp obj1 s))
	(setf (slot-value obj1 s) (make-variable))
	)
      (when (not (slot-boundp obj2 s))
	(setf (slot-value obj2 s) (make-variable))
	)
      (assert! (slot-values-equalv (slot-valuev obj1 s) (slot-valuev obj2 s)))
      )

    ;;; This is some postprocessing to eliminate unnecessary screamer variables
    ;;; If a slot value is a bound screamer variable, then the screamer variable
    ;;; is replaced by its bound value.
    (dolist (s (value-of obj1-slots))
      (when (and (slot-boundp obj1 s)
		 (variable? (slot-value obj1 s))
		 (bound? (slot-value obj1 s))
		 )
	(setf (slot-value obj1 s) (value-of (slot-value obj1 s)))
	)
      (when (and (slot-boundp obj2 s)
		 (variable? (slot-value obj2 s))
		 (bound? (slot-value obj2 s))
		 )
	(setf (slot-value obj2 s) (value-of (slot-value obj2 s)))
	)
      )
    t
    )
  )

;;; This function is expected to be internal to the package

(defun slot-values-equalv (val1 val2)
  (let (
	(z (a-booleanv))
	ok
	)

    (setq ok
	  (catch 'fail 
	    (assert! (equalv z (equalv val1 val2)))
	    t
	    )
	  )
     (when (not ok)
       (setq z (a-booleanv))
       (reconcile val1 val2)
       )
     z
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: slot-valuev
;;;
;;; Returns a value constrained to be the given slot value of the supplied 
;;; object 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slot-valuev (objvar slotname)
  (if (and (bound? objvar)
	   (slot-boundp (value-of objvar) slotname)
	   )
      (slot-value (value-of objvar) (value-of slotname))
    (let (
	  (z (make-variable))
	  )
      
      (screamer::attach-noticer!
       #'(lambda()
	   (when (bound? objvar)
		      ; and (slot-boundp (value-of objvar) slotname) ?
	     (assert! (equalv z (slot-value (value-of objvar) (value-of slotname))))
	     )
	   (when (and (not (bound? objvar))
		      (enumerated-domain-p objvar)
		      )
	     ;(format t "One of ~a~%" (mapcar #'(lambda(x) (slot-value x slotname)) (variable-enumerated-domain objvar)))
	     (assert!-memberv-internal z
			       (mapcar #'(lambda(x) (slot-value x slotname))
				       (variable-enumerated-domain objvar))
			       )
	     
	     )
	   )
       objvar)

      ;; This noticer added 27.10.98
      ;; It ensures that when z is further restricted, the domain of objvar is
      ;; again tested for consistency with the new domain of z
       (screamer::attach-noticer!
       #'(lambda()
	   (when (and (bound? objvar)
		      (slot-boundp (value-of objvar) slotname)
		      )
	     (assert! (equalv z (funcallv #'slot-value objvar slotname)))
	     )
	   (when (and (not (bound? z))
		      (not (bound? objvar))
		      (enumerated-domain-p z))
	     ;; The idea of this bit is that if the slot-value is known, but
	     ;; the object isn't, some objects could be ruled out because
	     ;; they don't have the right slot-value
	     ;; Unfortunately, it wasn't working very well so I commented it
	     ;; out on 3/3/99
	     ;; I think I probably need to test for consistency, rather than
	     ;; just equality

	     #|
	     (assert!-memberv-internal objvar
				       (remove-if-not #'(lambda(x) (member (slot-value x slotname) (variable-enumerated-domain z) :test #'equal))
						      (variable-enumerated-domain objvar))
				       )
		      
|#

	     )
	   )
       z)

      
      z
      )
    )
  )

;;; This function returns true when all the slots which are bound in both
;;; objects are equal

(defun bound-slots-equal (obj1 obj2)
  (let (
	(obj1-slots (slot-names-of obj1))
	(obj2-slots (slot-names-of obj2))
	)
    
    (when (equal obj1-slots obj2-slots)
      (every
       #'(lambda(s)
	   (if (and (slot-boundp obj1 s) (slot-boundp obj2 s))
	       (equal (slot-value obj1 s) (slot-value obj2 s))
	     t
	     )
	   )
       obj1-slots
       )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function: not-equalv
;;;
;;; This is an experiment to improve upon the SCREAMER version
;;; (notv (equalv x y)), which has poor propagation properties
;;; When full propagation is set to true, then a domain value which
;;; is equal to a bound argument (either x or y) is automatically
;;; removed from the enumerated domain of the other variable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun not-equalv (x y &key (full-propagation nil))
  (if (and (bound? x) (bound? y))
      (not (equal (value-of x) (value-of y)))
    
    (let (
	  (z (a-booleanv))
	  )

      ;; This is where I get the functionality of (notv (equalv x y))
      (assert! (eqv z (notv (equalv x y))))

      ;; This is where I improve upon it

      (attach-noticer!
       #'(lambda()
	   (when (and (bound? x) (not (bound? y)))
	     (assert!-equalv z (notv (memberv y (list (value-of x)))))
	     (when (and full-propagation (known?-true z) (enumerated-domain-p y))
	       (assert!-memberv-internal
		y
		(remove-if #'(lambda(e) (equal e (value-of x)))
			   (variable-enumerated-domain y))
		)
	       )
	     )
	   )
       x)
       
      (attach-noticer!
       #'(lambda()
	   (when (and (bound? y) (not (bound? x)))
	     (assert!-equalv z (notv (memberv x (list (value-of y)))))
	     (when (and full-propagation (known?-true z) (enumerated-domain-p x))
	       (assert!-memberv-internal
		x
		(remove-if #'(lambda(e) (equal e (value-of y)))
			   (variable-enumerated-domain x))
		)
	       )
	     )
	   )
       y)

      ;; I could also attach a noticer to z

      z)
    )
  )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *enable-package-locked-errors*
;;; (Comment by Simon White on 28/5/1997 at 13:13
;;; Reset the variable to its default so that errors are signalled again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq excl:*enable-package-locked-errors* t)

