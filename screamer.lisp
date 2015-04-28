;;; -*- Mode: LISP; Package: (SCREAMER :USE CL :COLON-MODE :EXTERNAL); Base: 10; Syntax: Ansi-common-lisp -*-

;;; LaHaShem HaAretz U'Mloah

;;; Screamer
;;; A portable efficient implementation of nondeterministic CommonLisp
;;; Version 3.24
;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997 NEC Research Institute, Inc. All rights reserved.

;;; Written by:
;;; Jeffrey Mark Siskind
;;; NEC Research Institute, Inc.
;;; 4 Independence Way
;;; Princeton NJ 08540 USA
;;; Qobi@research.nj.nec.com
;;; 609/951-2705
;;; and:
;;; David Allen McAllester
;;; MIT Artificial Intelligence Laboratory
;;; 545 Technology Square Room NE43-412
;;; Cambridge MA 02139
;;; DAM@AI.MIT.EDU
;;; 617/253-6599

;;; You are free to use, copy and distribute this software provided that:
;;;  1. You report *ALL* bugs to Bug-Screamer@AI.MIT.EDU whether or not you
;;;     need them fixed. Include the version number (3.24) in the message.
;;;  2. You report *ALL* bugs that you fixed to Bug-Screamer@AI.MIT.EDU.
;;;     Include the version number (3.24) in the message.
;;;  3. Every time you run Screamer on a machine or using a Lisp compiler not
;;;     mentioned below, you send a message stating the new environment and the
;;;     version number (3.24) to Bug-Screamer@AI.MIT.EDU.
;;;  4. You inform us that you obtained a copy of Screamer by sending a message
;;;     to Info-Screamer-Request@AI.MIT.EDU to be put on the
;;;     Info-Screamer@AI.MIT.EDU mailing list.

;;; Important notice: In this version of Screamer, if Screamer is already
;;; loaded and you wish to recompile the entire file, the recompilation will
;;; proceed much faster if you first do:
;;; (CLRHASH SCREAMER::*FUNCTION-RECORD-TABLE*)

;;; Machines Supported or Not Supported
;;; Please send mail to Bug-Screamer@AI.MIT.EDU whenever you discover a new
;;; machine or CommonLisp implementation that either DOES or DOESN'T run
;;; Screamer correctly so I can update this list.
;;;  1. Genera 8.1.1 and 8.3 on Symbolics 36xx and Ivory:
;;;     No known bugs.
;;;     Updates to 8.1.1 and 8.3 based on modifications graciously provided by
;;;     Marty Hall of the Applied Physics Laboratory.
;;;  2. Lucid 4.0.2 and 4.1 on Sun SPARC,
;;;     Lucid 4.1 on SGI MIPS,
;;;     Lucid 4.1 on HP PA,
;;;     Lucid 4.1 on DEC MIPS, and
;;;     Lucid 4.0.1 on IBM RS/6000:
;;;     No known bugs except the following. Lucid has a royal screw however.
;;;     Macros like INCF and PUSH expand directly into internal structure
;;;     updating operations when called on generalized variables. The
;;;     expansion does not go indirectly through SETF so that (LOCAL (INCF X))
;;;     will compile into a global increment rather than a local increment.
;;;     This is technically allowed by the CommonLisp spec but shouldn't be.
;;;     For greater efficiency, you should load the patch mbmfe.sbin into
;;;     4.0.2/SPARC. This optional patch fixes a bug which prevents tail
;;;     recursion optimization. This patch should not be loaded into 4.1/SPARC
;;;     since 4.1/SPARC already contains this patch. Lucid 4.1/HP requires that
;;;     the patch file bug-5511.hbin be loaded. Lucid 4.1/DEC requires that
;;;     the patch file bug-5511.mbin be loaded.
;;;  3. MCL 2.0 and 2.0p2 on Apple Macintosh:
;;;     No known bugs except that 2.0 needs not-so-trivial-patch.fasl to be
;;;     loaded. This patch should not be loaded into 2.0p2 since 2.0p2 already
;;;     contains this patch.
;;;     This port based on an earlier port graciously provided by Volker
;;;     Haarslev of the Computer Science Department, University of Hamburg.
;;;  4. Harlequin 3.0.3+ on Sun SPARC:
;;;     No known bugs.
;;;     Assitance for this port graciously provided by Marty Hall of the
;;;     Applied Physics Laboratory.
;;;  5. Allegro 4.1 and 4.2 on Sun SPARC and SGI MIPS,
;;;     Allegro 4.1 on DEC MIPS, and
;;;     Allegro 4.3 on Linux/x86:
;;;     No known bugs.
;;;     Assistance for this port graciously provided by Brad Miller of the
;;;     Computer Science Department, University of Rochester.
;;;  6. Poplog 14.2 on Sun SPARC:
;;;     No known bugs.
;;;     This port based on an earlier port graciously provided by Aaron Sloman
;;;     of the Computer Science Department, University of Birmingham.
;;;  7. AKCL 1.605 and 1.615 on Sun SPARC:
;;;     No known bugs.
;;;  8. CMU Common Lisp 17b on Sun SPARC:
;;;     No known bugs.

;;; TTMTTD
;;;  1. Manual.
;;;  2. Should have way of having a stream of values.
;;;  3. Kashket's constraint additions Fall90-158.
;;;  4. Compress trail after repeated LOCAL SETF/SETQ to same variable
;;;  5. LOCAL SETF/SETQ on symbol can use special variable binding stack
;;;     instead of unwind-protect.
;;;  6. (F (G (H (EITHER X Y)))) where F, G and H are deterministic can
;;;     CPS-CONVERT to (FUNCALL #'(LAMBDA (Z) (F (G (H Z)))) (EITHER X Y)).
;;;  7. Should give warning when it recompiles functions due to changing
;;;     determinism status.
;;;  8. =V <V <=V >V and >=V should do congruence/monotone closure.
;;;  9. =V should propagate domains.
;;; 10. BEST-VALUE
;;; 11. Should cache VARIABLE-LOWER-BOUND/VARIABLE-UPPER-BOUND for domain
;;;     variables.
;;; 12. Faster version of BIND! which doesn't cons.
;;; 13. Get DIAGNOSIS and MONTAGUE to work.
;;; 14. Get GROW-UP and NONLINEAR examples to work.
;;; 15. FUNCALLV and APPLYV need to assert the domains of the variable that
;;;     they return.
;;; 16. Check that +V, -V, *V, /V, MINV and MAXV work and do the right thing
;;;     with type propagation.
;;; 17. Check that PROPAGATE does the right thing with bounds of integers.
;;; 18. MEMBERV and derivatives should support vectors.
;;; 19. Backtracking out of INTEGER-BETWEENV and the like will yield an
;;;     unconstrained variable since the constraints are asserted locally.

;;; Bugs to fix
;;;  1. LOCAL SETF does the wrong thing with GETHASH.
;;;  2. LOCAL (SETF/SETQ X e) will signal an error if X is unbound because it
;;;     needs to trail the previous value of X and it has none.
;;;  3. Deterministic/nondeterministic LOCAL SETF/SETQ undone out of order.
;;;  4. Changing determinism status can cause code from a different file to
;;;     be included causing wierd behavior.
;;;  5. Will signal an obscure error if FAIL is called in a deterministic
;;;     context which is not nested in a choice point.
;;;  6. May loop when expressions contain circular lists.
;;;  7. APPLY-NONDETERMINISTIC conses.

;;; Limitations
;;;  1. Does not handle SETF methods with multiple values for LOCAL SETF.
;;;  2. If you do a (SETF (SYMBOL-FUNCTION 'FOO) ...) to a nondeterministic
;;;     function you will lose when you attempt to evaluate (FOO ...).
;;;  3. If you do a (SETF (SYMBOL-FUNCTION 'FOO) ...) to a deterministic
;;;     function when FOO was previously defined as a nondeterministic
;;;     function you will lose when you attempt to evaluate (FOO ...).
;;;  4. The function record table will not work if we ever support FLET and
;;;     LABELS and in particular, if we ever support FLET and LABELS of
;;;     nondeterministic functions.
;;;  5. There is no way to force Screamer into compiling a deterministic
;;;     function as a nondeterministic one. A wizard might want to do this to
;;;     take advantage of the fact that a LOCAL SETF/SETQ in a nondeterministic
;;;     function does not cons up closures.
;;;  6. Doesn't handle most CommonLisp special forms.
;;;     Currently handle:
;;;       BLOCK
;;;       FUNCTION
;;;       GO
;;;       IF
;;;       LET
;;;       LET*
;;;       MULTIPLE-VALUE-CALL
;;;       MULTIPLE-VALUE-PROG1
;;;       PROGN
;;;       QUOTE
;;;       RETURN-FROM
;;;       SETQ
;;;       TAGBODY
;;;       THE
;;;     Probably will never handle:
;;;       CATCH
;;;       DECLARE
;;;       EVAL-WHEN
;;;       FLET
;;;       LABELS
;;;       MACROLET
;;;       PROGV
;;;       THROW
;;;       UNWIND-PROTECT
;;;    CLtL1 obsolete:
;;;       COMPILER-LET
;;;    CLtL2 additions:
;;;       GENERIC-FLET
;;;       GENERIC-LABELS
;;;       LOAD-TIME-VALUE
;;;       LOCALLY
;;;       WITH-ADDED-METHODS
;;;       SYMBOL-MACROLET

;;; Change Log
;;; W25Sep91 Qobi
;;;  Changed the who calls database code to fix the bug whereby after loading
;;;  the definition of a nondeterministic function FOO
;;;  (ALL-VALUES (FOO ...)) would not work because FOO would not be recognized
;;;  as nondeterministic until at least one DEFUN was expanded. In the process
;;;  changed the OLD/NEW-DETERMINISTIC? terminology.
;;; W25Sep91 Qobi
;;;  Fixed the bug whereby a function FOO which referenced #'BAR or
;;;  #'(LAMBDA (...) ... (BAR ...) ...) would not be recompiled when the
;;;  deterministic status of BAR changed. This involved a polarity switch on
;;;  NESTED?. This also fixed the bug whereby STATIC-ORDERING and REORDER were
;;;  incorrectly classified as nondeterministic.
;;; W25Sep91 Qobi
;;;  Made SOLUTION walk its argument.
;;; W25Sep91 Qobi
;;;  Separated USE-PACKAGE from IN-PACKAGE.
;;; W25Sep91 Qobi
;;;  Added *SCREAMER-VERSION*. Set it to 2.1
;;; H28Sep91 Qobi
;;;  Added FLIP. Changed EITHER to a macro which expands into FLIP. Expunged
;;;  from the code walker, any notion of EITHER being a special form. Removed
;;;  the optimization that inline expanded calls to FAIL. Version 2.2.
;;; W2Oct91 Qobi
;;;  Fixed bug in VARIABLES-IN. Version 2.3.
;;; R3Oct91 Qobi
;;;  Added COUNT-FAILURES. Version 2.4.
;;; S13Oct91 Qobi
;;;  Added :SCREAMER to *FEATURES* at the request of CGDEMARC. Version 2.5.
;;; F25Oct91 Qobi
;;;  Fixed bug with FUTURE-COMMON-LISP on Symbolics with SETF, FUNCTION and
;;;  LAMBDA. Version 2.6.
;;; M4Nov91 Qobi
;;;  Fixed INTEGER-BETWEEN to work correctly with noninteger arguments.
;;;  Removed SUBST form of Beta-conversion. Version 2.7.
;;; S1Dec91 Qobi
;;;  Changed -NOT- to -NOTV- naming convention to be consistent.
;;;  Changed INTEGERV to INTEGERPV, REALV to REALPV, NUMBERV to NUMBERPV and
;;;  BOOLEANV to BOOLEANPV to be consistent.
;;;  Can now walk EVAL-WHEN, FLET, LABELS, PROGV and THE.
;;;  Can now CPS convert THE. Also added types to CPS conversion.
;;;  Added WHEN-FAILING and rewrote COUNT-FAILURES.
;;;  Added POSSIBLY?
;;;  Added LOCAL-I/O
;;;  Version 2.8.
;;; T11Feb92 Qobi
;;;  Fixed PROCESS-SUBFORMS to fix bug whereby it didn't correctly walk
;;;  EVAL-WHEN, FLET and LABELS.
;;;  Fixed DEFUN FUNCTION-NAME to support RETURN-FROM FUNCTION-NAME in its
;;;  body for both deterministic and nondeterministic cases.
;;;  Fixed PEAL-OFF-DOCUMENTATION-STRING-AND-DECLARATIONS to not consider a
;;;  lone string in the body as a documentation string.
;;;  Version 2.9.
;;; M16Mar92 Qobi
;;;  Removed redundant "Improper form" error.
;;;  Changed all ''NIL and 'NIL to NIL.
;;;  Reorganized FORM-TYPEs.
;;;  Changed CONSTANT to QUOTE.
;;;  Aesthetic capitalization of ALLOW-OTHER-KEYS.
;;;  Renamed BLOCK to SEGMENT to be consistent.
;;;  Added SELF-EVALUATING? and QUOTIFY and changed processing of QUOTE and
;;;  VARIABLE.
;;;  Enforce non-NIL function names.
;;;  Added SCREAMER? argument to WALK.
;;;  Allow FLET/LABELS to bind SETF functions.
;;;  Version 2.10.
;;; T17Mar92 Qobi
;;;  Built new mechanism to determine when to recompile functions due to
;;;  changing determinism status to solve a long standing bug.
;;;  PERFORM-SUBSTITUTIONS had a call to itself rather than a FUNCALL to
;;;  FUNCTION.
;;;  Removed redundant check for NEEDS-SUBSTITUTION? in CPS-CONVERT since that
;;;  was checked by PERFORM-SUBSTITUTION anyway.
;;;  Made the check performed by NEEDS-SUBSTITUTION? tighter so that fewer
;;;  needless macro expansions take place for deterministic DEFUNs.
;;;  Version 2.11.
;;; M6Apr92, T7Apr92, R9Apr92, M13Apr92 Qobi
;;;  Changed DEFUN-COMPILE-TIME to compile functions.
;;;  Fixed bug in CPS-CONVERT introduced by QUOTE change.
;;;  Fixed polarity bug of FUNCTION-LAMBDA in NEEDS-SUBSTITUTION?
;;;  Got rid of POSITIVE/NEGATIVE-INFINITY and (OR RATIONAL FLOAT) bogosity.
;;;  Changed rules to use (NOT (VARIABLE? X)) instead of prior bogosity.
;;;  Fixed fence-post error in trail unwinding.
;;;  Added UNWIND-TRAIL.
;;;  Added COST-FUNCTION and PREDICATE parameters to REORDER
;;;  Fixed bug in DOMAIN-SIZE.
;;;  Added RANGE-SIZE.
;;;  Moved consistency checks to ends of rules to fix a bug.
;;;  Removed unsound type propagation from rules relating to Gaussian integers.
;;;  Changed naming conventions: MIN->LOWER-BOUND, MAX->UPPER-BOUND.
;;;  Added fuzzy comparisons to bounds restrictions.
;;;  Added *MINIMUM-SHRINK-RATIO*.
;;;  Moved type consistency checks in ASSERT!-INTEGERPV etc. to beginning.
;;;  Removed all fuzziness except for RANGE-SIZE. Fuzzy noninteger-real
;;;  variables no longer dereference. REORDER cost function must now return
;;;  non-NIL value for a variable to be forced.
;;;  Fixed =-RULE to `support' complex numbers.
;;;  Fixed CHECK-MEMBERV-RESTRICTION to check for groundness rather than
;;;  variables.
;;;  Fixed RESTRICT-UPPER/LOWER-BOUND! and ASSERT!-INTEGERPV to have integer
;;;  bounds for integer variables fixing (INTEGER-BETWEENV 2.1 2.2) problem.
;;;  Added RESTRICT-BOUNDS!
;;;  Differentiated up versus down rules.
;;;  Version 2.12.
;;; R30Apr92 Qobi
;;;  Added NECESSARILY? and FOR-EFFECTS.
;;;  Changed MAP-VALUES to accept multiple forms.
;;;  Changed PRINT-VALUES and ALL-VALUES to use new version of MAP-VALUES.
;;;  Changed all &BODY BODY to &BODY FORMS.
;;;  Version 2.13
;;; S24May92 Qobi
;;;  Changed failure strategy for INTEGER-BETWEEN and MEMBER-OF.
;;;  Removed (DECLARE (IGNORE FORM)) from NEEDS-SUBSTITUTION?.
;;;  Removed MAP-VALUES and changed all callers to use FOR-EFFECTS.
;;;  Added AN-INTEGER, INTEGER-ABOVE, INTEGER-BELOW, A-REALV and AN-INTEGERV.
;;;  Changed LINEAR-FORCE to no longer require that an integer variable have
;;;  bounds.
;;;  Fixed CPS conversion of TAGBODY so that CPS converted code always
;;;  evaluates to NIL.
;;;  Redid dependency calculations yet again to fix a latent bug.
;;;  Removed error messages so that now can RETURN-FROM or GO to deterministic
;;;  code from nondeterministic code wrapped in a FOR-EFFECTS but not in a
;;;  DEFUN.
;;;  Version 2.14
;;; T26May92 Qobi
;;;  Fixed a bug in the redone dependency calculations.
;;;  Version 2.15
;;; R28May92 Qobi
;;;  Fixed a bug in CHECK-MEMBERV-RESTRICTION that prevented BIND! to a
;;;  variable. Wrapped FORMS in PROGN in ALL-VALUES and PRINT-VALUES to fix a
;;;  bug introduced by the elimination of MAP-VALUES.
;;;  Version 2.16
;;; S14Jun92 Qobi
;;;  Redid fix for CHECK-MEMBERV-RESTRICTION. Fixed a bug in dependency
;;;  calculations so that mutually recursive nondeterministic functions can
;;;  become deterministic. Also fixed bug so that macroexpand without compile
;;;  doesn't cache definition. Redid PRINT-VARIABLE. Changed NON- to NON.
;;;  Fixed bug in EQUALV. Versions of KNOWN?-TRUE-INTERNAL and
;;;  KNOWN?-FALSE-INTERNAL without ASSERT!-BOOLEANPV. Type noticers.
;;;  Fixed bug in RESTRICT-BOUNDS!. Fixed +V2 -V2 *V2 /V2 MINV2 MAXV2
;;;  ASSERT!-=V2 ASSERT!-<=V2 ASSERT!-<V2 ASSERT!-/=V2 to run rule only after
;;;  noticers have been attached.
;;;  Version 2.17
;;; R18Jun92 Qobi
;;;  Completely excised all support for domains containing variables.
;;;  EQUAL and EQL don't use = so fixed bug that EQUALV and friends mistakingly
;;;  used =V for numbers. As a result it is possible for variables to be bound
;;;  to numbers, including complex and real numbers. <=V2 and <V2 asserted
;;;  theirs arguments to be numbers which sould be real. Completely redesigned
;;;  the constraint package. Must walk body of FOR-EFFECTS.
;;;  Version 3.0
;;; W29Jul92 Qobi
;;;  Implemented CGDEMARC's version of AN-INTEGER, AN-INTEGER-ABOVE,
;;;  AN-INTEGER-BELOW, AN-INTEGER-BETWEEN, A-MEMBER-OF-VECTOR and
;;;  A-MEMBER-OF-LIST. Added IScream-based Y-OR-N-P. Fixed stupid efficiency
;;;  bug in ITH-VALUE.
;;;  Version 3.1
;;; W12Aug92 Qobi
;;;  Fixed bug whereby SUPPLIED-P arguments of deterministic surrogate
;;;  functions were not ignored. Added VALUE-OF to all primitives. Made
;;;  A-MEMBER-OF be one function. Exported *MAXIMUM-DISCRETIZATION-RANGE*.
;;;  FIND-BEST now always returns only variables with non-null cost. FIND-BEST
;;;  also will not return a corrupted variable, one where a divide-and-conquer
;;;  step will not reduce the RANGE-SIZE. REORDER-INTERNAL no longer conses.
;;;  DIVIDE-AND-CONQUER-FORCE will stop recursing if a step fails to tighten
;;;  the bound it tried to. Added changes from Volker Haarslev to support
;;;  MCL 2.0. Changed reference from LISP package to COMMON-LISP. The functions
;;;  PRINT-NONDETERMINISTIC-FUNCTION, FAIL, UNWIND-TRAIL, PURGE and
;;;  UNWEDGE-SCREAMER are now defined with DEFUN rather than COMMON-LISP:DEFUN.
;;;  MCL supports ENVIRONMENT argument to MACRO-FUNCTION. Workaround MCL
;;;  MAPHASH bug in CALLERS. Replaced MAPC with DOLIST everywhere.
;;;  DEFSTRUCT-COMPILE-TIME VARIABLE. CHECK-LAMBDA-EXPRESSION integrated into
;;;  LAMBDA-EXPRESSION?. Fixed bug in CPS-CONVERT-TAGBODY. Added DYNAMIC-EXTENT
;;;  optimization.
;;;  Version 3.2
;;; F21Aug92 Qobi
;;;  Changed RESTRICT-ENUMERATED-DOMAIN! and RESTRICT-ENUMERATED-ANTIDOMAIN! to
;;;  call SET-ENUMERATED-DOMAIN! to fix a bug whereby they didn't restrict
;;;  upper and lower bounds. Also fixed many bugs in SHARE!. Fixed bug in MINV2
;;;  and MAXV2 where they used INFINITY-MIN and INFINITY-MAX incorrectly.
;;;  Fixed bug in CORRUPTED? to allow it to work on nonreal variables. Fixed
;;;  bug in FIND-BEST so that it dereferences variables. Removed
;;;  DEFSTRUCT-COMPILE-TIME VARIABLE since fixed the real bug which was that
;;;  GET-SETF-METHOD needed to take the ENVIRONMENT as its argument. Changed
;;;  preamble to conform to new style. Changed many calls to LOOP to TAGBODY
;;;  since MCL 2.0 macroexpands LOOP into MACROLET which WALK can't handle.
;;;  Changed some WHENs to IFs. GET-SETF-METHOD now takes ENVIRONMENT argument.
;;;  Removed extra space in PRINT-VALUES Y-OR-N-P. The functions
;;;  PRINT-NONDETERMINISTIC-FUNCTION, FAIL, UNWIND-TRAIL, PURGE and
;;;  UNWEDGE-SCREAMER are again defined with COMMON-LISP:DEFUN rather than
;;;  DEFUN. Modifications to RESTRICT-LOWER-BOUND!, RESTRICT-UPPER-BOUND! and
;;;  RESTRICT-BOUNDS! which improves efficiency slightly. Changed calls to
;;;  SEQUENCEP to TYPEP SEQUENCE. I don't know why CLtL2 doesn't have
;;;  SEQUENCEP while Lucid does. Lifted generators now take optional NAME
;;;  argument like MAKE-VARIABLE. Changed = 0, < 0 and > 0 to ZEROP, MINUSP
;;;  and PLUSP. Changed INT-CHAR to CODE-CHAR. Changed /-RULE to not divide by
;;;  zero. Also *-RULE-UP/DOWN now just FAIL on divide by zero.
;;;  Version 3.3
;;; W26Aug92 Qobi
;;;  Changed references to COMMON-LISP and COMMON-LISP-USER to CL and CL-USER.
;;;  Added DEFINE-SCREAMER-PACKAGE and modified definition of SCREAMER-USER to
;;;  use it. All calls to GET-SETF-METHOD and MACRO-FUNCTION now pass
;;;  ENVIRONMENT since GENERA 8.1, Lucid 4.0.2 and MCL 2.0 all support this
;;;  argument. Added Kludge to support Lucid 4.0.2 without CLIM 1.1 loaded.
;;;  Added compile-time switch option whereby variables can be represented
;;;  either using DEFSTRUCT or using DEFCLASS. Changed FUTURE-COMMON-LISP to
;;;  LISP since now using Ansi-common-lisp syntax for Symbolics.
;;;  Version 3.4
;;; F11Sep92 Qobi
;;;  Implemented the missing cases of BCP from ANDV and ORV. Changed
;;;  VALUE-OF as per suggestions of Volker Haarslev. Removed check whether
;;;  *QUERY-IO* was same as *TERMINAL-IO* from Y-OR-N-P since *QUERY-IO* is
;;;  usually a synonym stream and Lucid doesn't implement
;;;  SYNONYM-STREAM-SYMBOL and even if it did, there would be no way to
;;;  determine whether or not a steam is a a synonym stream.
;;;  Version 3.5
;;; T27Oct92 Qobi
;;;  ATTACH-NOTICER! now runs it. Load extended LOOP macro for MCL since
;;;  regular MCL LOOP expands into a MACROLET which WALK can't handle.
;;;  Undid change which turned LOOP into TAGBODY. Don't trail unnested LOCAL
;;;  SETF and LOCAL-OUTPUT. Special case BOOLEANS. Fixed bug whereby ANDV
;;;  didn't return NIL when one argument was known to be NIL and ORV didn't
;;;  return T when one argument was known to be T. Added ASSERT!-ORV and
;;;  ASSERT!-NOTV-ANDV optimizations. Fixed a really obscure bug in
;;;  PERFORM-SUBSTITUTIONS where it didn't perform substitutions on a
;;;  RETURN-FROM.
;;;  Version 3.6
;;; W3Nov92 Qobi
;;;  Fixed bug in DETERMINE-WHETHER-CALLERS-ARE-DETERMINISTIC. Fixed the fix
;;;  to the obscure bug in PERFORM-SUBSTITUTIONS. Changed the call to
;;;  CPS-CONVERT inside CPS-CONVERT-RETURN-FROM to pass (FOURTH TAG) as VALUE?
;;;  to fix an obscure bug due to John Eric Fosler.
;;;  Version 3.7
;;; R12Nov92 Qobi
;;;  More efficient ANDV, ORV, ASSERT!-NOTV-ANDV and ASSERT!-ORV. Added
;;;  COUNT-TRUES and COUNT-TRUESV. Fixed bug in TRANSFORM-ASSERT!. Added
;;;  INTERNAL versions of ANDV, ORV, ASSERT!-NOTV-ANDV, ASSERT!-ORV,
;;;  COUNT-TRUES and COUNT-TRUESV. Fixed bug in FUNCALLV and APPLYV. Fixed
;;;  efficiency bug in CPS-CONVERT-CALL. Fixed bug in RESTRICT-INTEGER!.
;;;  Version 3.8
;;; T22Dec92--R25Feb93 Qobi
;;;  Exported REAL, REALP, BOOLEAN and BOOLEANP. Added support for partial
;;;  evaluator. T is now SELF-EVALUATING. Fixed bug in NEEDS-SUBSTITUTION?
;;;  so that NESTED? is T. Fixed CACHE-DEFINITION. Added #||# to IN-PACKAGE.
;;;  Added EVAL-WHEN to REQUIRE :LOOP for MCL. Fixed bug in RESTRICT-VALUE!.
;;;  Version 3.9
;;; M15Mar93 Qobi
;;;  Changed meaning of POLARITY? in KNOWN?-CONSTRAINT, PROPAGATE, and
;;;  ASSERT!-CONSTRAINT so that non-NIL result of FUNCALLV or APPLYV is
;;;  considered to satisfy constraint.
;;;  Version 3.10
;;; S9May93--S11Jul93 Qobi
;;;  Added initial values for LAMBDA-LIST, BODY, ENVIRONMENT, CALLEES, and
;;;  OLD-DETERMINISTIC? of FUNCTION-RECORD to allow to run under Genera 8.1.1.
;;;  Changed WALK of IF to support Genera 8.3. Conditionalized
;;;  SPECIAL-OPERATOR-P and GET-SETF-EXPANSION to support both CLtL2 and dpANS.
;;;  CACHE-DEFINITION and friends no longer save ENVIRONMENT. Got rid of code
;;;  which saved environments in FUNCTION-RECORD in CACHE-ENVIRONMENT and got
;;;  rid of COPY-LIST of environments in DEFUN since that was not portable.
;;;  Added #-POPLOG ENVIRONMENT to calls to GET-SETF-METHOD and MACRO-FUNCTION.
;;;  Added some other conditionalization to support Poplog. Walker
;;;  conditionalization for COND now just used for Explorer and not Allegro.
;;;  Added wraps around MACRO-FUNCTION to support Allegro. Added support for
;;;  nondeterministic functions that return multiple values. Added support for
;;;  AKCL. Fixed efficiency bug in ASSERT!-CONSTRAINT. Fixed error messages
;;;  for FUNCALLV/APPLYV. FUNCALLV/APPLYV now return ground value when
;;;  manifest. Added arc consistency. DEFUN now returns function name.
;;;  Completely obliterated all traces of FUNCTION-RECORD-ENVIRONMENT and
;;;  commented all cases where current rather than saved environment is used.
;;;  Various machinations to get Screamer to run under Harlequin, Allegro, MCL,
;;;  and AKCL. Fixed bugs in ASSERT!-MEMBERV-INTERNAL, ASSERT!-MEMBERV,
;;;  ASSERT!-NOTV-MEMBERV-INTERNAL, and ASSERT!-NOTV-MEMBERV. FUNCALLV and
;;;  APPLYV now propagate to Z when all arguments become bound.
;;;  Version 3.11
;;; S12Jul93 Qobi
;;;  To consolidate version skew on version 3.11.
;;;  Version 3.12
;;; T20Jul93 Qobi
;;;  Fixed bug in -V2 (i.e. (-V2 0 <variable>)) by removing bogus special case.
;;;  Version 3.13
;;; T27Jul93 Qobi
;;;  Since ATTACH-NOTICER! now runs the noticer after attaching it removed the
;;;  cases where the noticers were explicitly run by lifted functions.
;;;  Version 3.14
;;; W22Sep93 Qobi
;;;  Iterate no longer exports FINISH under AKCL since it conflicts with PCL.
;;;  TERMINATE is a synonym anyway.
;;;  Version 3.15
;;; T28Sep93-M4Oct93 Qobi
;;;  Ported to CMU CommonLisp 17b. This change necesitated converting the
;;;  LOOPs in Y-OR-N-P, UNWIND-TRAIL, VALUE-OF, VARIABLIZE, and
;;;  CHOICE-POINT-INTERNAL into TAGBODY/GO combintations since CMU CommonLisp
;;;  expands LOOP into MACROLET. Changed POSSIBLY-BETA-REDUCE-FUNCALL to again
;;;  do SUBST optimization. Changed CPS-CONVERT-BLOCK, CPS-CONVERT-IF,
;;;  CPS-CONVERT-TAGBODY, and CPS-CONVERT-CALL to use
;;;  POSSIBLY-BETA-REDUCE-FUNCALL to encapsulate the *DYNAMIC-EXTENT?*
;;;  interface and fix some efficiency bugs. Even Symbolics port now uses
;;;  MAGIC. Set *DYNAMIC-EXTENT?* to NIL for Symbolics. Added patch files for
;;;  Lucid bug-5511. *TRAIL* now has an initial size of 4096 and a growth rate
;;;  of 1024 so that we don't spend much time growing it on implementations
;;;  where that is inefficient.
;;;  Version 3.16
;;; T26Oct93 Qobi
;;;  PERFORM-SUBSTITUTIONS didn't handle FOR-EFFECTS which caused a bug when
;;;  a deterministic DEFUN contained a FOR-EFFECTS which had a nested LOCAL
;;;  side effect.
;;;  Version 3.17
;;; M22Nov93 Qobi
;;;  Fixed bug in CPS-CONVERT-RETURN-FROM that surfaced due to the previous
;;;  bug fix.
;;;  Version 3.18
;;; M27Dec93 Qobi
;;;  Fixed bug in WHEN-FAILING so that it now nests.
;;;  Version 3.19
;;; T8Mar94 Qobi
;;;  Fixes to make work under Allegro 4.2 and Genera 8.3.
;;;  Future work includes integrating the Allegro\PC and CLISP mods,
;;;  fixing the conditionalization on the DEFTYPE BOOLEAN, and checking that
;;;  the new official DEFTYPE BOOLEAN corresponds to what Screamer expects.
;;;  Version 3.20
;;; T24May94 Qobi
;;;  Added comment stating that Screamer runs under CMU CommonLisp.
;;;  Version 3.21
;;; R26May94 Qobi
;;;  Fixed bug renaming allegro-4.1 to allegro-v4.1.
;;;  Version 3.22
;;; R25Sep97 Qobi
;;;  Changed #-allegro-v4.2 to #-(or allegro-v4.2 x3j13).
;;;  Fixed bug in APPLY-NONDETERMINISTIC-NONDETERMINISTIC
;;;  Version 3.23
;;; M7Sep98 Qobi
;;;  Changed (< (VARIABLE-LOWER-BOUND Y) (VARIABLE-UPPER-BOUND Y)) to
;;;  (<= (VARIABLE-LOWER-BOUND Y) (VARIABLE-UPPER-BOUND Y)) in SHARE! as per
;;;  swhite@csd.abdn.ac.uk
;;;  Version 3.24
;;; F11Jul03 Kevin Rosenberg
;;;  Add gensyms to avoid variable capture in a number of macros
;;;  Version 3.24.1

;;; A kludge to get Screamer to run under Lucid 4.0.2 without CLIM 1.1 loaded
;;; or Poplog or AKCL.
#+(and lucid (not lcl4.1)) (in-package :user)
#-(or (and lucid (not lcl4.1)) poplog akcl) (in-package :cl-user)

;;; A kludge to get Screamer to run under Lucid 4.0.2 without CLIM 1.1 loaded
#+lucid
(eval-when (:compile-toplevel :load-toplevel :execute)
 (unless (find-package :cl)
  (let* ((lisp-package (find-package :lisp))
	 (lisp-package-name (package-name lisp-package))
	 (lisp-package-nicknames (package-nicknames lisp-package))
	 (foo-package (gensym)))
   (rename-package lisp-package foo-package)
   (rename-package lisp-package
		   lisp-package-name
		   (list* :cl
			  :common-lisp
			  lisp-package-nicknames))))
 (unless (find-package :cl-user)
  (let* ((user-package (find-package :user))
	 (user-package-name (package-name user-package))
	 (user-package-nicknames (package-nicknames user-package))
	 (foo-package (gensym)))
   (rename-package user-package foo-package)
   (rename-package user-package
		   user-package-name
		   (list* :cl-user
			  :common-lisp-user
			  user-package-nicknames)))))

;;; MCL needs the extended LOOP macro since the default one expands into
;;; MACROLET which WALK can't handle.
#+mcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :loop))

;;; note: This EVAL-WHEN shouldn't be necessary but it is due to a bug in
;;;       Lucid.
#-(or poplog akcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defpackage :screamer
  (:shadow :defun :multiple-value-bind :y-or-n-p :variable)
  (:use :cl #+lucid :lcl)
  #+:harlequin-common-lisp
  (:shadowing-import-from :ansi-loop :loop :loop-finish)
  (:export :either
	   :fail
	   :local
	   :global
	   :for-effects
	   :multiple-value-call-nondeterministic
	   :one-value
	   :possibly?
	   :necessarily?
	   :all-values
	   :ith-value
	   :print-values
	   :nondeterministic-function?
	   :funcall-nondeterministic
	   :apply-nondeterministic
	   :unwind-trail
	   :purge
	   :unwedge-screamer
	   :local-output
	   :a-boolean
	   :an-integer
	   :an-integer-above
	   :an-integer-below
	   :an-integer-between
	   :a-member-of
	   :when-failing
	   :count-failures
	   :boolean
	   :booleanp
	   :make-variable
	   :numberpv
	   :realpv
	   :integerpv
	   :booleanpv
	   :memberv
	   :assert!
	   :known?
	   :decide
	   :=v
	   :<v
	   :<=v
	   :>v
	   :>=v
	   :/=v
	   :a-booleanv
	   :an-integerv
	   :an-integer-abovev
	   :an-integer-belowv
	   :an-integer-betweenv
	   :a-realv
	   :a-real-abovev
	   :a-real-belowv
	   :a-real-betweenv
	   :a-numberv
	   :a-member-ofv
	   :notv
	   :andv
	   :orv
	   :count-trues
	   :count-truesv
	   :+v
	   :-v
	   :*v
	   :/v
	   :minv
	   :maxv
	   :funcallv
	   :applyv
	   :equalv
	   :bound?
	   :value-of
	   :ground?
	   :apply-substitution
	   :linear-force
	   :divide-and-conquer-force
	   :static-ordering
	   :domain-size
	   :range-size
	   :reorder
	   :solution
	   :best-value
	   :template
	   :define-screamer-package
	   :*screamer-version*
	   :*dynamic-extent?*
	   :*iscream?*
	   :*minimum-shrink-ratio*
	   :*maximum-discretization-range*
	   :*strategy*)))

(in-package :screamer)

#+(or poplog akcl)
(shadow '(defun multiple-value-bind y-or-n-p variable))

#+(or poplog akcl)
(export '(either
	  fail
	  local
	  global
	  for-effects
	  multiple-value-call-nondeterministic
	  one-value
	  possibly?
	  necessarily?
	  all-values
	  ith-value
	  print-values
	  nondeterministic-function?
	  funcall-nondeterministic
	  apply-nondeterministic
	  unwind-trail
	  purge
	  unwedge-screamer
	  local-output
	  a-boolean
	  an-integer
	  an-integer-above
	  an-integer-below
	  an-integer-between
	  a-member-of
	  when-failing
	  count-failures
	  boolean
	  booleanp
	  make-variable
	  numberpv
	  realpv
	  integerpv
	  booleanpv
	  memberv
	  assert!
	  known?
	  decide
	  =v
	  <v
	  <=v
	  >v
	  >=v
	  /=v
	  a-booleanv
	  an-integerv
	  an-integer-abovev
	  an-integer-belowv
	  an-integer-betweenv
	  a-realv
	  a-real-abovev
	  a-real-belowv
	  a-real-betweenv
	  a-numberv
	  a-member-ofv
	  notv
	  andv
	  orv
	  count-trues
	  count-truesv
	  +v
	  -v
	  *v
	  /v
	  minv
	  maxv
	  funcallv
	  applyv
	  equalv
	  bound?
	  value-of
	  ground?
	  apply-substitution
	  linear-force
	  divide-and-conquer-force
	  static-ordering
	  domain-size
	  range-size
	  reorder
	  solution
	  best-value
	  template
	  define-screamer-package
	  *screamer-version*
	  *dynamic-extent?*
	  *iscream?*
	  *minimum-shrink-ratio*
	  *maximum-discretization-range*
	  *strategy*))

#+(or poplog akcl)
(use-package :lisp)

;;; A kludge to get Screamer to run under Poplog or AKCL
#+(or poplog akcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
 (unless (find-package :cl)
  (let* ((lisp-package (find-package :lisp))
	 (lisp-package-name (package-name lisp-package))
	 (lisp-package-nicknames (package-nicknames lisp-package))
	 (foo-package (gensym)))
   (rename-package lisp-package foo-package)
   (rename-package lisp-package
		   lisp-package-name
		   (list* :cl
			  :common-lisp
			  lisp-package-nicknames))))
 (unless (find-package :cl-user)
  (let* ((user-package (find-package :user))
	 (user-package-name (package-name user-package))
	 (user-package-nicknames (package-nicknames user-package))
	 (foo-package (gensym)))
   (rename-package user-package foo-package)
   (rename-package user-package
		   user-package-name
		   (list* :cl-user
			  :common-lisp-user
			  user-package-nicknames)))))

;;; note: Need to remove conditional when Lucid, Poplog, and AKCL
;;;       support CLtL2.
#-(or lucid poplog akcl) (declaim (declaration magic))
#+(or lucid poplog akcl) (proclaim '(declaration magic))

#-(or poplog akcl)
(defmacro define-screamer-package (defined-package-name &rest options)
 ;; note: This EVAL-WHEN shouldn't be necessary but it is due to a bug in
 ;;       Lucid.
 `(eval-when (:compile-toplevel :load-toplevel :execute)
   (defpackage ,defined-package-name ,@options
    (:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p
     #+:harlequin-common-lisp :loop #+:harlequin-common-lisp :loop-finish)
    (:use :cl #+lucid :lcl :screamer))))

#-(or poplog akcl)
(define-screamer-package :screamer-user)

#+(or poplog akcl)
(eval-when (load eval)
 (eval '(make-package :screamer-user :use '(:cl :screamer)))
 (eval
  '(shadowing-import '(defun multiple-value-bind y-or-n-p) :screamer-user)))

(defmacro defstruct-compile-time (options &body items)
 `(eval-when (:compile-toplevel :load-toplevel :execute)
   (defstruct ,options ,@items)))

(defmacro defvar-compile-time (name &optional initial-value documentation)
 `(eval-when (:compile-toplevel :load-toplevel :execute)
   (defvar ,name ,initial-value ,documentation)))

(defmacro defun-compile-time (function-name lambda-list &body body)
 `(eval-when (:compile-toplevel :load-toplevel :execute)
   (cl:defun ,function-name ,lambda-list ,@body)
   #-(or akcl harlequin-common-lisp)
   (eval-when (:compile-toplevel) (compile ',function-name))))

;;; Needed because Allegro has some bogosity whereby (MACRO-FUNCTION <m> <e>)
;;; returns NIL during compile time when <m> is a macro being defined for the
;;; first time in the file being compiled.
(defmacro defmacro-compile-time (function-name lambda-list &body body)
 `(eval-when (:compile-toplevel :load-toplevel :execute)
   (defmacro ,function-name ,lambda-list ,@body)))

(defparameter *screamer-version* "3.24"
 "The version of Screamer which is loaded.")

(defvar-compile-time *dynamic-extent?*
 #-(or poplog akcl symbolics) t #+(or poplog akcl symbolics) nil
 "T to enable the dynamic extent optimization.")

(defvar *iscream?* nil
 "T if Screamer is running under ILisp/GNUEmacs with iscream.el loaded.")

(defvar *nondeterministic?* nil "This must be globally NIL.")

(defvar-compile-time *screamer?* nil
 "This must be NIL except when defining internal Screamer functions.")

(defvar-compile-time *nondeterministic-context?* nil
 "This must be globally NIL.")

(defvar-compile-time *local?* nil "This must be globally NIL.")

(defvar-compile-time *block-tags* '() "This must be globally NIL.")

(defvar-compile-time *tagbody-tags* '() "This must be globally NIL.")

(defvar *trail* (make-array 4096 :adjustable t :fill-pointer 0) "The trail.")

(defvar-compile-time *function-record-table* (make-hash-table :test #'equal)
 "The function record table.")

(defvar-compile-time *ordered-lambda-list-keywords*
 '(&optional &rest &key &allow-other-keys &aux)
 "The allowed lambda list keywords in order.")

(defmacro-compile-time choice-point-internal (form)
 ;; note: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
 ;;       trail?
 `(catch 'fail
    (let ((*nondeterministic?* t))
     (unwind-protect ,form
      (block nil
       (tagbody
	loop
	(if (= (fill-pointer *trail*) trail-pointer) (return))
	(funcall (vector-pop *trail*))
	;; note: This is to allow the trail closures to be garbage
	;;       collected.
	(setf (aref *trail* (fill-pointer *trail*)) nil)
	(go loop)))))))

(defmacro-compile-time choice-point-external (&rest forms)
 ;; note: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
 ;;       trail?
 `(let ((trail-pointer (fill-pointer *trail*))) ,@forms))

(defmacro-compile-time choice-point (form)
 `(choice-point-external (choice-point-internal ,form)))

(defstruct-compile-time function-record
 function-name
 (lambda-list nil)
 (body nil)
 (callees nil)
 (deterministic? t)
 (old-deterministic? nil)
 (screamer? *screamer?*))

(defstruct-compile-time (nondeterministic-function
                         (:print-function print-nondeterministic-function)
                         (:predicate nondeterministic-function?-internal))
 function)

(defun-compile-time screamer-error (header &rest args)
 (apply
  #'error
  (concatenate
   'string
   header
   "~%There are eight types of nondeterministic contexts: the body of a~%~
    function defined with DEFUN, the body of a call to the FOR-EFFECTS~%~
    macro, the first argument of a call to the ONE-VALUE macro, the body of~%~
    a call to the POSSIBLY? macro, the body of a call to the NECESSARILY?~%~
    macro, the body of a call to the ALL-VALUES macro, the second argument~%~
    of a call to the ITH-VALUE macro and the body of a call to the~%~
    PRINT-VALUES macro. Note that, the default forms of &OPTIONAL and &KEY~%~
    arguments and the initialization forms of &AUX variables, are always~%~
    deterministic contexts even though they may appear inside a DEFUN.")
  args))

(defun-compile-time get-function-record (function-name)
 (let ((function-record (gethash function-name *function-record-table*)))
  (unless function-record
   (setf function-record (make-function-record :function-name function-name))
   (setf (gethash function-name *function-record-table*) function-record))
  function-record))

(defun-compile-time peal-off-documentation-string-and-declarations
 (body &optional documentation-string?)
 ;; note: This will need to be done as well for LOCALLY and MACROLET when we
 ;;       eventually implement them.
 ;; needs work: This requires that the documentation string preceed all
 ;;             declarations which needs to be fixed.
 (let (documentation-string declarations)
  (when (and documentation-string?
	     (not (null body))
	     (not (null (rest body)))
	     (stringp (first body)))
   (setf documentation-string (first body))
   (setf body (rest body)))
  (loop (unless (and (not (null body))
		     (consp (first body))
		     (eq (first (first body)) 'declare))
	 (return))
	(push (first body) declarations)
	(pop body))
  (values body (reverse declarations) documentation-string)))

(defun-compile-time self-evaluating? (thing)
 (and (not (consp thing))
      (or (not (symbolp thing))
	  (null thing)
	  (eq thing t)
	  (eq (symbol-package thing) (symbol-package :x)))))

(defun-compile-time quotify (thing)
 (if (self-evaluating? thing) thing `',thing))

(defun-compile-time lambda-expression? (form)
 (and (consp form)
      (or (eq (first form) 'lambda)
	  #+symbolics (eq (first form) 'lisp:lambda))
      (or (and (null (rest (last form)))
	       (>= (length form) 2)
	       (listp (second form)))
	  (error "Invalid syntax for LAMBDA expression: ~S" form))))

(defun-compile-time valid-function-name? (function-name)
 (or (and (symbolp function-name) (not (null function-name)))
     (and (consp function-name)
	  (or (eq (first function-name) 'setf)
	      #+symbolics (eq (first function-name) 'lisp:setf))
	  (null (rest (last function-name)))
	  (= (length function-name) 2)
	  (symbolp (second function-name))
	  (not (null (second function-name))))))

(defun-compile-time check-function-name (function-name)
 (unless (valid-function-name? function-name)
  (error "Invalid function name: ~S" function-name)))

(defun-compile-time every-other (list)
 (cond ((null list) list)
       ((null (rest list)) list)
       (t (cons (first list) (every-other (rest (rest list)))))))

(defun-compile-time check-lambda-list-internal (lambda-list &optional mode)
 (cond
  ((null lambda-list))
  ((member (first lambda-list) *ordered-lambda-list-keywords* :test #'eq)
   (check-lambda-list-internal (rest lambda-list) (first lambda-list)))
  (t (let ((parameter (first lambda-list)))
      (ecase mode
       ((nil)
	(unless (symbolp parameter)
	 (error "Invalid parameter: ~S" parameter)))
       (&optional
	(unless (or (symbolp parameter)
		    (and (consp parameter)
			 (null (rest (last parameter)))
			 (or (= (length parameter) 1)
			     (= (length parameter) 2)
			     (and (= (length parameter) 3)
				  (symbolp (third parameter))))
			 (symbolp (first parameter))))
	 (error "Invalid &OPTIONAL parameter: ~S" parameter)))
       (&rest
	(unless (symbolp parameter)
	 (error "Invalid &REST parameter: ~S" parameter)))
       (&key
	(unless (or (symbolp parameter)
		    (and (consp parameter)
			 (null (rest (last parameter)))
			 (or (= (length parameter) 1)
			     (= (length parameter) 2)
			     (and (= (length parameter) 3)
				  (symbolp (third parameter))))
			 (or (symbolp (first parameter))
			     (and (consp (first parameter))
				  (null (rest (last (first parameter))))
				  (= (length (first parameter)) 2)
				  (symbolp (first (first parameter)))
				  (symbolp (second (first parameter)))))))
	 (error "Invalid &KEY parameter: ~S" parameter)))
       (&aux
	(unless (or (symbolp parameter)
		    (and (consp parameter)
			 (null (rest (last parameter)))
			 (or (= (length parameter) 1)
			     (= (length parameter) 2))
			 (symbolp (first parameter))))
	 (error "Invalid &AUX parameter: ~S" parameter)))))
     (check-lambda-list-internal (rest lambda-list) mode))))

(defun-compile-time check-lambda-list (lambda-list)
 (unless (null (rest (last lambda-list)))
  (error "Improper lambda-list: ~S" lambda-list))
 (let ((rest (member '&rest lambda-list :test #'eq)))
  (if rest
      (let ((rest (rest rest)))
       (unless (not (member '&rest rest :test #'eq))
	(error "&REST cannot appear more than once: ~S" lambda-list))
       (unless (and (not (null rest))
		    (not (member (first rest) lambda-list-keywords :test #'eq))
		    (or (null (rest rest))
			(member (first (rest rest)) lambda-list-keywords
				:test #'eq)))
	(error "&REST must be followed by exactly one variable: ~S"
	       lambda-list)))))
 (let ((allow-other-keys (member '&allow-other-keys lambda-list :test #'eq)))
  (if allow-other-keys
      (unless (or (null (rest allow-other-keys))
		  (member (first (rest allow-other-keys)) lambda-list-keywords
			  :test #'eq))
       (error "&ALLOW-OTHER-KEYS must not be followed by a parameter: ~S"
	      lambda-list))))
 (let ((keywords
	(remove-if-not #'(lambda (argument)
			  (member argument lambda-list-keywords :test #'eq))
		       lambda-list)))
  (unless (every #'(lambda (keyword)
		    (member keyword *ordered-lambda-list-keywords* :test #'eq))
		 keywords)
   (error "Invalid lambda list keyword: ~S" lambda-list))
  (unless (every #'(lambda (x y)
		    (member y (member x *ordered-lambda-list-keywords*
				      :test #'eq)
			    :test #'eq))
		 keywords
		 (rest keywords))
   (error "Invalid order for lambda list keywords: ~S" lambda-list)))
 (check-lambda-list-internal lambda-list))

(defun-compile-time walk-lambda-list-reducing
 (map-function reduce-function screamer? partial? nested? lambda-list
	       environment &optional mode)
 (cond
  ((null lambda-list) (funcall reduce-function))
  ((member (first lambda-list) *ordered-lambda-list-keywords* :test #'eq)
   (walk-lambda-list-reducing map-function
			      reduce-function
			      screamer?
			      partial?
			      nested?
			      (rest lambda-list)
			      environment
			      (first lambda-list)))
  (t (ecase mode
      ((nil &rest &allow-other-keys &aux)
       (walk-lambda-list-reducing map-function
				  reduce-function
				  screamer?
				  partial?
				  nested?
				  (rest lambda-list)
				  environment
				  mode))
      ((&optional &key)
       (if (and (consp (first lambda-list))
		(consp (rest (first lambda-list))))
	   (funcall
	    reduce-function
	    (walk map-function reduce-function screamer? partial? nested?
		  (second (first lambda-list)) environment)
	    (walk-lambda-list-reducing map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       (rest lambda-list)
				       environment
				       mode))
	   (walk-lambda-list-reducing map-function
				      reduce-function
				      screamer?
				      partial?
				      nested?
				      (rest lambda-list)
				      environment
				      mode)))))))

(defun-compile-time walk-lambda-list
 (map-function reduce-function screamer? partial? nested? lambda-list
	       environment)
 (check-lambda-list lambda-list)
 (if reduce-function
     (funcall
      reduce-function
      (funcall map-function lambda-list 'lambda-list)
      (walk-lambda-list-reducing map-function
				 reduce-function
				 screamer?
				 partial?
				 nested?
				 lambda-list
				 environment))
     (funcall map-function lambda-list 'lambda-list)))

(defun-compile-time walk-block
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper BLOCK: ~S" form))
 (unless (>= (length form) 2)
  (error "BLOCK must have at least one argument, a NAME: ~S" form))
 (unless (symbolp (second form)) (error "NAME must be a symbol: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'block)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest (rest form)))))
     (funcall map-function form 'block)))

(defun-compile-time walk-catch
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper PROGN: ~S" form))
 (unless (>= (length form) 2)
  (error "CATCH must have at least one argument, a TAG: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'catch)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest form))))
     (funcall map-function form 'catch)))

(defun-compile-time walk-eval-when
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper EVAL-WHEN: ~S" form))
 (unless (>= (length form) 2)
  (error "EVAL-WHEN must have at least one argument: ~S" form))
 (unless (listp (second form))
  (error "First argument of EVAL-WHEN must be a list: ~S" form))
 (unless (null (rest (last (second form))))
  (error "Improper list of SITUATIONS: ~S" form))
 (unless (every #'(lambda (situation)
		   (member situation '(:compile-toplevel
				       :load-toplevel
				       :execute
				       compile
				       load
				       evel)
			   :test #'eq))
		(second form))
  (error "Invalid SITUATION: ~S" form))
 (if (member :execute (second form) :test #'eq)
     (walk-progn map-function
		 reduce-function
		 screamer?
		 partial?
		 nested?
		 `(progn ,@(rest (rest form)))
		 environment)
     (funcall map-function nil 'quote)))

(defun-compile-time walk-flet/labels
 (map-function reduce-function screamer? partial? nested? form environment
	       form-type)
 (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
 (unless (>= (length form) 2)
  (error "~S must have BINDINGS: ~S" form-type form))
 (unless (and (listp (second form))
	      (null (rest (last (second form))))
	      (every #'(lambda (binding)
			(and (consp binding)
			     (null (rest (last binding)))
			     (>= (length binding) 2)
			     (valid-function-name? (first binding))
			     (listp (second binding))))
		     (second form)))
  (error "Invalid BINDINGS for ~S: ~S" form-type form))
 (if reduce-function
     (funcall
      reduce-function
      (funcall map-function form form-type)
      (if nested?
	  (funcall
	   reduce-function
	   (reduce
	    reduce-function
	    (mapcar
	     #'(lambda (binding)
		(funcall reduce-function
			 (walk-lambda-list map-function
					   reduce-function
					   screamer?
					   partial?
					   nested?
					   (second binding)
					   environment)
			 (mapcar
			  #'(lambda (subform)
			     (walk map-function
				   reduce-function
				   screamer?
				   partial?
				   nested?
				   subform
				   environment))
			  (peal-off-documentation-string-and-declarations
			   (rest (rest binding)) t))))
	     (second form)))
	   (reduce reduce-function
		   (mapcar #'(lambda (subform)
			      (walk map-function
				    reduce-function
				    screamer?
				    partial?
				    nested?
				    subform
				    environment))
			   (rest (rest form)))))
	  (reduce reduce-function
		  (mapcar #'(lambda (subform)
			     (walk map-function
				   reduce-function
				   screamer?
				   partial?
				   nested?
				   subform
				   environment))
			  (rest (rest form))))))
     (funcall map-function form form-type)))

(defun-compile-time walk-function
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper FUNCTION: ~S" form))
 (unless (= (length form) 2)
  (error "FUNCTION must have one argument: ~S" form))
 (cond ((lambda-expression? (second form))
	(if (and reduce-function nested?)
	    (funcall
	     reduce-function
	     (funcall map-function form 'function-lambda)
	     (funcall
	      reduce-function
	      (walk-lambda-list map-function
				reduce-function
				screamer?
				partial?
				nested?
				(second (second form))
				environment)
	      (reduce
	       reduce-function
	       (mapcar #'(lambda (subform)
			  (walk map-function
				reduce-function
				screamer?
				partial?
				nested?
				subform
				environment))
		       (peal-off-documentation-string-and-declarations
			(rest (rest (second form))) t)))))
	    (funcall map-function form 'function-lambda)))
       ((valid-function-name? (second form))
	(cond
	 ((symbolp (second form))
	  (if (or (#+(not (or lucid ansi-90 ansi-cl allegro cmu sbcl clisp gcl))
		     special-form-p
		     #+lucid lisp:special-form-p
		     #+(or ansi-90 ansi-cl allegro cmu sbcl clisp gcl) special-operator-p
		     (second form))
		  ;; note: Allegro has some braindamage in the way it treats
		  ;;       some macros as special forms and refuses to
		  ;;       provide a macro function for them. This
		  ;;       circumvents that problem.
		  (let (#+allegro-v4.1 (sys:*macroexpand-for-compiler* nil))
		   ;; note: Poplog and AKCL only support CLtL1.
		   (macro-function
		    (second form) #-(or poplog akcl) environment)))
	      (error "You can't reference the FUNCTION of a special form or~%~
                      macro: ~S"
		     form))
	  (funcall map-function form 'function-symbol))
	 (t (funcall map-function form 'function-setf))))
       (t (error "Invalid argument to FUNCTION: ~S" form))))

(defun-compile-time walk-go (map-function form)
 (unless (null (rest (last form))) (error "Improper GO: ~S" form))
 (unless (= (length form) 2) (error "GO must have one argument: ~S" form))
 (unless (or (symbolp (second form)) (integerp (second form)))
  (error "TAG of GO must be a symbol or integer: ~S" form))
 (funcall map-function form 'go))

(defun-compile-time walk-if
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper IF: ~S" form))
 (unless (or (= (length form) 3) (= (length form) 4))
  (error "IF must have two or three arguments: ~S" form))
 (if reduce-function
     (if (= (length form) 4)
	 (funcall reduce-function
		  (funcall map-function form 'if)
		  (funcall reduce-function
			   (walk map-function
				 reduce-function
				 screamer?
				 partial?
				 nested?
				 (second form)
				 environment)
			   (funcall reduce-function
				    (walk map-function
					  reduce-function
					  screamer?
					  partial?
					  nested?
					  (third form)
					  environment)
				    (walk map-function
					  reduce-function
					  screamer?
					  partial?
					  nested?
					  (fourth form)
					  environment))))
	 (funcall reduce-function
		  (funcall map-function form 'if)
		  (funcall reduce-function
			   (walk map-function
				 reduce-function
				 screamer?
				 partial?
				 nested?
				 (second form)
				 environment)
			   (walk map-function
				 reduce-function
				 screamer?
				 partial?
				 nested?
				 (third form)
				 environment))))
     (funcall map-function form 'if)))

(defun-compile-time walk-let/let*
 (map-function reduce-function screamer? partial? nested? form environment
	       form-type)
 (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
 (unless (>= (length form) 2)
  (error "~S must have BINDINGS: ~S" form-type form))
 (unless (and (listp (second form))
	      (null (rest (last (second form))))
	      (every #'(lambda (binding)
			(or (symbolp binding)
			    (and (consp binding)
				 (null (rest (last binding)))
				 (or (= (length binding) 1)
				     (= (length binding) 2))
				 (symbolp (first binding)))))
		     (second form)))
  (error "Invalid BINDINGS for ~S: ~S" form-type form))
 (if reduce-function
     (funcall
      reduce-function
      (funcall map-function form form-type)
      (funcall reduce-function
	       (reduce reduce-function
		       (mapcar #'(lambda (binding)
				  (walk map-function
					reduce-function
					screamer?
					partial?
					nested?
					(second binding)
					environment))
			       (remove-if-not
				#'(lambda (binding)
				   (and (consp binding)
					(= (length binding) 2)))
				(second form))))
	       (reduce reduce-function
		       (mapcar #'(lambda (subform)
				  (walk map-function
					reduce-function
					screamer?
					partial?
					nested?
					subform
					environment))
			       (peal-off-documentation-string-and-declarations
				(rest (rest form)))))))
     (funcall map-function form form-type)))

(defun-compile-time walk-multiple-value-call
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form)))
  (error "Improper MULTIPLE-VALUE-CALL: ~S" form))
 (unless (>= (length form) 2)
  (error "MULTIPLE-VALUE-CALL must have at least one argument, a FUNCTION: ~S"
	 form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'multiple-value-call)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest form))))
     (funcall map-function form 'multiple-value-call)))

(defun-compile-time walk-multiple-value-prog1
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form)))
  (error "Improper MULTIPLE-VALUE-PROG1: ~S" form))
 (unless (>= (length form) 2)
  (error "MULTIPLE-VALUE-PROG1 must have at least one argument, a FORM: ~S"
	 form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'multiple-value-prog1)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest form))))
     (funcall map-function form 'multiple-value-prog1)))

(defun-compile-time walk-progn
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper PROGN: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'progn)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest form))))
     (funcall map-function form 'progn)))

(defun-compile-time walk-progv
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper PROGV: ~S" form))
 (unless (>= (length form) 3)
  (error "PROGV must have at least two arguments: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'progv)
	      (funcall reduce-function
		       (funcall reduce-function
				(walk map-function
				      reduce-function
				      screamer?
				      partial?
				      nested?
				      (second form)
				      environment)
				(walk map-function
				      reduce-function
				      screamer?
				      partial?
				      nested?
				      (third form)
				      environment))
		       (reduce reduce-function
			       (mapcar #'(lambda (subform)
					  (walk map-function
						reduce-function
						screamer?
						partial?
						nested?
						subform
						environment))
				       (rest (rest (rest form)))))))
     (funcall map-function form 'progv)))

(defun-compile-time walk-quote (map-function form)
 (unless (null (rest (last form))) (error "Improper QUOTE: ~S" form))
 (unless (= (length form) 2)
  (error "QUOTE must have one argument: ~S" form))
 (funcall map-function (second form) 'quote))

(defun-compile-time walk-return-from
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper RETURN-FROM: ~S" form))
 (unless (or (= (length form) 2) (= (length form) 3))
  (error "RETURN-FROM must have one or two arguments,~%~
          a NAME and an optional RESULT: ~S" form))
 (unless (symbolp (second form)) (error "NAME must be a symbol: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'return-from)
	      (walk map-function
		    reduce-function
		    screamer?
		    partial?
		    nested?
		    (if (= (length form) 3) (third form) nil)
		    environment))
     (funcall map-function form 'return-from)))

(defun-compile-time walk-setq
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper SETQ: ~S" form))
 (unless (every #'symbolp (every-other (rest form)))
  (error "Invalid destination for SETQ: ~S" form))
 (unless (evenp (length (rest form)))
  (error "Odd number of arguments to SETQ: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'setq)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (every-other (rest (rest form))))))
     (funcall map-function form 'setq)))

(defun-compile-time walk-tagbody
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper TAGBODY: ~S" form))
 (unless (every #'(lambda (subform)
		   (or (symbolp subform) (integerp subform) (listp subform)))
		(rest form))
  (error "A subforms of a TAGBODY must be symbols, integers or lists: ~S"
	 form))
 (let ((tags (remove-if #'consp (rest form))))
  (unless (= (length tags) (length (remove-duplicates tags)))
   (error "TAGBODY has duplicate TAGs: ~S" form)))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'tagbody)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (remove-if-not #'consp (rest form)))))
     (funcall map-function form 'tagbody)))

(defun-compile-time walk-the
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper THE: ~S" form))
 (unless (= (length form) 3) (error "THE must have two arguments: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (walk map-function
		    reduce-function
		    screamer?
		    partial?
		    nested?
		    (third form)
		    environment)
	      (funcall map-function form 'the))
     (funcall map-function form 'the)))

(defun-compile-time walk-throw
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper THROW: ~S" form))
 (unless (= (length form) 3)
  (error "THROW must have two arguments, a TAG and a RESULT: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'throw)
	      (funcall reduce-function
		       (walk map-function
			     reduce-function
			     screamer?
			     partial?
			     nested?
			     (second form)
			     environment)
		       (walk map-function
			     reduce-function
			     screamer?
			     partial?
			     nested?
			     (third form)
			     environment)))
     (funcall map-function form 'throw)))

(defun-compile-time walk-unwind-protect
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper UNWIND-PROTECT: ~S" form))
 (unless (>= (length form) 2)
  (error "UNWIND-PROTECT must have at least one argument, a PROTECTED-FORM: ~S"
	 form))
 (if reduce-function
     (funcall
      reduce-function
      (funcall map-function form 'unwind-protect)
      (funcall reduce-function
	       (walk map-function
		     reduce-function
		     screamer?
		     partial?
		     nested?
		     (second form)
		     environment)
	       (reduce reduce-function
		       (mapcar #'(lambda (subform)
				  (walk map-function
					reduce-function
					screamer?
					partial?
					nested?
					subform
					environment))
			       (rest (rest form))))))
     (funcall map-function form 'unwind-protect)))

;;; note: Symbolics needs this to handle DOTIMES.
#+symbolics
(defun-compile-time walk-invisible-references
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form)))
  (error "Improper COMPILER:INVISIBLE-REFERENCES: ~S" form))
 (unless (>= (length form) 2)
  (error "COMPILER:INVISIBLE-REFERENCES must have at least one argument,~%~
          a list of VARIABLES: ~S"
	 form))
 (unless (and (listp (second form)) (every #'symbolp (second form)))
  (error "Invalid VARIABLE list: ~S" form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'compiler:invisible-references)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest (rest form)))))
     (funcall map-function form 'compiler:invisible-references)))

(defun-compile-time walk-for-effects
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper FOR-EFFECTS: ~S" form))
 ;; note: We used to think that we should never walk the body of FOR-EFFECTS
 ;;       as we thought that the walker would get confused on the code
 ;;       generated by FOR-EFFECTS and that FOR-EFFECTS called
 ;;       CPS-CONVERT-PROGN on its body and that CPS-CONVERT-PROGN did the
 ;;       walk for us. But that was wrong since FORM-CALLEES also walks and
 ;;       thus would miss functions called in the body of a FOR-EFFECTS. So now
 ;;       we walk the body of a FOR-EFFECTS without macro-expanding it, but
 ;;       only when NESTED? is true which is essentially only for FORM-CALLEES
 ;;       since DETERMINISTIC? must not walk the body of FOR-EFFECTS or else
 ;;       it will mistakingly report that that a FOR-EFFECTS form is
 ;;       nondeterministic when its body is nondeterministic.
 (if (and reduce-function nested?)
     (funcall reduce-function
	      (funcall map-function form 'for-effects)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest form))))
     (funcall map-function form 'for-effects)))

(defun-compile-time walk-setf
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form))) (error "Improper SETF: ~S" form))
 (unless (evenp (length (rest form)))
  (error "Odd number of arguments to SETF: ~S" form))
 (if *local?*
     (if reduce-function
	 (funcall reduce-function
		  (funcall map-function form 'local-setf)
		  (reduce reduce-function
			  (mapcar #'(lambda (subform)
				     (walk map-function
					   reduce-function
					   screamer?
					   partial?
					   nested?
					   subform
					   environment))
				  (every-other (rest (rest form))))))
	 (funcall map-function form 'local-setf))
     (walk map-function
	   reduce-function
	   screamer?
	   partial?
	   nested?
	   (let ((*macroexpand-hook* #'funcall))
	    (macroexpand-1 form environment))
	   environment)))

(defun-compile-time walk-multiple-value-call-nondeterministic
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form)))
  (error "Improper MULTIPLE-VALUE-CALL-NONDETERMINISTIC: ~S" form))
 (unless (>= (length form) 2)
  (error "MULTIPLE-VALUE-CALL-NONDETERMINISTIC must have at least one ~
          argument, a FUNCTION: ~S"
	 form))
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'multiple-value-call-nondeterministic)
	      (reduce reduce-function
		      (mapcar #'(lambda (subform)
				 (walk map-function
				       reduce-function
				       screamer?
				       partial?
				       nested?
				       subform
				       environment))
			      (rest form))))
     (funcall map-function form 'multiple-value-call-nondeterministic)))

(defun-compile-time walk-full (map-function form)
 (unless (null (rest (last form))) (error "Improper FULL: ~S" form))
 (unless (= (length form) 2)
  (error "FULL must have exactly one argument, a FORM: ~S" form))
 (funcall map-function form 'full))

(defun-compile-time walk-macro-call
 (map-function reduce-function screamer? partial? nested? form environment)
 (if reduce-function
     (funcall reduce-function
	      (funcall map-function form 'macro-call)
	      (walk map-function
		    reduce-function
		    screamer?
		    partial?
		    nested?
		    (let ((*macroexpand-hook* #'funcall))
		     (macroexpand-1 form environment))
		    environment))
     (walk map-function
	   reduce-function
	   screamer?
	   partial?
	   nested?
	   (let ((*macroexpand-hook* #'funcall))
	    (macroexpand-1 form environment))
	   environment)))

(defun-compile-time walk-function-call
 (map-function reduce-function screamer? partial? nested? form environment)
 (unless (null (rest (last form)))
  (error "Improper function call form: ~S" form))
 (cond
  ((lambda-expression? (first form))
   (if reduce-function
       (funcall
	reduce-function
	(funcall map-function form 'lambda-call)
	(funcall
	 reduce-function
	 (reduce reduce-function
		 (mapcar #'(lambda (subform)
			    (walk map-function
				  reduce-function
				  screamer?
				  partial?
				  nested?
				  subform
				  environment))
			 (rest form)))
	 (funcall
	  reduce-function
	  (walk-lambda-list map-function
			    reduce-function
			    screamer?
			    partial?
			    nested?
			    (second (first form))
			    environment)
	  (reduce reduce-function
		  (mapcar #'(lambda (subform)
			     (walk map-function
				   reduce-function
				   screamer?
				   partial?
				   nested?
				   subform
				   environment))
			  (peal-off-documentation-string-and-declarations
			   (rest (rest (first form))) t))))))
       (funcall map-function form 'lambda-call)))
  ((valid-function-name? (first form))
   (if (symbolp (first form))
       (if reduce-function
	   (funcall reduce-function
		    (funcall map-function form 'symbol-call)
		    (reduce reduce-function
			    (mapcar #'(lambda (subform)
				       (walk map-function
					     reduce-function
					     screamer?
					     partial?
					     nested?
					     subform
					     environment))
				    (rest form))))
	   (funcall map-function form 'symbol-call))
       (if reduce-function
	   (funcall reduce-function
		    (funcall map-function form 'setf-call)
		    (reduce reduce-function
			    (mapcar #'(lambda (subform)
				       (walk map-function
					     reduce-function
					     screamer?
					     partial?
					     nested?
					     subform
					     environment))
				    (rest form))))
	   (funcall map-function form 'setf-call))))
  (t (error "CAR of form ~S is not a valid function" form))))

;;; Possible FORM-TYPEs
;;;  Other:
;;;   LAMBDA-LIST VARIABLE
;;;  Special forms:
;;;   BLOCK CATCH EVAL-WHEN FLET FUNCTION-LAMBDA FUNCTION-SYMBOL FUNCTION-SETF
;;;   GO IF LABELS LET LET* MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 PROGN
;;;   PROGV QUOTE RETURN-FROM SETQ TAGBODY THE THROW UNWIND-PROTECT
;;;  Symbolics special forms:
;;;   SYS:VARIABLE-LOCATION COMPILER:INVISIBLE-REFERENCES
;;;  Screamer special forms:
;;;   FOR-EFFECTS LOCAL-SETF
;;;  Partial special forms:
;;;   FULL
;;;  Other:
;;;   MACRO-CALL LAMBDA-CALL SYMBOL-CALL SETF-CALL

(defun-compile-time walk
 (map-function reduce-function screamer? partial? nested? form environment)
 ;; needs work: Cannot walk MACROLET or special forms not in both CLtL1 and
 ;;             CLtL2.
 (cond
  ((self-evaluating? form) (funcall map-function form 'quote))
  ((symbolp form) (funcall map-function form 'variable))
  ((eq (first form) 'block)
   (walk-block
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'catch)
   (walk-catch
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'eval-when)
   (walk-eval-when
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'flet)
   (walk-flet/labels
    map-function reduce-function screamer? partial? nested? form environment
    'flet))
  ((or (eq (first form) 'function)
       #+symbolics (eq (first form) 'lisp:function))
   (walk-function
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'go) (walk-go map-function form))
  ;; Change to support Genera 8.3
  ((or (eq (first form) 'if) #+ansi-90 (eq (first form) 'lisp:if))
   (walk-if map-function reduce-function screamer? partial? nested? form
	    environment))
  ((eq (first form) 'labels)
   (walk-flet/labels
    map-function reduce-function screamer? partial? nested? form environment
    'labels))
  ((eq (first form) 'let)
   (walk-let/let*
    map-function reduce-function screamer? partial? nested? form environment
    'let))
  ((eq (first form) 'let*)
   (walk-let/let*
    map-function reduce-function screamer? partial? nested? form environment
    'let*))
  ;; needs work: This is a temporary kludge to support MCL.
  ((and (eq (first form) 'locally) (null (fourth form)))
   (walk map-function reduce-function screamer? partial? nested? (third form)
	 environment))
  ((eq (first form) 'multiple-value-call)
   (walk-multiple-value-call
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'multiple-value-prog1)
   (walk-multiple-value-prog1
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'progn)
   (walk-progn
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'progv)
   (walk-progv
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'quote) (walk-quote map-function form))
  ((eq (first form) 'return-from)
   (walk-return-from
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'setq)
   (walk-setq
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'tagbody)
   (walk-tagbody
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'the)
   (walk-the
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'throw)
   (walk-throw
    map-function reduce-function screamer? partial? nested? form environment))
  ((eq (first form) 'unwind-protect)
   (walk-unwind-protect
    map-function reduce-function screamer? partial? nested? form environment))
  ;; note: Explorer has some braindamage and treats COND as a special form
  ;;       and is unable to macroexpand it.
  #+explorer
  ((eq (first form) 'cond)
   (unless (null (rest (last form))) (error "Improper COND: ~S" form))
   (unless (or (null (rest form))
	       (and (consp (rest form)) (consp (second form))))
    (error "Improper COND clause: ~S" form))
   (walk map-function
	 reduce-function
	 screamer?
	 partial?
	 nested?
	 (if (null (rest form))
	     nil
	     `(if
	       ,(first (second form))
	       (progn ,@(rest (second form)))
	       (cond ,@(rest (rest form)))))
	 environment))
  #+allegro-v4.1
  ((eq (first form) 'multiple-value-list)
   (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-LIST: ~S" form))
   (unless (= (length form) 2)
    (error "MULTIPLE-VALUE-LIST must have one argument, a FORM: ~S" form))
   (walk map-function
	 reduce-function
	 screamer?
	 partial?
	 nested?
	 `(multiple-value-call #'list ,(second form))
	 environment))
  ;; note: Symbolics needs this to handle LOOP COLLECT clauses.
  #+symbolics
  ((eq (first form) 'sys:variable-location)
   (unless (null (rest (last form)))
    (error "Improper SYS:VARIABLE-LOCATION: ~S" form))
   (unless (= (length form) 2)
    (error "SYS:VARIABLE-LOCATION must have one argument, a VAR: ~S" form))
   (funcall map-function form 'sys:variable-location))
  ;; note: Symbolics needs this to handle DOTIMES.
  #+symbolics
  ((eq (first form) 'compiler:invisible-references)
   (walk-invisible-references
    map-function reduce-function screamer? partial? nested? form environment))
  ((and screamer? (eq (first form) 'for-effects))
   (walk-for-effects
    map-function reduce-function screamer? partial? nested? form environment))
  ((and screamer?
	(or (eq (first form) 'setf)
	    #+symbolics (eq (first form) 'lisp:setf)))
   (walk-setf
    map-function reduce-function screamer? partial? nested? form environment))
  ((and screamer? (eq (first form) 'local))
   (let ((*local?* t))
    (walk-progn
     map-function reduce-function screamer? partial? nested? form
     environment)))
  ((and screamer? (eq (first form) 'global))
   (let ((*local?* nil))
    (walk-progn
     map-function reduce-function screamer? partial? nested? form
     environment)))
  ((and screamer? (eq (first form) 'multiple-value-call-nondeterministic))
   (walk-multiple-value-call-nondeterministic
    map-function reduce-function screamer? partial? nested? form environment))
  ((and partial? (eq (first form) 'full)) (walk-full map-function form))
  ((and (symbolp (first form))
	;; note: Allegro has some braindamage in the way it treats some
	;;       macros as special forms and refuses to provide a macro
	;;       function for them. This circumvents that problem.
	(let (#+allegro-v4.1 (sys:*macroexpand-for-compiler* nil))
	 ;; note: Poplog and AKCL only support CLtL1.
	 (macro-function (first form) #-(or poplog akcl) environment)))
   (walk-macro-call
    map-function reduce-function screamer? partial? nested? form environment))
  ((#+(not (or lucid ansi-90 ansi-cl allegro cmu sbcl clisp gcl)) special-form-p
      #+lucid lisp:special-form-p
      #+(or ansi-90 ansi-cl allegro cmu sbcl clisp gcl) special-operator-p
      (first form))
   (error "Cannot (currently) handle the special form ~S" (first form)))
  (t (walk-function-call
      map-function reduce-function screamer? partial? nested? form
      environment))))

(defun-compile-time process-subforms (function form form-type environment)
 (case form-type
  (lambda-list (error "This shouldn't happen"))
  ((variable go) form)
  ;; note: Symbolics needs this to handle DOTIMES.
  ((eval-when #+symbolics compiler:invisible-references)
   (cons (first form)
	 (cons (second form)
	       (mapcar #'(lambda (subform)
			  (funcall function subform environment))
		       (rest (rest form))))))
  ((flet labels)
   `(,(first form)
     ,(mapcar
       #'(lambda (binding)
	  (cl:multiple-value-bind (body declarations documentation-string)
	    (peal-off-documentation-string-and-declarations
	     (rest (rest binding)) t)
	   `(,(first binding)
	     ;; needs work: To process subforms of lambda list.
	     ,(second binding)
	     ,@(if documentation-string (list documentation-string))
	     ,@declarations
	     ,@(mapcar
		#'(lambda (subform) (funcall function subform environment))
		body))))
       (second form))
     ,@(mapcar
	#'(lambda (subform) (funcall function subform environment))
	(rest (rest form)))))
  ((let let*)
   (cl:multiple-value-bind (body declarations)
     (peal-off-documentation-string-and-declarations (rest (rest form)))
    `(,(first form)
      ,(mapcar
	#'(lambda (binding)
	   (if (and (consp binding) (= (length binding) 2))
	       `(,(first binding)
		 ,(funcall function (second binding) environment))
	       binding))
	(second form))
      ,@declarations
      ,@(mapcar
	 #'(lambda (subform) (funcall function subform environment)) body))))
  (progn
   `(progn ,@(mapcar
	      #'(lambda (subform) (funcall function subform environment))
	      (rest form))))
  (quote (quotify form))
  (the `(the ,(second form) ,(funcall function (third form) environment)))
  (macro-call (error "This shouldn't happen"))
  (lambda-call
   (cl:multiple-value-bind (body declarations documentation-string)
     (peal-off-documentation-string-and-declarations
      (rest (rest (first form))) t)
    ;; needs work: To process subforms of lambda list.
    `((lambda ,(second (first form))
       ,@(if documentation-string (list documentation-string))
       ,@declarations
       ,@(mapcar #'(lambda (subform) (funcall function subform environment))
		 body))
      ,@(mapcar
	 #'(lambda (subform) (funcall function subform environment))
	 (rest form)))))
  (otherwise
   (cons (first form)
	 (mapcar #'(lambda (subform) (funcall function subform environment))
		 (rest form))))))

(defun-compile-time deterministic? (form environment)
 (walk
  #'(lambda (form form-type)
     (case form-type
      ((symbol-call setf-call)
       (function-record-deterministic? (get-function-record (first form))))
      (multiple-value-call-nondeterministic nil)
      ;; note: not really sure about CATCH, THROW and UNWIND-PROTECT
      (otherwise t)))
  ;; note: potentially inefficient because must walk entire form even
  ;;       after it is known to be nondeterministic
  #'(lambda (&optional (x nil x?) y) (if x? (and x y) t))
  t
  nil
  nil
  form
  environment))

(defun-compile-time deterministic-lambda-list? (lambda-list environment)
 (walk-lambda-list
  #'(lambda (form form-type)
     (case form-type
      ((symbol-call setf-call)
       (function-record-deterministic? (get-function-record (first form))))
      (multiple-value-call-nondeterministic nil)
      ;; note: not really sure about CATCH, THROW and UNWIND-PROTECT
      (otherwise t)))
  ;; note: potentially inefficient because must walk entire form even
  ;;       after it is known to be nondeterministic
  #'(lambda (&optional (x nil x?) y) (if x? (and x y) t))
  t
  nil
  nil
  lambda-list
  environment))

(defun-compile-time needs-substitution? (form environment)
 (walk
  #'(lambda (form form-type)
     (case form-type
      (function-lambda
       (not (and (every #'(lambda (form) (deterministic? form environment))
			(peal-off-documentation-string-and-declarations
			 (rest (rest (second form))) t))
		 (deterministic-lambda-list?
		  (second (second form)) environment))))
      ((function-symbol function-setf)
       (not (function-record-deterministic?
	     (get-function-record (second form)))))
      (return-from (let ((tag (assoc (second form) *block-tags* :test #'eq)))
		    (and tag (second tag))))
      (go (let ((tag (assoc (second form) *tagbody-tags*)))
	   (and tag (second tag))))
      (setq *local?*)
      (local-setf t)
      (otherwise nil)))
  ;; note: potentially inefficient because must walk entire form even
  ;;       after it is known to need substitution
  #'(lambda (&optional (x nil x?) y) (if x? (or x y) '()))
  t
  nil
  t
  form
  environment))

(defun-compile-time contains-local-setf/setq? (form environment)
 (walk #'(lambda (form form-type)
	  (declare (ignore form))
	  (or (and *local?* (eq form-type 'setq))
	      (eq form-type 'local-setf)))
       ;; note: potentially inefficient because must walk entire form even
       ;;       after it is known to contain a LOCAL SETF/SETQ special form
       #'(lambda (&optional (x nil x?) y) (if x? (or x y) '()))
       t
       nil
       nil
       form
       environment))

(defun-compile-time form-callees (form environment)
 (walk #'(lambda (form form-type)
	  (case form-type
	   ((function-symbol function-setf) (list (second form)))
	   ((symbol-call setf-call) (list (first form)))
	   (otherwise '())))
       #'(lambda (&optional (x nil x?) y)
	  (if x? (union x y :test #'equal) '()))
       t
       nil
       t
       form
       environment))

(defun-compile-time callees (function-name)
 (function-record-callees (get-function-record function-name)))

(defun-compile-time indirect-callees-internal (function-names callees)
 (if (null function-names)
     callees
     (let ((function-name (first function-names)))
      (if (member function-name callees :test #'equal)
	  (indirect-callees-internal (rest function-names) callees)
	  (indirect-callees-internal
	   (rest function-names)
	   (indirect-callees-internal
	    (callees function-name) (cons function-name callees)))))))

(defun-compile-time indirect-callees (function-name)
 (indirect-callees-internal (callees function-name) '()))

(defun-compile-time callers (function-name)
 (let ((callers '())
       (function-names '()))
  (maphash #'(lambda (function-name function-record)
	      (declare (ignore function-record))
	      (push function-name function-names))
	   *function-record-table*)
  (dolist (caller function-names)
   (if (member function-name (callees caller) :test #'equal)
       (pushnew caller callers :test #'equal)))
  callers))

(defun-compile-time indirect-callers-internal (function-names callers)
 (if (null function-names)
     callers
     (let ((function-name (first function-names)))
      (if (member function-name callers :test #'equal)
	  (indirect-callers-internal (rest function-names) callers)
	  (indirect-callers-internal
	   (rest function-names)
	   (indirect-callers-internal
	    (callers function-name) (cons function-name callers)))))))

(defun-compile-time indirect-callers (function-name)
 (indirect-callers-internal (callers function-name) '()))

(defun-compile-time expand-local-setf (pairs environment)
 (if (null pairs)
     '(progn)
     (let ((d (gensym "DUMMY-"))
	   (dummy-argument (gensym "DUMMY-")))
      (cl:multiple-value-bind (vars vals stores store-form access-form)
	(#+(not (or lucid ansi-90 ansi-cl allegro cmu sbcl clisp gcl)) get-setf-method
	   #+lucid lisp:get-setf-method
	   #+(or ansi-90 ansi-cl allegro cmu sbcl clisp gcl) get-setf-expansion
	   ;; note: Poplog and AKCL only support CLtL1.
	   (first pairs) #-(or poplog akcl) environment)
       `(let* (,@(mapcar #'list vars vals)
		 (,dummy-argument ,(second pairs))
		 (,d ,access-form))
	 (trail #'(lambda () ,(subst d (first stores) store-form)))
	 ,@(if (null (rest (rest pairs)))
	       (list (subst dummy-argument (first stores) store-form))
	       (list (subst dummy-argument (first stores) store-form)
		     (expand-local-setf (rest (rest pairs)) environment))))))))

(defun-compile-time expand-local-setq (pairs environment)
 (if (null pairs)
     '(progn)
     (let ((d (gensym "DUMMY-")))
      `(let ((,d ,(first pairs)))
	(trail #'(lambda () (setq ,(first pairs) ,d)))
	,@(if (null (rest (rest pairs)))
	      (list `(setq
		      ,(first pairs)
		      ,(perform-substitutions (second pairs) environment)))
	      (list `(setq
		      ,(first pairs)
		      ,(perform-substitutions (second pairs) environment))
		    (expand-local-setq (rest (rest pairs)) environment)))))))

(defun-compile-time perform-substitutions (form environment)
 (if (needs-substitution? form environment)
     (walk
      #'(lambda (form form-type)
	 (case form-type
	  (lambda-list (error "This shouldn't happen"))
	  (variable (error "This shouldn't happen"))
	  (block (let ((*block-tags*
			(cons (list (second form) nil) *block-tags*)))
		  (process-subforms
		   #'perform-substitutions form form-type environment)))
	  (function-lambda
	   (unless (deterministic-lambda-list?
		    (second (second form)) environment)
	    (screamer-error
	     "Cannot (currently) handle a LAMDBA expression with~%~
              nondeterministic initializations forms for~%~
              &OPTIONAL and &AUX parameters: ~S"
	     form))
	   (cl:multiple-value-bind (body declarations documentation-string)
	     (peal-off-documentation-string-and-declarations
	      (rest (rest (second form))) t)
	    (if (every #'(lambda (form) (deterministic? form environment))
		       body)
		;; needs work: To process subforms of lambda list.
		`#'(lambda ,(second (second form))
		    ,@(if documentation-string (list documentation-string))
		    ,@declarations
		    ,@(mapcar
		       #'(lambda (subform)
			  (perform-substitutions subform environment))
		       body))
		(let ((continuation (gensym "CONTINUATION-")))
		 ;; note: This conses every time #'(LAMBDA (...) ...) is
		 ;;       accessed when it is nondeterministic. A small
		 ;;       price to pay for a lot of error checking.
		 `(make-nondeterministic-function
		   :function
		   ;; needs work: To process subforms of lambda list.
		   #'(lambda (,continuation ,@(second (second form)))
		      ,@(if documentation-string (list documentation-string))
		      ,@declarations
		      ,continuation	;ignore
		      ,(cps-convert-progn body
					  continuation
					  '()
					  t
					  environment)))))))
	  ((function-symbol function-setf)
	   (if (function-record-deterministic?
		(get-function-record (second form)))
	       form
	       ;; note: This conses every time #'FOO  or #'(SETF FOO) is
	       ;;       accessed when FOO or (SETF FOO) is nondeterministic.
	       ;;       A small price to pay for a lot of error checking.
	       `(make-nondeterministic-function
		 :function #',(cps-convert-function-name (second form)))))
	  (go (let ((tag (assoc (second form) *tagbody-tags*)))
	       ;; note: Can't issue an error here if tag not found since it
	       ;;       might be outside the scope of a FOR-EFFECTS.
	       (if (and tag (second tag)) `(,(second tag)) form)))
	  (quote (error "This shouldn't happen"))
	  (return-from
	   (let ((tag (assoc (second form) *block-tags* :test #'eq))
		 (value (perform-substitutions
			 (if (= (length form) 3) (third form) nil)
			 environment)))
	    ;; note: Can't issue an error here if tag not found since it
	    ;;       might be outside the scope of a FOR-EFFECTS.
	    (if (and tag (second tag))
		(possibly-beta-reduce-funcall
		 (second tag) '() value (fourth tag))
		`(return-from ,(second form) ,value))))
	  (setq (if *local?*
		    (expand-local-setq (rest form) environment)
		    (process-subforms
		     #'perform-substitutions form form-type environment)))
	  (tagbody (let ((*tagbody-tags*
			  (append (mapcar #'(lambda (tag) (list tag nil))
					  (remove-if #'consp (rest form)))
				  *tagbody-tags*)))
		    (process-subforms
		     #'perform-substitutions form form-type environment)))
	  ;; note: Symbolics needs this to handle LOOP COLLECT clauses.
	  #+symbolics
	  (sys:variable-location (error "This shouldn't happen"))
	  (for-effects (perform-substitutions
			(let ((*macroexpand-hook* #'funcall))
			 (macroexpand-1 form environment))
			environment))
	  (local-setf (perform-substitutions
		       (expand-local-setf (rest form) environment)
		       environment))
	  (macro-call (error "This shouldn't happen"))
	  (otherwise (process-subforms
		      #'perform-substitutions form form-type environment))))
      nil
      t
      nil
      nil
      form
      environment)
     form))

(defun-compile-time is-magic-declaration? (form)
 (and (consp form)
      (eq (first form) 'declare)
      (consp (rest form))
      (consp (second form))
      (eq (first (second form)) 'magic)))

(defun-compile-time is-magic-continuation? (continuation)
 ;; Checks that CONTINUATION is of the form:
 ;;   #'(lambda (...) (declare (magic) ...) ...)
 (and (consp continuation)
      (or (eq (first continuation) 'function)
	  #+symbolics (eq (first continuation) 'lisp:function))
      (null (rest (last continuation)))
      (= (length continuation) 2)
      (lambda-expression? (second continuation))
      (>= (length (second continuation)) 3)
      (is-magic-declaration? (third (second continuation)))))

(defun-compile-time magic-continuation-argument (continuation)
 (if (or (eq (first (second (second continuation))) '&optional)
	 (eq (first (second (second continuation))) '&rest))
     (second (second (second continuation)))
     (first (second (second continuation)))))

(defun-compile-time possibly-beta-reduce-funcall
 (continuation types form value?)
 (unless (or (and (symbolp continuation) (not (symbol-package continuation)))
	     (and (consp continuation)
		  (or (eq (first continuation) 'function)
		      #+symbolics (eq (first continuation) 'lisp:function))
		  (null (rest (last continuation)))
		  (= (length continuation) 2)
		  (symbolp (second continuation)))
	     (is-magic-continuation? continuation))
  (error "Please report this bug; This shouldn't happen (A)"))
 (cond
  ((symbolp continuation)
   (if value?
       (if (null types)
	   (if (consp form)
	       `(multiple-value-call ,continuation ,form)
	       ;; note: This optimization is technically unsound if FORM
	       ;;       is a symbol macro that returns multiple values.
	       `(funcall ,continuation ,form))
	   ;; note: This optimization assumes that there are no VALUES
	   ;;       types.
	   `(funcall ,continuation (the (and ,@types) ,form)))
       `(progn ,form (funcall ,continuation))))
  ((symbolp (second continuation))
   (if value?
       (if (null types)
	   (if (consp form)
	       `(multiple-value-call ,continuation ,form)
	       ;; note: This optimization is technically unsound if FORM
	       ;;       is a symbol macro that returns multiple values.
	       `(,(second continuation) ,form))
	   ;; note: This optimization assumes that there are no VALUES
	   ;;       types.
	   `(,(second continuation) (the (and ,@types) ,form)))
       `(progn ,form (,(second continuation)))))
  (t (if value?
	 (progn
	  (if (null (second (second continuation)))
	      (error "Please report this bug; This shouldn't happen (B)"))
	  (cond
	   ((eq (first (second (second continuation))) '&rest)
	    (if (null types)
		`(let ((,(magic-continuation-argument continuation)
			(multiple-value-list ,form)))
		  ;; Peal off LAMBDA, arguments, and DECLARE.
		  ,@(rest (rest (rest (second continuation)))))
		`(let ((,(magic-continuation-argument continuation)
			(list (the (and ,@types) ,form))))
		  ;; Peal off LAMBDA, arguments, and DECLARE.
		  ,@(rest (rest (rest (second continuation)))))))
	   ((or (and (consp form)
		     (not
		      (and (or (eq (first form) 'function)
			       #+symbolics (eq (first form) 'lisp:function))
			   (null (rest (last form)))
			   (= (length form) 2)
			   (symbolp (second form)))))
		(and (symbolp form) (symbol-package form))
		(symbol-package (magic-continuation-argument continuation)))
	    (if (null types)
		`(let ((,(magic-continuation-argument continuation) ,form))
		  ,@(if (and *dynamic-extent?* (is-magic-continuation? form))
			`((declare
			   (dynamic-extent
			    ,(magic-continuation-argument continuation)))))
		  ;; Peal off LAMBDA, arguments, and DECLARE.
		  ,@(rest (rest (rest (second continuation)))))
		`(let ((,(magic-continuation-argument continuation)
			(the (and ,@types) ,form)))
		  (declare
		   (type (and ,@types)
			 ,(magic-continuation-argument continuation)))
		  ;; Peal off LAMBDA, arguments, and DECLARE.
		  ,@(rest (rest (rest (second continuation)))))))
	   ;; note: This case may be unsoundly taken in the following cases:
	   ;;       a. (MAGIC-CONTINUATION-ARGUMENT CONTINUATION) is a
	   ;;          non-Screamer GENSYM. This can only happen if a
	   ;;          a BINDING-VARIABLE is a GENSYM in CPS-CONVERT-LET*.
	   ;;       b. FORM is a non-Screamer GENSYM
	   (t (if (null types)
		  (subst form
			 (magic-continuation-argument continuation)
			 ;; Peal off LAMBDA, arguments, and DECLARE.
			 `(progn ,@(rest (rest (rest (second continuation)))))
			 :test #'eq)
		  (subst `(the (and ,@types) ,form)
			 (magic-continuation-argument continuation)
			 ;; Peal off LAMBDA, arguments, and DECLARE.
			 `(progn ,@(rest (rest (rest (second continuation)))))
			 :test #'eq)))))
	 (progn
	  (unless (null (second (second continuation)))
	   (error "Please report this bug; This shouldn't happen (C)"))
	  ;; Peal off LAMBDA, arguments, and DECLARE.
	  `(progn ,form ,@(rest (rest (rest (second continuation))))))))))

(defun-compile-time void-continuation (continuation)
 (unless (or (and (symbolp continuation) (not (symbol-package continuation)))
	     (and (consp continuation)
		  (or (eq (first continuation) 'function)
		      #+symbolics (eq (first continuation) 'lisp:function))
		  (null (rest (last continuation)))
		  (= (length continuation) 2)
		  (symbolp (second continuation)))
	     (is-magic-continuation? continuation))
  (error "Please report this bug; This shouldn't happen (D)"))
 (let ((dummy-argument (gensym "DUMMY-")))
  ;; note: We could get rid of this bogosity by having two versions of each
  ;;       nondeterministic function, one which returned a value and one which
  ;;       didn't.
  `#'(lambda (&rest ,dummy-argument)
      (declare (magic)
	       #+symbolics (sys:downward-function)
	       (ignore ,dummy-argument))
      ,@(cond ((symbolp continuation) `((funcall ,continuation)))
	      ((symbolp (second continuation)) `((,(second continuation))))
	      ;; Peal off LAMBDA, arguments, and DECLARE.
	      (t (rest (rest (rest (second continuation)))))))))

(defun-compile-time cps-convert-function-name (function-name)
 (if (symbolp function-name)
     (intern (format nil "~A-NONDETERMINISTIC" (string function-name))
	     (symbol-package function-name))
     `(setf ,(intern (format nil "~A-NONDETERMINISTIC"
			     (string (second function-name)))
		     (symbol-package (second function-name))))))

(defun-compile-time cps-convert-block
 (name body continuation types value? environment)
 (let* ((c (gensym "CONTINUATION-"))
	(*block-tags* (cons (list name c types value?) *block-tags*)))
  (possibly-beta-reduce-funcall
   `#'(lambda (,c)
       (declare (magic)	#+symbolics (sys:downward-function))
       ,(cps-convert-progn body c types value? environment))
   '()
   continuation
   t)))

(defun-compile-time cps-convert-if (antecedent
                                    consequent
                                    alternate
                                    continuation
				    types
                                    value?
                                    environment)
 (let ((c (gensym "CONTINUATION-"))
       (dummy-argument (gensym "DUMMY-"))
       (other-arguments (gensym "OTHER-")))
  (possibly-beta-reduce-funcall
   `#'(lambda (,c)
       (declare (magic)	#+symbolics (sys:downward-function))
       ,(cps-convert
	 antecedent
	 `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
	     (declare (magic)
		      #+symbolics (sys:downward-function)
		      (ignore ,other-arguments))
	     (if ,dummy-argument
		 ,(cps-convert consequent c types value? environment)
		 ,(cps-convert alternate c types value? environment)))
	 '()
	 t
	 environment))
   '()
   continuation
   t)))

(defun-compile-time cps-convert-let (bindings
                                     body
                                     declarations
                                     continuation
				     types
                                     value?
                                     environment
                                     &optional
                                     new-bindings)
 (if (null bindings)
     `(let ,new-bindings
       ,@declarations
       ,(cps-convert-progn body continuation types value? environment))
     (let* ((binding (first bindings))
	    (binding-variable
	     (if (symbolp binding) binding (first binding)))
	    (binding-form
	     (if (and (consp binding) (= (length binding) 2))
		 (second binding)
		 nil))
	    (dummy-argument (gensym "DUMMY-"))
	    (other-arguments (gensym "OTHER-")))
      (cps-convert
       binding-form
       `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
	   (declare (magic)
		    #+symbolics (sys:downward-function)
		    (ignore ,other-arguments))
	   ,(cps-convert-let (rest bindings)
			     body
			     declarations
			     continuation
			     types
			     value?
			     environment
			     (cons (list binding-variable dummy-argument)
				   new-bindings)))
       '()
       t
       environment))))

(defun-compile-time cps-convert-let* (bindings
                                      body
                                      declarations
                                      continuation
				      types
                                      value?
                                      environment)
 (if (null bindings)
     (if (null declarations)
	 (cps-convert-progn body continuation types value? environment)
	 `(let ()
	   ,@declarations
	   ,(cps-convert-progn body continuation types value? environment)))
     (let* ((binding (first bindings))
	    (binding-variable
	     (if (symbolp binding) binding (first binding)))
	    (binding-form
	     (if (and (consp binding) (= (length binding) 2))
		 (second binding)
		 nil))
	    (other-arguments (gensym "OTHER-")))
      (cps-convert
       binding-form
       `#'(lambda (&optional ,binding-variable &rest ,other-arguments)
	   (declare (magic)
		    #+symbolics (sys:downward-function)
		    (ignore ,other-arguments))
	   ,(cps-convert-let* (rest bindings)
			      body
			      declarations
			      continuation
			      types
			      value?
			      environment))
       '()
       t
       environment))))

(defun-compile-time cps-convert-multiple-value-call-internal
 (nondeterministic? function forms continuation types value? environment
		    &optional arguments)
 (if (null forms)
     (if nondeterministic?
	 ;; needs work: TYPES is never actually used in this branch.
	 `(apply-nondeterministic-nondeterministic
	   ,(if value? continuation (void-continuation continuation))
	   ,function
	   (append ,@(reverse arguments)))
	 (possibly-beta-reduce-funcall
	  continuation
	  types
	  `(apply ,function (append ,@(reverse arguments)))
	  value?))
     (let ((dummy-argument (gensym "DUMMY-")))
      (cps-convert
       (first forms)
       `#'(lambda (&rest ,dummy-argument)
	   (declare (magic) #+symbolics (sys:downward-function))
	   ,(cps-convert-multiple-value-call-internal
	     nondeterministic? function (rest forms) continuation types value?
	     environment (cons dummy-argument arguments)))
       nil
       t
       environment))))

(defun-compile-time cps-convert-multiple-value-call
 (nondeterministic? function forms continuation types value? environment)
 (let ((dummy-argument (gensym "DUMMY-"))
       (other-arguments (gensym "OTHER-")))
  (cps-convert
   function
   `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
       (declare (magic)
		#+symbolics (sys:downward-function)
		(ignore ,other-arguments))
       ,(cps-convert-multiple-value-call-internal
	 nondeterministic? dummy-argument forms continuation types value?
	 environment))
   nil
   t
   environment)))

(defun-compile-time cps-convert-multiple-value-prog1
 (form forms continuation types value? environment)
 (if value?
     (let ((dummy-argument (gensym "DUMMY-")))
      (cps-convert
       form
       `#'(lambda (&rest ,dummy-argument)
	   (declare (magic) #+symbolics (sys:downward-function))
	   ,(cps-convert-progn
	     forms
	     `#'(lambda ()
		 (declare (magic) #+symbolics (sys:downward-function))
		 (possibly-beta-reduce-funcall
		  continuation types `(values-list ,dummy-argument) t))
	     nil
	     nil
	     environment))
       types
       t
       environment))
     (cps-convert-progn (cons form forms) continuation types nil environment)))

(defun-compile-time cps-convert-progn
 (body continuation types value? environment)
 (cond
  ((null body) (possibly-beta-reduce-funcall continuation types nil value?))
  ((null (rest body))
   (cps-convert (first body) continuation types value? environment))
  (t (cps-convert
      (first body)
      `#'(lambda ()
	  (declare (magic) #+symbolics (sys:downward-function))
	  ,(cps-convert-progn
	    (rest body) continuation types value? environment))
      '()
      nil
      environment))))

(defun-compile-time cps-convert-return-from (name result environment)
 (let ((tag (assoc name *block-tags* :test #'eq)))
  (if (and tag (second tag))
      (cps-convert result (second tag) (third tag) (fourth tag) environment)
      ;; note: Can't issue an error here if tag not found since it might be
      ;;       outside the scope of a FOR-EFFECTS. Thus we must compile a
      ;;       RETURN-FROM nondeterministic code to deterministic code.
      ;;       Likewise, can't issue an error here if tag is found but
      ;;       (SECOND TAG) is NIL since this arrises when you have a
      ;;       RETURN-FROM inside a FOR-EFFECTS to a tag outside the
      ;;       FOR-EFFECTS.
      (let ((dummy-argument (gensym "DUMMY-")))
       (cps-convert
	result
	`#'(lambda (&rest ,dummy-argument)
	    (declare (magic) #+symbolics (sys:downward-function))
	    (return-from ,name (values-list ,dummy-argument)))
	'()
	t
	environment)))))

(defun-compile-time cps-convert-setq
 (arguments continuation types value? environment)
 (if (null arguments)
     (possibly-beta-reduce-funcall continuation types nil value?)
     (let ((dummy-argument (gensym "DUMMY-"))
	   (other-arguments (gensym "OTHER-")))
      (cps-convert
       (second arguments)
       `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
	   (declare (magic)
		    #+symbolics (sys:downward-function)
		    (ignore ,other-arguments)
		    ,@(if (and (null (rest (rest arguments)))
			       (not (null types)))
			  `((type (and ,@types) ,dummy-argument))))
	   ,(if (null (rest (rest arguments)))
		(possibly-beta-reduce-funcall
		 continuation
		 types
		 `(setq ,(first arguments) ,dummy-argument)
		 value?)
		`(progn (setq ,(first arguments) ,dummy-argument)
			,(cps-convert-setq
			  (rest (rest arguments))
			  continuation
			  types
			  value?
			  environment))))
       (if (null (rest (rest arguments))) types '())
       t
       environment))))

(defun-compile-time cps-convert-tagbody
 (body continuation types value? environment)
 (let ((segments (list (list 'header)))
       (*tagbody-tags* *tagbody-tags*))	;cool!
  (dolist (form body)
   (if (consp form)
       (push form (rest (first segments)))
       (let ((c (gensym "CONTINUATION-")))
	(push (list form c) *tagbody-tags*)
	(push (list c) segments))))
  (push nil (rest (first segments)))
  (let ((segments (reverse segments))
	(dummy-argument (gensym "DUMMY-"))
	(other-arguments (gensym "OTHER-")))
   ;; needs work: The closures created by LABELS functions aren't declared to
   ;;             have DYNAMIC-EXTENT since I don't know how to do this in
   ;;             CommonLisp.
   `(labels ,(mapcar
	      #'(lambda (segment)
		 (let ((next (rest (member segment segments :test #'eq))))
		  `(,(first segment)
		    (&optional ,dummy-argument &rest ,other-arguments)
		    (declare (ignore ,dummy-argument ,other-arguments))
		    ,(cps-convert-progn
		      (reverse (rest segment))
		      (if next `#',(first (first next)) continuation)
		      (if next '() types)
		      (or next value?)
		      environment))))
	      (rest segments))
	    ,(let ((next (rest segments)))
	      (cps-convert-progn
	       (reverse (rest (first segments)))
	       (if next `#',(first (first next)) continuation)
	       (if next '() types)
	       (or next value?)
	       environment))))))

(defun-compile-time cps-convert-local-setf/setq
 (arguments continuation types value? environment)
 (if (null arguments)
     (possibly-beta-reduce-funcall continuation types nil value?)
     (let ((d (gensym "DUMMY-"))
	   (dummy-argument (gensym "DUMMY-"))
	   (other-arguments (gensym "OTHER-")))
      (cl:multiple-value-bind (vars vals stores store-form access-form)
	(#+(not (or lucid ansi-90 ansi-cl allegro cmu sbcl clisp gcl)) get-setf-method
	   #+lucid lisp:get-setf-method
	   #+(or ansi-90 ansi-cl allegro cmu sbcl clisp gcl) get-setf-expansion
	   ;; note: Poplog and AKCL only support CLtL1.
	   (first arguments) #-(or poplog akcl) environment)
       (cps-convert
	(second arguments)
	`#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
	    (declare (magic)
		     #+symbolics (sys:downward-function)
		     (ignore ,other-arguments)
		     ,@(if (and (null (rest (rest arguments)))
				(not (null types)))
			   `((type (and ,@types) ,dummy-argument))))
	    (let* (,@(mapcar #'list vars vals) (,d ,access-form))
	     (unwind-protect
	       ,(if (null (rest (rest arguments)))
		    (possibly-beta-reduce-funcall
		     continuation
		     types
		     (subst dummy-argument (first stores) store-form)
		     value?)
		    `(progn ,(subst
			      dummy-argument
			      (first stores)
			      store-form)
			    ,(cps-convert-local-setf/setq
			      (rest (rest arguments))
			      continuation
			      types
			      value?
			      environment)))
	      ,(subst d (first stores) store-form))))
	(if (null (rest (rest arguments))) types '())
	t
	environment)))))

(defun-compile-time cps-convert-call (function-name
                                      arguments
                                      continuation
				      types
                                      value?
                                      environment
                                      &optional
                                      dummy-arguments)
 ;; needs work: TYPES is never actually used here.
 (if (null arguments)
     (let ((c (gensym "CONTINUATION-")))
      (possibly-beta-reduce-funcall
       `#'(lambda (,c)
	   (declare (magic) #+symbolics (sys:downward-function))
	   (,(cps-convert-function-name function-name)
	    ,c
	    ,@(reverse dummy-arguments)))
       '()
       (if value? continuation (void-continuation continuation))
       t))
     (let ((dummy-argument (gensym "DUMMY-"))
	   (other-arguments (gensym "OTHER-")))
      (cps-convert
       (first arguments)
       `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
	   (declare (magic)
		    #+symbolics (sys:downward-function)
		    (ignore ,other-arguments))
	   ,(cps-convert-call
	     function-name
	     (rest arguments)
	     continuation
	     types
	     value?
	     environment
	     (cons dummy-argument dummy-arguments)))
       '()
       t
       environment))))

(defun-compile-time cps-non-convert-call (function-name
                                          arguments
                                          continuation
					  types
                                          value?
                                          environment
                                          &optional
                                          dummy-arguments)
 (if (null arguments)
     (possibly-beta-reduce-funcall
      continuation
      types
      (if (not (null types))
	  `(the (and ,@types) (,function-name ,@(reverse dummy-arguments)))
	  `(,function-name ,@(reverse dummy-arguments)))
      value?)
     (let ((dummy-argument (gensym "DUMMY-"))
	   (other-arguments (gensym "OTHER-")))
      (cps-convert
       (first arguments)
       `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
	   (declare (magic)
		    #+symbolics (sys:downward-function)
		    (ignore ,other-arguments))
	   ,(cps-non-convert-call
	     function-name
	     (rest arguments)
	     continuation
	     types
	     value?
	     environment
	     (cons dummy-argument dummy-arguments)))
       '()
       t
       environment))))

(defun-compile-time cps-convert (form continuation types value? environment)
 (walk #'(lambda (form form-type)
	  (if (and (not (eq form-type 'quote))
		   (deterministic? form environment)
		   (not (contains-local-setf/setq? form environment)))
	      (possibly-beta-reduce-funcall
	       continuation
	       types
	       (perform-substitutions form environment)
	       value?)
	      (case form-type
	       (lambda-list (error "This shouldn't happen"))
	       (variable (possibly-beta-reduce-funcall
			  continuation types form value?))
	       (block (cps-convert-block (second form)
					 (rest (rest form))
					 continuation
					 types
					 value?
					 environment))
	       ((function-lambda function-symbol function-setf)
		(possibly-beta-reduce-funcall
		 continuation
		 types
		 (perform-substitutions form environment)
		 value?))
	       (go (error "This shouldn't happen"))
	       (if (cps-convert-if (second form)
				   (third form)
				   (if (null (rest (rest (rest form))))
				       nil
				       (fourth form))
				   continuation
				   types
				   value?
				   environment))
	       (let (cl:multiple-value-bind (body declarations)
		      (peal-off-documentation-string-and-declarations
		       (rest (rest form)))
		     (cps-convert-let
		      (second form)
		      body
		      declarations
		      continuation
		      types
		      value?
		      environment)))
	       (let* (cl:multiple-value-bind (body declarations)
		       (peal-off-documentation-string-and-declarations
			(rest (rest form)))
		      (cps-convert-let*
		       (second form)
		       body
		       declarations
		       continuation
		       types
		       value?
		       environment)))
	       (multiple-value-call
		(cps-convert-multiple-value-call
		 nil
		 (second form)
		 (rest (rest form))
		 continuation
		 types
		 value?
		 environment))
	       (multiple-value-prog1
		(cps-convert-multiple-value-prog1
		 (second form)
		 (rest (rest form))
		 continuation
		 types
		 value?
		 environment))
	       (progn (cps-convert-progn
		       (rest form) continuation types value? environment))
	       (quote (possibly-beta-reduce-funcall
		       continuation types (quotify form) value?))
	       (return-from (cps-convert-return-from
			     (second form)
			     (if (= (length form) 2) nil (third form))
			     environment))
	       (setq (if *local?*
			 (cps-convert-local-setf/setq
			  (rest form) continuation types value? environment)
			 (cps-convert-setq
			  (rest form) continuation types value? environment)))
	       (tagbody (cps-convert-tagbody
			 (rest form) continuation types value? environment))
	       (the (cps-convert (third form)
				 continuation
				 (cons (second form) types)
				 value?
				 environment))
	       ;; note: Symbolics needs this to handle LOOP COLLECT clauses.
	       #+symbolics
	       (sys:variable-location (error "This shouldn't happen"))
	       ;; note: Symbolics needs this to handle DOTIMES.
	       #+symbolics
	       (compiler:invisible-references
		`(compiler:invisible-references ,(second form)
						,(cps-convert-progn
						  (rest (rest form))
						  continuation
						  types
						  value?
						  environment)))
	       (for-effects (possibly-beta-reduce-funcall
			     continuation types form value?))
	       (local-setf
		(cps-convert-local-setf/setq
		 (rest form) continuation types value? environment))
	       (multiple-value-call-nondeterministic
		(cps-convert-multiple-value-call
		 t
		 (second form)
		 (rest (rest form))
		 continuation
		 types
		 value?
		 environment))
	       (macro-call (error "This shouldn't happen"))
	       (lambda-call
		(unless (deterministic-lambda-list?
			 (second (first form)) environment)
		 (screamer-error
		  "Cannot (currently) handle a LAMDBA expression with~%~
                   nondeterministic initializations forms for~%~
                   &OPTIONAL and &AUX parameters: ~S"
		  form))
		(unless (every
			 #'(lambda (argument)
			    (and (symbolp argument)
				 (not (member argument lambda-list-keywords
					      :test #'eq))))
			 (second (first form)))
		 (error "Cannot (currently) handle a nondeterministic~%~
                         form whose CAR is a LAMBDA expression with~%~
                         lambda list keywords or arguments that are not~%~
                         symbols: ~S"
			form))
		(unless (= (length (second (first form)))
			   (length (rest form)))
		 (error "The form ~S has a CAR which is a LAMBDA~%~
                         expression which takes a different number of~%~
                         arguments than it is called with"
			form))
		(cl:multiple-value-bind (body declarations)
		  (peal-off-documentation-string-and-declarations
		   (rest (rest (first form))) t)
		 ;; note: The documentation string is lost for lambda calls
		 ;;       that are CPS Converted.
		 (cps-convert-let
		  (mapcar #'list (second (first form)) (rest form))
		  body
		  declarations
		  continuation
		  types
		  value?
		  environment)))
	       ((symbol-call setf-call)
		(if (function-record-deterministic?
		     (get-function-record (first form)))
		    (cps-non-convert-call (first form)
					  (rest form)
					  continuation
					  types
					  value?
					  environment)
		    (cps-convert-call (first form)
				      (rest form)
				      continuation
				      types
				      value?
				      environment)))
	       (otherwise
		(screamer-error
		 "Cannot (currently) handle the special form ~S inside a~%~
                  nondeterministic context."
		 (first form))))))
       nil
       t
       nil
       nil
       form
       environment))

(defun-compile-time declare-deterministic (function-name)
 (setf (function-record-deterministic? (get-function-record function-name)) t))

(defun-compile-time declare-nondeterministic (function-name)
 (setf (function-record-deterministic? (get-function-record function-name))
       nil))

(defun-compile-time compute-callees (body environment)
 ;; note: What bogosity in CommonLisp! UNION should allow zero arguments and
 ;;       return NIL as the identity element for use by REDUCE.
 (reduce
  #'union
  (mapcar #'(lambda (form) (form-callees form environment))
	  (peal-off-documentation-string-and-declarations body t))
  :initial-value '()))

(defun-compile-time cache-definition (function-name lambda-list body callees)
 (let ((function-record (get-function-record function-name)))
  (setf (function-record-lambda-list function-record) lambda-list)
  (setf (function-record-body function-record) body)
  (setf (function-record-callees function-record) callees)))

(defun-compile-time determine-whether-deterministic (function-name environment)
 ;; note: This is using the current rather than the saved ENVIRONMENT.
 (let* ((function-record (get-function-record function-name)))
  (setf (function-record-deterministic? function-record)
	(and (every #'(lambda (form) (deterministic? form environment))
		    (peal-off-documentation-string-and-declarations
		     (function-record-body function-record) t))
	     (deterministic-lambda-list?
	      (function-record-lambda-list function-record) environment)))))

(defun-compile-time determine-whether-callers-are-deterministic
 (function-name function-names environment)
 ;; note: This is using the current rather than the saved ENVIRONMENT.
 (dolist (caller (callers function-name))
  (unless (member caller function-names :test #'equal)
   (determine-whether-deterministic caller environment)
   (determine-whether-callers-are-deterministic
    caller (cons caller function-names) environment))))

(defun-compile-time function-definition (function-name environment)
 ;; note: This is using the current rather than the saved ENVIRONMENT.
 (let* ((function-record (get-function-record function-name))
	(lambda-list (function-record-lambda-list function-record))
	(body (function-record-body function-record)))
  (cl:multiple-value-bind (body declarations documentation-string)
    (peal-off-documentation-string-and-declarations body t)
   (if (function-record-deterministic? function-record)
       (let ((*block-tags* (list (list function-name nil))))
	;; needs work: To process subforms of lambda list.
	(list `(cl:defun ,function-name ,lambda-list
		,@(if documentation-string (list documentation-string))
		,@declarations
		,@(mapcar #'(lambda (form)
			     (perform-substitutions form environment))
			  body))
	      `(declare-deterministic ',function-name)))
       (let* ((continuation (gensym "CONTINUATION-"))
	      ;; note: Could provide better TYPES and VALUE? here.
	      (*block-tags* (list (list function-name continuation '() t))))
	(list `(cl:defun ,function-name ,lambda-list
		,@(if documentation-string (list documentation-string))
		,@declarations
		(declare
		 (ignore
		  ,@(reduce
		     #'append
		     (mapcar
		      #'(lambda (argument)
			 (if (consp argument)
			     (if (and (consp (rest argument))
				      (consp (rest (rest argument))))
				 (list (first argument) (third argument))
				 (list (first argument)))
			     (list argument)))
		      (set-difference
		       lambda-list
		       lambda-list-keywords
		       :test #'eq)))))
		(screamer-error
		 "Function ~S is a nondeterministic function. As such, it~%~
                  must be called only from a nondeterministic context."
		 ',function-name))
	      `(cl:defun ,(cps-convert-function-name function-name)
		 (,continuation ,@lambda-list)
		,@(if documentation-string (list documentation-string))
		,@declarations
		,continuation		;ignore
		,(cps-convert-progn body continuation '() t environment))
	      `(declare-nondeterministic ',function-name)))))))

(defun-compile-time modified-function-definitions (function-name environment)
 ;; note: This is using the current rather than the saved ENVIRONMENT.
 (let ((function-record (get-function-record function-name))
       (callers (indirect-callers function-name))
       (function-records '()))
  (setf (function-record-old-deterministic? function-record)
	(function-record-deterministic? function-record))
  (setf (function-record-deterministic? function-record) t)
  (push function-record function-records)
  (dolist (caller callers)
   (let ((function-record (get-function-record caller)))
    (unless (member function-record function-records :test #'eq)
     (setf (function-record-old-deterministic? function-record)
	   (function-record-deterministic? function-record))
     (setf (function-record-deterministic? function-record) t)
     (push function-record function-records))))
  (dolist (caller callers)
   (dolist (callee (callees caller))
    (let ((function-record (get-function-record callee)))
     (unless (member function-record function-records :test #'eq)
      (setf (function-record-old-deterministic? function-record)
	    (function-record-deterministic? function-record))
      (push function-record function-records)))))
  (determine-whether-deterministic function-name environment)
  (determine-whether-callers-are-deterministic function-name nil environment)
  (let ((definitions (function-definition function-name environment)))
   (unless (eq (not (function-record-deterministic? function-record))
	       (not (function-record-old-deterministic? function-record)))
    (dolist (caller callers)
     (if (and (not (equal caller function-name))
	      (some #'(lambda (callee)
		       (let ((function-record (get-function-record callee)))
			(not (eq (not (function-record-deterministic?
				       function-record))
				 (not (function-record-old-deterministic?
				       function-record))))))
		    (callees caller)))
	 (setf definitions
	       (append (function-definition caller environment)
		       definitions)))))
   ;; note: This is so that macroexpand without compile doesn't get out of
   ;;       sync.
   (dolist (function-record function-records)
    (setf (function-record-deterministic? function-record)
	  (function-record-old-deterministic? function-record)))
   definitions)))

;;; The protocol

#+symbolics
(setf (gethash 'defun zwei:*lisp-indentation-offset-hash-table*) '(2 1))

#+symbolics
(setf (get 'defun 'zwei:definition-function-spec-parser)
      (get 'cl:defun 'zwei:definition-function-spec-parser))

(defmacro-compile-time defun
 (function-name lambda-list &body body &environment environment)
 (let ((*nondeterministic-context?* t))
  (check-function-name function-name)
  (let* ((callees (compute-callees body environment))
	 (function-record (get-function-record function-name))
	 (function-record-lambda-list
	  (function-record-lambda-list function-record))
	 (function-record-body (function-record-body function-record))
	 (function-record-callees (function-record-callees function-record))
	 (function-record-deterministic?
	  (function-record-deterministic? function-record))
	 (function-record-old-deterministic?
	  (function-record-old-deterministic? function-record))
	 (function-record-screamer?
	  (function-record-screamer? function-record)))
   (cache-definition function-name lambda-list body callees)
   (let ((modified-function-definitions
	  ;; note: This is using the current rather than the saved ENVIRONMENT.
	  (modified-function-definitions function-name environment)))
    ;; note: This is so that macroexpand without compile doesn't get out of
    ;;       sync.
    (setf (function-record-lambda-list function-record)
	  function-record-lambda-list)
    (setf (function-record-body function-record) function-record-body)
    (setf (function-record-callees function-record)
	  function-record-callees)
    (setf (function-record-deterministic? function-record)
	  function-record-deterministic?)
    (setf (function-record-old-deterministic? function-record)
	  function-record-old-deterministic?)
    (setf (function-record-screamer? function-record)
	  function-record-screamer?)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (cache-definition ',function-name ',lambda-list ',body ',callees)
      ,@modified-function-definitions
      ',function-name)))))

(defmacro-compile-time either (&body forms)
 (case (length forms)
  (0 '(fail))
  (1 (first forms))
  (otherwise `(if (a-boolean) ,(first forms) (either ,@(rest forms))))))

(defmacro-compile-time local (&body forms &environment environment)
 (let ((*local?* t))
  `(progn ,@(mapcar
	     #'(lambda (form) (perform-substitutions form environment))
	     forms))))

(defmacro-compile-time global (&body forms &environment environment)
 (let ((*local?* nil))
  `(progn ,@(mapcar
	     #'(lambda (form) (perform-substitutions form environment))
	     forms))))

(defmacro-compile-time for-effects (&body forms &environment environment)
 `(choice-point
   ,(let ((*nondeterministic-context?* t))
     (cps-convert-progn forms '#'fail nil nil environment))))

(defmacro-compile-time one-value (form1 &optional (form2 nil form2?))
 `(block one-value
   (for-effects (return-from one-value ,form1))
   ,(if form2? form2 '(fail))))

(defmacro-compile-time possibly? (&body forms)
 `(one-value (let ((value (progn ,@forms))) (unless value (fail)) value) nil))

(defmacro-compile-time necessarily? (&body forms)
  (let ((result (gensym "RESULT-"))
	(value (gensym "VALUE-")))
    `(let ((,result t))
      (one-value
       (let ((,value (progn ,@forms)))
	 (when ,value (setf ,result ,value) (fail))
	 ,value)
       ,result))))

(defmacro-compile-time all-values (&body forms)
  (let ((values (gensym "VALUES-"))
	(last-value-cons  (gensym "LAST-VALUE-CONS-"))
	(value (gensym "VALUE-")))
    `(let ((,values '())
	   (,last-value-cons nil))
      (for-effects
       (let ((,value (progn ,@forms)))
	 (global (cond ((null ,values)
			(setf ,last-value-cons (list ,value))
			(setf ,values ,last-value-cons))
		       (t (setf (rest ,last-value-cons) (list ,value))
			  (setf ,last-value-cons (rest ,last-value-cons)))))))
      ,values)))

(defmacro-compile-time ith-value (i form1 &optional (form2 nil form2?))
  (let ((iv (gensym "IV-"))
	(value (gensym "VALUE-")))
    `(block ith-value
      (let ((,iv (value-of ,i)))
	(for-effects (let ((,value ,form1))
		       (if (zerop ,iv) (return-from ith-value ,value))
		       (decf ,iv)))
	,(if form2? form2 '(fail))))))

(defun trail (function)
 ;; note: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
 ;;       trail?
 (if *nondeterministic?* (vector-push-extend function *trail* 1024)))

(defun y-or-n-p
  (&optional (format-string nil format-string?) &rest format-args)
 (cond
  (*iscream?*
   (let ((query (if format-string?
		    (format nil "~A (Y or N): "
			    (apply #'format nil format-string format-args))
		    "(Y or N): ")))
    #-allegro
    (emacs-eval '(y-or-n-p-begin))
    (unwind-protect
      (tagbody
       loop
       (format *query-io* "~%~A" query)
       (let ((char (read-char *query-io*)))
	(when (or (char= char #\y) (char= char #\Y))
	 (format *query-io* "Y")
	 (return-from y-or-n-p t))
	(when (or (char= char #\n) (char= char #\N))
	 (format *query-io* "N")
	 (return-from y-or-n-p nil)))
       (format *query-io* "Please type a single character, Y or N")
       (go loop))
     #-allegro
     (emacs-eval '(y-or-n-p-end)))))
  (format-string? (apply #'cl:y-or-n-p format-string format-args))
  (t (cl:y-or-n-p))))

(defmacro-compile-time print-values (&body forms)
  (let ((value (gensym "VALUE-")))
    `(catch 'succeed
      (for-effects
       (let ((,value (progn ,@forms)))
	 (print ,value)
	 (unless (y-or-n-p "Do you want another solution?")
	   (throw 'succeed ,value)))))))

;;; note: Should have way of having a stream of values.

(eval-when (:compile-toplevel :load-toplevel :execute) (setf *screamer?* t))

(defun print-nondeterministic-function
  (nondeterministic-function stream print-level)
 (declare (ignore print-level))
 (format stream "#<~A ~S>"
	 'nondeterministic
	 (nondeterministic-function-function nondeterministic-function)))

(eval-when (:compile-toplevel :load-toplevel :execute) (declare-nondeterministic 'a-boolean))

(cl:defun a-boolean ()
 (screamer-error
  "A-BOOLEAN is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))

(cl:defun a-boolean-nondeterministic (continuation)
 (choice-point (funcall continuation t))
 (funcall continuation nil))

(defun fail () (throw 'fail nil))

(defmacro-compile-time when-failing ((&body failing-forms) &body forms)
 (let ((old-fail (gensym "FAIL-")))
  `(let ((,old-fail #'fail))
    (unwind-protect
      (progn (setf (symbol-function 'fail)
		   #'(lambda () ,@failing-forms (funcall ,old-fail)))
	     ,@forms)
     (setf (symbol-function 'fail) ,old-fail)))))

(defmacro-compile-time count-failures (&body forms)
 (let ((values (gensym "VALUES-")))
  `(let ((failure-count 0))
    (when-failing ((incf failure-count))
		  (let ((,values (multiple-value-list (progn ,@forms))))
		   (format t "Failures	       = ~10<~;~d~>" failure-count)
		   (values-list ,values))))))

(defun nondeterministic-function? (thing)
 (nondeterministic-function?-internal (value-of thing)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (declare-nondeterministic 'funcall-nondeterministic))

(cl:defun funcall-nondeterministic (function &rest arguments)
 (declare (ignore function arguments))
 (screamer-error
  "FUNCALL-NONDETERMINISTIC is a nondeterministic function. As such, it~%~
   must be called only from a nondeterministic context."))

(cl:defun funcall-nondeterministic-nondeterministic
  (continuation function &rest arguments)
 (let ((function (value-of function)))
  (if (nondeterministic-function? function)
      (apply (nondeterministic-function-function function)
	     continuation
	     arguments)
      (funcall continuation (apply function arguments)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (declare-nondeterministic 'apply-nondeterministic))

(cl:defun apply-nondeterministic (function argument &rest arguments)
 (declare (ignore function argument arguments))
 (screamer-error
  "APPLY-NONDETERMINISTIC is a nondeterministic function. As such, it must~%~
   be called only from a nondeterministic context."))

(cl:defun apply-nondeterministic-nondeterministic
  (continuation function argument &rest arguments)
 (let ((function (value-of function)))
  (if (null arguments)
      (if (nondeterministic-function? function)
          (apply (nondeterministic-function-function function)
	         continuation
                 argument)
          (funcall continuation (apply function argument)))
      (if (nondeterministic-function? function)
          ;; note: I don't know how to avoid the consing here.
          (apply (nondeterministic-function-function function)
	         continuation
	         (apply #'list* (cons argument arguments)))
          (funcall continuation (apply function argument arguments))))))

(defmacro-compile-time multiple-value-bind
 (variables form &body body &environment environment)
 (if (every #'(lambda (form) (deterministic? form environment))
	    (peal-off-documentation-string-and-declarations body))
     `(cl:multiple-value-bind ,variables ,form ,@body)
     (let ((other-arguments (gensym "OTHER-")))
      `(multiple-value-call-nondeterministic
	#'(lambda (&optional ,@variables &rest ,other-arguments)
	   (declare (ignore ,other-arguments))
	   ,@body)
	,form))))

(defun unwind-trail ()
 (tagbody
  loop
  (if (zerop (fill-pointer *trail*)) (return-from unwind-trail))
  (funcall (vector-pop *trail*))
  ;; note: This is to allow the trail closures to be garbage collected.
  (setf (aref *trail* (fill-pointer *trail*)) nil)
  (go loop)))

(defun purge (function-name)
 (remhash (value-of function-name) *function-record-table*)
 t)

(defun unwedge-screamer ()
 (maphash #'(lambda (function-name function-record)
	     (unless (function-record-screamer? function-record)
	      (remhash function-name *function-record-table*)))
	  *function-record-table*)
 t)

;;; note: These optimized versions of AN-INTEGER, AN-INTEGER-ABOVE,
;;;       AN-INTEGER-BELOW, AN-INTEGER-BETWEEN and A-MEMBER-OF have different
;;;       failure behavior as far as WHEN-FAILING is concerned than the
;;;       original purely Screamer versions. This is likely to affect only
;;;       failure counts generated by COUNT-FAILURES. A small price to pay for
;;;       tail recursion optimization.

(eval-when (:compile-toplevel :load-toplevel :execute) (declare-nondeterministic 'an-integer))

(cl:defun an-integer ()
 (screamer-error
  "AN-INTEGER is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun an-integer-nondeterministic (continuation)
 (choice-point-external
  (choice-point-internal (funcall continuation 0))
  (let ((i 1))
   (loop (choice-point-internal (funcall continuation i))
	 (choice-point-internal (funcall continuation (- i)))
	 (incf i)))))

(eval-when (:compile-toplevel :load-toplevel :execute) (declare-nondeterministic 'an-integer-above))

(cl:defun an-integer-above (low)
 (declare (ignore low))
 (screamer-error
  "AN-INTEGER-ABOVE is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-above-nondeterministic (continuation low)
 (let ((low (ceiling (value-of low))))
  (choice-point-external
   (let ((i low))
    (loop (choice-point-internal (funcall continuation i))
	  (incf i))))))

(eval-when (:compile-toplevel :load-toplevel :execute) (declare-nondeterministic 'an-integer-below))

(cl:defun an-integer-below (high)
 (declare (ignore high))
 (screamer-error
  "AN-INTEGER-BELOW is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-below-nondeterministic (continuation high)
 (let ((high (floor (value-of high))))
  (choice-point-external
   (let ((i high))
    (loop (choice-point-internal (funcall continuation i))
	  (decf i))))))

(eval-when (:compile-toplevel :load-toplevel :execute) (declare-nondeterministic 'an-integer-between))

(cl:defun an-integer-between (low high)
 (declare (ignore low high))
 (screamer-error
  "AN-INTEGER-BETWEEN is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-between-nondeterministic (continuation low high)
 (let ((low (ceiling (value-of low)))
       (high (floor (value-of high))))
  (unless (> low high)
   (choice-point-external
    (do ((i low (1+ i))) ((= i high))
	(choice-point-internal (funcall continuation i))))
   (funcall continuation high))))

(eval-when (:compile-toplevel :load-toplevel :execute) (declare-nondeterministic 'a-member-of))

(cl:defun a-member-of (sequence)
 (declare (ignore sequence))
 (screamer-error
  "A-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun a-member-of-nondeterministic (continuation sequence)
 (let ((sequence (value-of sequence)))
  (cond
   ((listp sequence)
    (unless (null sequence)
     (choice-point-external
      (loop (if (null (rest sequence)) (return))
	    (choice-point-internal (funcall continuation (first sequence)))
	    (setf sequence (value-of (rest sequence)))))
     (funcall continuation (first sequence))))
   ((vectorp sequence)
    (let ((n (1- (length sequence))))
     (unless (zerop n)
      (choice-point-external
       (dotimes (i n)
	(choice-point-internal (funcall continuation (aref sequence i)))))
      (funcall continuation (aref sequence n)))))
   (t (error "SEQUENCE must be a sequence")))))

;;; note: The following two functions work only when Screamer is running under
;;;       ILisp/GNUEmacs with iscream.el loaded.

(defun emacs-eval (expression)
 #+allegro
 (lep::eval-in-emacs expression)
 #-allegro
 (unless *iscream?*
  (error "Cannot do EMACS-EVAL unless Screamer is running under~%~
          ILisp/GNUEmacs with iscream.el loaded."))
 #-allegro
 (format *terminal-io* "~A~A~A"
	 (format nil "~A" (code-char 27))
	 (string-downcase (format nil "~A" expression))
	 (format nil "~A" (code-char 29))))

(defmacro-compile-time local-output (&body forms)
 `(progn
   (unless *iscream?*
    (error "Cannot do LOCAL-OUTPUT unless Screamer is running under~%~
            ILisp/GNUEmacs with iscream.el loaded."))
   (trail #'(lambda () (emacs-eval '(pop-end-marker))))
   (emacs-eval '(push-end-marker))
   ,@forms))

;;; Constraints

(defvar *name* 0 "The counter for anonymous names.")

(defvar *minimum-shrink-ratio* 1e-2
 "Ignore propagations which reduce the range of a variable by less than this
   ratio.")

(defvar *maximum-discretization-range* 20
 "Discretize integer variables whose range is not greater than this number.
   Discretize all integer variables if NIL.
   Must be an integer or NIL.")

(defvar *strategy* :gfc
 "Strategy to use for FUNCALLV and APPLYV: either :GFC or :AC")

;;; note: Enable this line to use CLOS instead of DEFSTRUCT for variables.
#|
(eval-when (:compile-toplevel :load-toplevel :execute) (pushnew :screamer-clos *features* :test #'eq))
|#

#-screamer-clos
(defstruct-compile-time (variable (:print-function print-variable)
				  (:predicate variable?)
				  (:constructor make-variable-internal))
 name
 (noticers nil)
 (enumerated-domain t)
 (enumerated-antidomain nil)
 value
 (possibly-integer? t)
 (possibly-noninteger-real? t)
 (possibly-nonreal-number? t)
 (possibly-boolean? t)
 (possibly-nonboolean-nonnumber? t)
 (lower-bound nil)
 (upper-bound nil))

#+screamer-clos
(defclass variable ()
 ((name :accessor variable-name :initarg :name)
  (noticers :accessor variable-noticers :initform nil)
  (enumerated-domain :accessor variable-enumerated-domain :initform t)
  (enumerated-antidomain :accessor variable-enumerated-antidomain
			 :initform nil)
  (value :accessor variable-value)
  (possibly-integer? :accessor variable-possibly-integer? :initform t)
  (possibly-noninteger-real? :accessor variable-possibly-noninteger-real?
			     :initform t)
  (possibly-nonreal-number? :accessor variable-possibly-nonreal-number?
			    :initform t)
  (possibly-boolean? :accessor variable-possibly-boolean? :initform t)
  (possibly-nonboolean-nonnumber?
   :accessor variable-possibly-nonboolean-nonnumber?
   :initform t)
  (lower-bound :accessor variable-lower-bound :initform nil)
  (upper-bound :accessor variable-upper-bound :initform nil)))

#+screamer-clos
(defmethod print-object ((variable variable) stream)
 (print-variable variable stream nil))

#+screamer-clos
(defun-compile-time variable? (thing) (typep thing 'variable))

;;; A kludge to get Screamer to run under Lucid 4.0.2, Poplog, and AKCL
#+(or (and lucid (not lcl4.1)) poplog akcl)
(deftype real () '(or rational float))

;;; A kludge to get Screamer to run under Lucid 4.0.2 and 4.1 as well as
;;; Poplog and AKCL. If you load patch bug-6920.sbin for Lucid 4.1 you MUST
;;; also evaluate (PUSH :BUG-6920 *FEATURES*) to disable this line.
#+(or (and lucid (not bug-6920)) poplog akcl)
(defun realp (x) (typep x 'real))

#-(or allegro-v4.2 x3j13 ansi-cl)
(deftype boolean () '(member t nil))

(defun booleanp (x) (typep x 'boolean))

(defun infinity-min (x y) (and x y (min x y)))

(defun infinity-max (x y) (and x y (max x y)))

(defun infinity-+ (x y) (and x y (+ x y)))

(defun infinity-- (x y) (and x y (- x y)))

(defun infinity-* (x y) (and x y (* x y)))

(defun contains-variables? (x)
 (typecase x
  (cons (or (contains-variables? (car x)) (contains-variables? (cdr x))))
  (variable t)
  (otherwise nil)))

(defun eliminate-variables (x)
 (if (contains-variables? x)
     (if (consp x)
	 (cons (eliminate-variables (car x)) (eliminate-variables (cdr x)))
	 (eliminate-variables (variable-value x)))
     x))

(defun print-variable (x stream print-level)
 (declare (ignore print-level))
 (let ((x (value-of x)))
  (cond
   ((variable? x)
    (if (and (not (eq (variable-enumerated-domain x) t))
	     (not (null (variable-enumerated-antidomain x))))
	(error "This shouldn't happen"))
    (format stream "[~S" (variable-name x))
    (format stream "~A"
	    (cond ((variable-boolean? x) " Boolean")
		  ((variable-integer? x) " integer")
		  ((variable-real? x)
		   (if (variable-noninteger? x) " noninteger-real" " real"))
		  ((variable-number? x)
		   (cond ((variable-nonreal? x) " nonreal-number")
			 ((variable-noninteger? x) " noninteger-number")
			 (t " number")))
		  ((variable-nonnumber? x) " nonnumber")
		  ((variable-nonreal? x) " nonreal")
		  ((variable-noninteger? x) " noninteger")
		  (t "")))
    (if (variable-real? x)
	(if (variable-lower-bound x)
	    (if (variable-upper-bound x)
		(format stream " ~D:~D"
			(variable-lower-bound x) (variable-upper-bound x))
		(format stream " ~D:" (variable-lower-bound x)))
	    (if (variable-upper-bound x)
		(format stream " :~D" (variable-upper-bound x)))))
    (if (and (not (eq (variable-enumerated-domain x) t))
	     (not (variable-boolean? x)))
	(format stream " enumerated-domain:~S"
		(variable-enumerated-domain x)))
    (if (not (null (variable-enumerated-antidomain x)))
	(format stream " enumerated-antidomain:~S"
		(variable-enumerated-antidomain x)))
    (format stream "]"))
   (t (format stream "~S" x)))))

(defun make-variable (&optional (name nil name?))
 (let ((variable
	#-screamer-clos
	(make-variable-internal :name (if name? name (incf *name*)))
	#+screamer-clos
	(make-instance 'variable :name (if name? name (incf *name*)))))
  (setf (variable-value variable) variable)
  variable))

(defun variable-integer? (x)
 (and (not (variable-possibly-boolean? x))
      (not (variable-possibly-nonboolean-nonnumber? x))
      (not (variable-possibly-nonreal-number? x))
      (not (variable-possibly-noninteger-real? x))
      (variable-possibly-integer? x)))

(defun variable-noninteger? (x)
 (and (or (variable-possibly-boolean? x)
	  (variable-possibly-nonboolean-nonnumber? x)
	  (variable-possibly-nonreal-number? x)
	  (variable-possibly-noninteger-real? x))
      (not (variable-possibly-integer? x))))

(defun variable-real? (x)
 (and (not (variable-possibly-boolean? x))
      (not (variable-possibly-nonboolean-nonnumber? x))
      (not (variable-possibly-nonreal-number? x))
      (or (variable-possibly-noninteger-real? x)
	  (variable-possibly-integer? x))))

(defun variable-nonreal? (x)
 (and (or (variable-possibly-boolean? x)
	  (variable-possibly-nonboolean-nonnumber? x)
	  (variable-possibly-nonreal-number? x))
      (not (variable-possibly-noninteger-real? x))
      (not (variable-possibly-integer? x))))

(defun variable-number? (x)
 (and (not (variable-possibly-boolean? x))
      (not (variable-possibly-nonboolean-nonnumber? x))
      (or (variable-possibly-nonreal-number? x)
	  (variable-possibly-noninteger-real? x)
	  (variable-possibly-integer? x))))

(defun variable-nonnumber? (x)
 (and (or (variable-possibly-boolean? x)
	  (variable-possibly-nonboolean-nonnumber? x))
      (not (variable-possibly-nonreal-number? x))
      (not (variable-possibly-noninteger-real? x))
      (not (variable-possibly-integer? x))))

(defun variable-boolean? (x)
 (and (variable-possibly-boolean? x)
      (not (variable-possibly-nonboolean-nonnumber? x))
      (not (variable-possibly-nonreal-number? x))
      (not (variable-possibly-noninteger-real? x))
      (not (variable-possibly-integer? x))))

(defun variable-nonboolean? (x)
 (and (not (variable-possibly-boolean? x))
      (or (variable-possibly-nonboolean-nonnumber? x)
	  (variable-possibly-nonreal-number? x)
	  (variable-possibly-noninteger-real? x)
	  (variable-possibly-integer? x))))

(defun variable-true? (x) (eq (variable-value x) t))

(defun variable-false? (x) (null (variable-value x)))

(defun value-of (x)
 (tagbody
  loop
  (if (or (not (variable? x))
	  #+screamer-clos (not (slot-boundp x 'value))
	  (eq (variable-value x) x))
      (return-from value-of x))
  (setf x (variable-value x))
  (go loop)))

(defun variablize (x)
 (if (variable? x)
     (tagbody
      loop
      (if (or (not (variable? (variable-value x)))
	      (eq (variable-value x) x))
	  (return-from variablize x))
      (setf x (variable-value x))
      (go loop))
     (let ((y (make-variable))) (restrict-value! y x) y)))

(defun bound? (x) (not (variable? (value-of x))))

(defun ground? (x)
 (let ((x (value-of x)))
  (and (not (variable? x))
       (or (not (consp x)) (and (ground? (car x)) (ground? (cdr x)))))))

(defun apply-substitution (x)
 (let ((x (value-of x)))
  (if (consp x)
      (cons (apply-substitution (car x)) (apply-substitution (cdr x)))
      x)))

(defun occurs-in? (x value)
 ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
 ;; note: Will loop if VALUE is circular.
 (cond
  ((eq x value) t)
  ((and (variable? value) (not (eq value (variable-value value))))
   (occurs-in? x (variable-value value)))
  ((consp value) (or (occurs-in? x (car value)) (occurs-in? x (cdr value))))
  (t nil)))

(defun attach-noticer!-internal (noticer x)
 ;; note: Will loop if X is circular.
 (typecase x
  (cons (attach-noticer!-internal noticer (car x))
	(attach-noticer!-internal noticer (cdr x)))
  (variable (if (eq x (variable-value x))
		;; note: I can't remember why this check for duplication is
		;;       here.
		(unless (member noticer (variable-noticers x) :test #'eq)
		 ;; note: This can't be a PUSH because of the Lucid screw.
		 (local (setf (variable-noticers x)
			      (cons noticer (variable-noticers x)))))
		(attach-noticer!-internal noticer (variable-value x))))))

(defun attach-noticer! (noticer x)
 (attach-noticer!-internal noticer x)
 (funcall noticer))

(defun run-noticers (x)
 (dolist (noticer (variable-noticers x)) (funcall noticer)))

;;; Restrictions

(defun restrict-integer! (x)
 ;; note: X must be a variable.
 (unless (variable-possibly-integer? x) (fail))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (variable-possibly-noninteger-real? x)
       (local (setf (variable-possibly-noninteger-real? x) nil))
       (setf run? t))
      (when (variable-possibly-nonreal-number? x)
       (local (setf (variable-possibly-nonreal-number? x) nil))
       (setf run? t))
      (when (variable-possibly-boolean? x)
       (local (setf (variable-possibly-boolean? x) nil))
       (setf run? t))
      (when (variable-possibly-nonboolean-nonnumber? x)
       (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (setf run? t))
      (when (and (variable-lower-bound x)
		 (not (integerp (variable-lower-bound x))))
       (if (and (variable-upper-bound x)
		(< (variable-upper-bound x)
		   (ceiling (variable-lower-bound x))))
	   (fail))
       (local (setf (variable-lower-bound x)
		    (ceiling (variable-lower-bound x))))
       (setf run? t))
      (when (and (variable-upper-bound x)
		 (not (integerp (variable-upper-bound x))))
       (if (and (variable-lower-bound x)
		(> (variable-lower-bound x)
		   (floor (variable-upper-bound x))))
	   (fail))
       (local (setf (variable-upper-bound x) (floor (variable-upper-bound x))))
       (setf run? t))
      (when run?
       (cond ((eq (variable-enumerated-domain x) t)
	      (if (and (variable-lower-bound x)
		       (variable-upper-bound x)
		       (or (null *maximum-discretization-range*)
			   (<= (- (variable-upper-bound x)
				  (variable-lower-bound x))
			       *maximum-discretization-range*)))
		  (set-enumerated-domain!
		   x (all-values (an-integer-between
				  (variable-lower-bound x)
				  (variable-upper-bound x))))))
	     ((not (every #'integerp (variable-enumerated-domain x)))
	      ;; note: Could do less consing if had LOCAL DELETE-IF.
	      ;;       This would also allow checking list only once.
	      (set-enumerated-domain!
	       x (remove-if-not #'integerp (variable-enumerated-domain x)))))
       (run-noticers x)))))

(defun restrict-noninteger! (x)
 ;; note: X must be a variable.
 (unless (or (variable-possibly-noninteger-real? x)
	     (variable-possibly-nonreal-number? x)
	     (variable-possibly-boolean? x)
	     (variable-possibly-nonboolean-nonnumber? x))
  (fail))
 (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
	    (variable-possibly-integer? x))
  (local (setf (variable-possibly-integer? x) nil))
  (if (and (not (eq (variable-enumerated-domain x) t))
	   (some #'integerp (variable-enumerated-domain x)))
      ;; note: Could do less consing if had LOCAL DELETE-IF.
      ;;       This would also allow checking list only once.
      (set-enumerated-domain!
       x (remove-if #'integerp (variable-enumerated-domain x))))
  (run-noticers x)))

(defun restrict-real! (x)
 ;; note: X must be a variable.
 (unless (or (variable-possibly-integer? x)
	     (variable-possibly-noninteger-real? x))
  (fail))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (variable-possibly-nonreal-number? x)
       (local (setf (variable-possibly-nonreal-number? x) nil))
       (setf run? t))
      (when (variable-possibly-boolean? x)
       (local (setf (variable-possibly-boolean? x) nil))
       (setf run? t))
      (when (variable-possibly-nonboolean-nonnumber? x)
       (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (setf run? t))
      (when run?
       (if (and (not (eq (variable-enumerated-domain x) t))
		(not (every #'realp (variable-enumerated-domain x))))
	   ;; note: Could do less consing if had LOCAL DELETE-IF.
	   ;;       This would also allow checking list only once.
	   (set-enumerated-domain!
	    x (remove-if-not #'realp (variable-enumerated-domain x))))
       (run-noticers x)))))

(defun restrict-nonreal! (x)
 ;; note: X must be a variable.
 (unless (or (variable-possibly-nonreal-number? x)
	     (variable-possibly-boolean? x)
	     (variable-possibly-nonboolean-nonnumber? x))
  (fail))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (variable-possibly-integer? x)
       (local (setf (variable-possibly-integer? x) nil))
       (setf run? t))
      (when (variable-possibly-noninteger-real? x)
       (local (setf (variable-possibly-noninteger-real? x) nil))
       (setf run? t))
      (when run?
       (if (and (not (eq (variable-enumerated-domain x) t))
		(some #'realp (variable-enumerated-domain x)))
	   ;; note: Could do less consing if had LOCAL DELETE-IF.
	   ;;       This would also allow checking list only once.
	   (set-enumerated-domain!
	    x (remove-if #'realp (variable-enumerated-domain x))))
       (run-noticers x)))))

(defun restrict-number! (x)
 ;; note: X must be a variable.
 (unless (or (variable-possibly-integer? x)
	     (variable-possibly-noninteger-real? x)
	     (variable-possibly-nonreal-number? x))
  (fail))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (variable-possibly-boolean? x)
       (local (setf (variable-possibly-boolean? x) nil))
       (setf run? t))
      (when (variable-possibly-nonboolean-nonnumber? x)
       (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (setf run? t))
      (when run?
       (if (and (not (eq (variable-enumerated-domain x) t))
		(not (every #'numberp (variable-enumerated-domain x))))
	   ;; note: Could do less consing if had LOCAL DELETE-IF.
	   ;;       This would also allow checking list only once.
	   (set-enumerated-domain!
	    x (remove-if-not #'numberp (variable-enumerated-domain x))))
       (run-noticers x)))))

(defun restrict-nonnumber! (x)
 ;; note: X must be a variable.
 (unless (or (variable-possibly-boolean? x)
	     (variable-possibly-nonboolean-nonnumber? x))
  (fail))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (variable-possibly-integer? x)
       (local (setf (variable-possibly-integer? x) nil))
       (setf run? t))
      (when (variable-possibly-noninteger-real? x)
       (local (setf (variable-possibly-noninteger-real? x) nil))
       (setf run? t))
      (when (variable-possibly-nonreal-number? x)
       (local (setf (variable-possibly-nonreal-number? x) nil))
       (setf run? t))
      (when run?
       (if (and (not (eq (variable-enumerated-domain x) t))
		(some #'numberp (variable-enumerated-domain x)))
	   ;; note: Could do less consing if had LOCAL DELETE-IF.
	   ;;       This would also allow checking list only once.
	   (set-enumerated-domain!
	    x (remove-if #'numberp (variable-enumerated-domain x))))
       (run-noticers x)))))

(defun restrict-boolean! (x)
 ;; note: X must be a variable.
 (unless (variable-possibly-boolean? x) (fail))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (variable-possibly-integer? x)
       (local (setf (variable-possibly-integer? x) nil))
       (setf run? t))
      (when (variable-possibly-noninteger-real? x)
       (local (setf (variable-possibly-noninteger-real? x) nil))
       (setf run? t))
      (when (variable-possibly-nonreal-number? x)
       (local (setf (variable-possibly-nonreal-number? x) nil))
       (setf run? t))
      (when (variable-possibly-nonboolean-nonnumber? x)
       (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (setf run? t))
      (when run?
       (cond
	((eq (variable-enumerated-domain x) t)
	 (local
	  (cond
	   ((member t (variable-enumerated-antidomain x) :test #'eq)
	    (cond ((member nil (variable-enumerated-antidomain x) :test #'eq)
		   (fail))
		  (t (setf (variable-enumerated-domain x) '(nil))
		     (setf (variable-enumerated-antidomain x) '())
		     (setf (variable-value x) nil))))
	   ((member nil (variable-enumerated-antidomain x) :test #'eq)
	    (setf (variable-enumerated-domain x) '(t))
	    (setf (variable-enumerated-antidomain x) '())
	    (setf (variable-value x) t))
	   (t (setf (variable-enumerated-domain x) '(t nil))
	      (unless (null (variable-enumerated-antidomain x))
	       (setf (variable-enumerated-antidomain x) '()))))))
	((not (every #'booleanp (variable-enumerated-domain x)))
	 ;; note: Could do less consing if had LOCAL DELETE-IF.
	 ;;       This would also allow checking list only once.
	 (set-enumerated-domain!
	  x (remove-if-not #'booleanp (variable-enumerated-domain x)))))
       (run-noticers x)))))

(defun restrict-nonboolean! (x)
 ;; note: X must be a variable.
 (unless (or (variable-possibly-integer? x)
	     (variable-possibly-noninteger-real? x)
	     (variable-possibly-nonreal-number? x)
	     (variable-possibly-nonboolean-nonnumber? x))
  (fail))
 (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
	    (variable-possibly-boolean? x))
  (local (setf (variable-possibly-boolean? x) nil))
  (cond ((eq (variable-enumerated-domain x) t)
	 (local (setf (variable-enumerated-antidomain x)
		      (adjoin t
			      (adjoin nil (variable-enumerated-antidomain x)
				      :test #'eq)
			      :test #'eq))))
	((some #'booleanp (variable-enumerated-domain x))
	 ;; note: Could do less consing if had LOCAL DELETE-IF.
	 ;;       This would also allow checking list only once.
	 (set-enumerated-domain!
	  x (remove-if #'booleanp (variable-enumerated-domain x)))))
  (run-noticers x)))

(defun restrict-lower-bound! (x lower-bound)
 ;; note: X must be a variable.
 ;; note: LOWER-BOUND must be a real constant.
 (if (variable-integer? x) (setf lower-bound (ceiling lower-bound)))
 (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
	    (or (not (variable-lower-bound x))
		(> lower-bound (variable-lower-bound x))))
  (if (and (variable-upper-bound x) (< (variable-upper-bound x) lower-bound))
      (fail))
  (when (or (not (variable-lower-bound x))
	    (not (variable-upper-bound x))
	    (>= (/ (- lower-bound (variable-lower-bound x))
		   (- (variable-upper-bound x) (variable-lower-bound x)))
		*minimum-shrink-ratio*))
   (local (setf (variable-lower-bound x) lower-bound))
   (cond ((eq (variable-enumerated-domain x) t)
	  (if (and lower-bound
		   (variable-upper-bound x)
		   (variable-integer? x)
		   (or (null *maximum-discretization-range*)
		       (<= (- (variable-upper-bound x) lower-bound)
			   *maximum-discretization-range*)))
	      (set-enumerated-domain!
	       x (all-values (an-integer-between lower-bound
						 (variable-upper-bound x))))))
	 ((some #'(lambda (element) (< element lower-bound))
		(variable-enumerated-domain x))
	  ;; note: Could do less consing if had LOCAL DELETE-IF.
	  ;;       This would also allow checking list only once.
	  (set-enumerated-domain!
	   x (remove-if #'(lambda (element) (< element lower-bound))
			(variable-enumerated-domain x)))))
   (run-noticers x))))

(defun restrict-upper-bound! (x upper-bound)
 ;; note: X must be a variable.
 ;; note: UPPER-BOUND must be a real constant.
 (if (variable-integer? x) (setf upper-bound (floor upper-bound)))
 (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
	    (or (not (variable-upper-bound x))
		(< upper-bound (variable-upper-bound x))))
  (if (and (variable-lower-bound x) (> (variable-lower-bound x) upper-bound))
      (fail))
  (when (or (not (variable-lower-bound x))
	    (not (variable-upper-bound x))
	    (>= (/ (- (variable-upper-bound x) upper-bound)
		   (- (variable-upper-bound x) (variable-lower-bound x)))
		*minimum-shrink-ratio*))
   (local (setf (variable-upper-bound x) upper-bound))
   (cond ((eq (variable-enumerated-domain x) t)
	  (if (and (variable-lower-bound x)
		   upper-bound
		   (variable-integer? x)
		   (or (null *maximum-discretization-range*)
		       (<= (- upper-bound (variable-lower-bound x))
			   *maximum-discretization-range*)))
	      (set-enumerated-domain!
	       x (all-values (an-integer-between (variable-lower-bound x)
						 upper-bound)))))
	 ((some #'(lambda (element) (> element upper-bound))
		(variable-enumerated-domain x))
	  ;; note: Could do less consing if had LOCAL DELETE-IF.
	  ;;       This would also allow checking list only once.
	  (set-enumerated-domain!
	   x (remove-if #'(lambda (element) (> element upper-bound))
			(variable-enumerated-domain x)))))
   (run-noticers x))))

(defun restrict-bounds! (x lower-bound upper-bound)
 ;; note: X must be a variable.
 ;; note: LOWER-BOUND and UPPER-BOUND must be real constants.
 (when (variable-integer? x)
  (if lower-bound (setf lower-bound (ceiling lower-bound)))
  (if upper-bound (setf upper-bound (floor upper-bound))))
 (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
     (let ((run? nil))
      (when (and lower-bound
		 (or (not (variable-lower-bound x))
		     (> lower-bound (variable-lower-bound x))))
       (if (and (variable-upper-bound x)
		(< (variable-upper-bound x) lower-bound))
	   (fail))
       (when (or (not (variable-lower-bound x))
	         (not (variable-upper-bound x))
	         (>= (/ (- lower-bound (variable-lower-bound x))
		        (- (variable-upper-bound x) (variable-lower-bound x)))
		     *minimum-shrink-ratio*))
	(local (setf (variable-lower-bound x) lower-bound))
	(setf run? t)))
      (when (and upper-bound
		 (or (not (variable-upper-bound x))
		     (< upper-bound (variable-upper-bound x))))
       (if (and (variable-lower-bound x)
		(> (variable-lower-bound x) upper-bound))
	   (fail))
       (when (or (not (variable-lower-bound x))
	         (not (variable-upper-bound x))
	         (>= (/ (- (variable-upper-bound x) upper-bound)
		        (- (variable-upper-bound x) (variable-lower-bound x)))
		     *minimum-shrink-ratio*))
	(local (setf (variable-upper-bound x) upper-bound))
	(setf run? t)))
      (when run?
       (cond ((eq (variable-enumerated-domain x) t)
	      (if (and (variable-lower-bound x)
		       (variable-upper-bound x)
		       (variable-integer? x)
		       (or (null *maximum-discretization-range*)
			   (<= (- (variable-upper-bound x)
				  (variable-lower-bound x))
			       *maximum-discretization-range*)))
		  (set-enumerated-domain!
		   x (all-values (an-integer-between
				  (variable-lower-bound x)
				  (variable-upper-bound x))))))
	     ((or (and lower-bound
		       (some #'(lambda (element) (< element lower-bound))
			     (variable-enumerated-domain x)))
		  (and upper-bound
		       (some #'(lambda (element) (> element upper-bound))
			     (variable-enumerated-domain x))))
	      ;; note: Could do less consing if had LOCAL DELETE-IF.
	      ;;       This would also allow checking list only once.
	      (set-enumerated-domain!
	       x (remove-if #'(lambda (element)
			       (or (and lower-bound (< element lower-bound))
				   (and upper-bound (> element upper-bound))))
			    (variable-enumerated-domain x)))))
       (run-noticers x)))))

(defun share! (x y)
 ;; note: X and Y must be variables such that (EQ X (VALUE-OF X)) and
 ;;       (EQ Y (VALUE-OF Y)).
 (let ((run? nil)
       (y-lower-bound? nil)
       (y-upper-bound? nil)
       (x-lower-bound (variable-lower-bound x))
       (x-upper-bound (variable-upper-bound x))
       (y-lower-bound (variable-lower-bound y))
       (y-upper-bound (variable-upper-bound y)))
  (cond ((and (variable-integer? y) (not (variable-integer? x)))
	 (if x-lower-bound (setf x-lower-bound (ceiling x-lower-bound)))
	 (if x-upper-bound (setf x-upper-bound (floor x-upper-bound))))
	((and (not (variable-integer? y)) (variable-integer? x))
	 (when (and y-lower-bound (not (integerp y-lower-bound)))
	  (setf y-lower-bound (ceiling y-lower-bound))
	  (setf y-lower-bound? t))
	 (when (and y-upper-bound (not (integerp y-upper-bound)))
	  (setf y-upper-bound (floor y-upper-bound))
	  (setf y-upper-bound? t))))
  (when (and (not (variable-possibly-integer? x))
	     (variable-possibly-integer? y))
   (local (setf (variable-possibly-integer? y) nil))
   (setf run? t))
  (when (and (not (variable-possibly-noninteger-real? x))
	     (variable-possibly-noninteger-real? y))
   (local (setf (variable-possibly-noninteger-real? y) nil))
   (setf run? t))
  (when (and (not (variable-possibly-nonreal-number? x))
	     (variable-possibly-nonreal-number? y))
   (local (setf (variable-possibly-nonreal-number? y) nil))
   (setf run? t))
  (when (and (not (variable-possibly-boolean? x))
	     (variable-possibly-boolean? y))
   (local (setf (variable-possibly-boolean? y) nil))
   (setf run? t))
  (when (and (not (variable-possibly-nonboolean-nonnumber? x))
	     (variable-possibly-nonboolean-nonnumber? y))
   (local (setf (variable-possibly-nonboolean-nonnumber? y) nil))
   (setf run? t))
  (unless (or (variable-possibly-integer? y)
	      (variable-possibly-noninteger-real? y)
	      (variable-possibly-nonreal-number? y)
	      (variable-possibly-boolean? y)
	      (variable-possibly-nonboolean-nonnumber? y))
   (fail))
  (cond ((and x-lower-bound
	      (or (not y-lower-bound) (> x-lower-bound y-lower-bound)))
	 (local (setf (variable-lower-bound y) x-lower-bound))
	 (setf run? t))
	(y-lower-bound?
	 (local (setf (variable-lower-bound y) y-lower-bound))
	 (setf run? t)))
  (cond ((and x-upper-bound
	      (or (not y-upper-bound) (< x-upper-bound y-upper-bound)))
	 (local (setf (variable-upper-bound y) x-upper-bound))
	 (setf run? t))
	(y-upper-bound?
	 (local (setf (variable-upper-bound y) y-upper-bound))
	 (setf run? t)))
  (unless (or (null (variable-lower-bound y))
	      (null (variable-upper-bound y))
	      (<= (variable-lower-bound y) (variable-upper-bound y)))
   (fail))
  (if run?
      (let ((lower-bound (variable-lower-bound y))
	    (upper-bound (variable-upper-bound y)))
       (if (eq (variable-enumerated-domain y) t)
	   (if (and lower-bound
		    upper-bound
		    (variable-integer? y)
		    (or (null *maximum-discretization-range*)
			(<= (- upper-bound lower-bound)
			    *maximum-discretization-range*)))
	       (set-enumerated-domain!
		y (all-values (an-integer-between lower-bound upper-bound))))
	   (if lower-bound
	       (if upper-bound
		   (if (some #'(lambda (element)
				(or (< element lower-bound)
				    (> element upper-bound)))
			     (variable-enumerated-domain y))
		       ;; note: Could do less consing if had LOCAL DELETE-IF.
		       ;;       This would also allow checking list only once.
		       (set-enumerated-domain!
			y (remove-if #'(lambda (element)
					(or (< element lower-bound)
					    (> element upper-bound)))
				     (variable-enumerated-domain y))))
		   (if (some #'(lambda (element) (< element lower-bound))
			     (variable-enumerated-domain y))
		       ;; note: Could do less consing if had LOCAL DELETE-IF.
		       ;;       This would also allow checking list only once.
		       (set-enumerated-domain!
			y (remove-if #'(lambda (element)
					(< element lower-bound))
				     (variable-enumerated-domain y)))))
	       (if upper-bound
		   (if (some #'(lambda (element) (> element upper-bound))
			     (variable-enumerated-domain y))
		       ;; note: Could do less consing if had LOCAL DELETE-IF.
		       ;;       This would also allow checking list only once.
		       (set-enumerated-domain!
			y (remove-if #'(lambda (element)
					(> element upper-bound))
				     (variable-enumerated-domain y)))))))))
  (local (let* ((enumerated-domain
		 (cond
		  ((eq (variable-enumerated-domain x) t)
		   (if (eq (variable-enumerated-domain y) t)
		       t
		       (set-difference (variable-enumerated-domain y)
				       (variable-enumerated-antidomain x)
				       :test #'equal)))
		  ((eq (variable-enumerated-domain y) t)
		   (set-difference (variable-enumerated-domain x)
				   (variable-enumerated-antidomain y)
				   :test #'equal))
		  (t (intersection (variable-enumerated-domain x)
				   (variable-enumerated-domain y)
				   :test #'equal))))
		(enumerated-antidomain
		 (if (eq enumerated-domain t)
		     (union (variable-enumerated-antidomain x)
			    (variable-enumerated-antidomain y)
			    :test #'equal)
		     '())))
	  (if (null enumerated-domain) (fail))
	  (if (and (not (eq enumerated-domain t))
		   (or (eq (variable-enumerated-domain y) t)
		       (< (length enumerated-domain)
			  (length (variable-enumerated-domain y)))))
	      (setf (variable-enumerated-domain y) enumerated-domain))
	  (if (if (eq enumerated-domain t)
		  (> (length enumerated-antidomain)
		     (length (variable-enumerated-antidomain y)))
		  (not (null (variable-enumerated-antidomain y))))
	      (setf (variable-enumerated-antidomain y) enumerated-antidomain)))
	 (setf (variable-noticers y)
	       (append (variable-noticers y) (variable-noticers x)))
	 (setf (variable-noticers x) '())
	 (setf (variable-value x) y))
  (run-noticers y)))

(defun restrict-value! (x value)
 ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
 ;; note: VALUE must not be a variable.
 (if (occurs-in? x value) (fail))
 (typecase value
  (integer (unless (variable-possibly-integer? x) (fail)))
  (real (unless (variable-possibly-noninteger-real? x) (fail)))
  (number (unless (variable-possibly-nonreal-number? x) (fail)))
  (boolean (unless (variable-possibly-boolean? x) (fail)))
  (otherwise (unless (variable-possibly-nonboolean-nonnumber? x) (fail))))
 ;; needs work: This is sound only if VALUE does not contain any variables.
 (if (eq (variable-enumerated-domain x) t)
     (if (member value (variable-enumerated-antidomain x) :test #'equal)
	 (fail))
     (unless (member value (variable-enumerated-domain x) :test #'equal)
      (fail)))
 (if (and (realp value)
	  (or (and (variable-lower-bound x)
		   (< value (variable-lower-bound x)))
	      (and (variable-upper-bound x)
		   (> value (variable-upper-bound x)))))
     (fail))
 (local (setf (variable-value x) value)
	(typecase value
	 (integer (if (variable-possibly-noninteger-real? x)
		      (setf (variable-possibly-noninteger-real? x) nil))
		  (if (variable-possibly-nonreal-number? x)
		      (setf (variable-possibly-nonreal-number? x) nil))
		  (if (variable-possibly-boolean? x)
		      (setf (variable-possibly-boolean? x) nil))
		  (if (variable-possibly-nonboolean-nonnumber? x)
		      (setf (variable-possibly-nonboolean-nonnumber? x) nil))
		  (if (or (null (variable-lower-bound x))
			  (not (integerp (variable-lower-bound x)))
			  (> value (variable-lower-bound x)))
		      (setf (variable-lower-bound x) value))
		  (if (or (null (variable-upper-bound x))
			  (not (integerp (variable-upper-bound x)))
			  (< value (variable-upper-bound x)))
		      (setf (variable-upper-bound x) value)))
	 (real (if (variable-possibly-integer? x)
		   (setf (variable-possibly-integer? x) nil))
	       (if (variable-possibly-nonreal-number? x)
		   (setf (variable-possibly-nonreal-number? x) nil))
	       (if (variable-possibly-boolean? x)
		   (setf (variable-possibly-boolean? x) nil))
	       (if (variable-possibly-nonboolean-nonnumber? x)
		   (setf (variable-possibly-nonboolean-nonnumber? x) nil))
	       (if (or (null (variable-lower-bound x))
		       (> value (variable-lower-bound x)))
		   (setf (variable-lower-bound x) value))
	       (if (or (null (variable-upper-bound x))
		       (< value (variable-upper-bound x)))
		   (setf (variable-upper-bound x) value)))
	 (number (if (variable-possibly-integer? x)
		     (setf (variable-possibly-integer? x) nil))
		 (if (variable-possibly-noninteger-real? x)
		     (setf (variable-possibly-noninteger-real? x) nil))
		 (if (variable-possibly-boolean? x)
		     (setf (variable-possibly-boolean? x) nil))
		 (if (variable-possibly-nonboolean-nonnumber? x)
		     (setf (variable-possibly-nonboolean-nonnumber? x) nil)))
	 (boolean (if (variable-possibly-integer? x)
		      (setf (variable-possibly-integer? x) nil))
		  (if (variable-possibly-noninteger-real? x)
		      (setf (variable-possibly-noninteger-real? x) nil))
		  (if (variable-possibly-nonreal-number? x)
		      (setf (variable-possibly-nonreal-number? x) nil))
		  (if (variable-possibly-nonboolean-nonnumber? x)
		      (setf (variable-possibly-nonboolean-nonnumber? x) nil)))
	 (otherwise (if (variable-possibly-integer? x)
			(setf (variable-possibly-integer? x) nil))
		    (if (variable-possibly-noninteger-real? x)
			(setf (variable-possibly-noninteger-real? x) nil))
		    (if (variable-possibly-nonreal-number? x)
			(setf (variable-possibly-nonreal-number? x) nil))
		    (if (variable-possibly-boolean? x)
			(setf (variable-possibly-boolean? x) nil))))
	(cond ((eq (variable-enumerated-domain x) t)
	       ;; needs work: This is sound only if VALUE does not contain any
	       ;;             variables.
	       (setf (variable-enumerated-domain x) (list value))
	       (setf (variable-enumerated-antidomain x) '()))
	      ((not (null (rest (variable-enumerated-domain x))))
	       ;; needs work: This is sound only if VALUE does not contain any
	       ;;             variables.
	       (setf (variable-enumerated-domain x) (list value)))))
 (run-noticers x))

(defun restrict-true! (x)
 ;; note: X must be a Boolean variable.
 (if (eq (variable-value x) nil) (fail))
 (when (eq (variable-value x) x)
  (local (setf (variable-value x) t)
	 (setf (variable-enumerated-domain x) '(t)))
  (run-noticers x)))

(defun restrict-false! (x)
 ;; note: X must be a Boolean variable.
 (if (eq (variable-value x) t) (fail))
 (when (eq (variable-value x) x)
  (local (setf (variable-value x) nil)
	 (setf (variable-enumerated-domain x) '(nil)))
  (run-noticers x)))

(defun set-enumerated-domain! (x enumerated-domain)
 ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
 ;; note: All callers must insure that the new ENUMERATED-DOMAIN is a subset
 ;;       of the old one.
 (if (null enumerated-domain) (fail))
 (local
  (cond
   ((eq (variable-enumerated-domain x) t)
    (setf (variable-enumerated-domain x) enumerated-domain)
    (unless (null (variable-enumerated-antidomain x))
     (setf (variable-enumerated-antidomain x) '()))
    (if (and (variable-possibly-boolean? x)
	     (not (some #'booleanp enumerated-domain)))
	(setf (variable-possibly-boolean? x) nil))
    (if (and (variable-possibly-nonboolean-nonnumber? x)
	     (not (some #'(lambda (x)
			   (and (not (booleanp x)) (not (numberp x))))
			enumerated-domain)))
	(setf (variable-possibly-nonboolean-nonnumber? x) nil))
    (if (and (variable-possibly-nonreal-number? x)
	     (not (some #'(lambda (x) (and (not (realp x)) (numberp x)))
			enumerated-domain)))
	(setf (variable-possibly-nonreal-number? x) nil))
    (if (and (variable-possibly-noninteger-real? x)
	     (not (some #'(lambda (x) (and (not (integerp x)) (realp x)))
			enumerated-domain)))
	(setf (variable-possibly-noninteger-real? x) nil))
    (if (and (variable-possibly-integer? x)
	     (not (some #'integerp enumerated-domain)))
	(setf (variable-possibly-integer? x) nil))
    (if (variable-real? x)
	(let ((lower-bound (reduce #'min enumerated-domain))
	      (upper-bound (reduce #'max enumerated-domain)))
	 (if (or (null (variable-lower-bound x))
		 (> lower-bound (variable-lower-bound x)))
	     (setf (variable-lower-bound x) lower-bound))
	 (if (or (null (variable-upper-bound x))
		 (< upper-bound (variable-upper-bound x)))
	     (setf (variable-upper-bound x) upper-bound))))
    (if (null (rest enumerated-domain))
	(setf (variable-value x) (first enumerated-domain)))
    t)
   ((< (length enumerated-domain) (length (variable-enumerated-domain x)))
    (setf (variable-enumerated-domain x) enumerated-domain)
    (if (and (variable-possibly-boolean? x)
	     (not (some #'booleanp enumerated-domain)))
	(setf (variable-possibly-boolean? x) nil))
    (if (and (variable-possibly-nonboolean-nonnumber? x)
	     (not (some #'(lambda (x)
			   (and (not (booleanp x)) (not (numberp x))))
			enumerated-domain)))
	(setf (variable-possibly-nonboolean-nonnumber? x) nil))
    (if (and (variable-possibly-nonreal-number? x)
	     (not (some #'(lambda (x) (and (not (realp x)) (numberp x)))
			enumerated-domain)))
	(setf (variable-possibly-nonreal-number? x) nil))
    (if (and (variable-possibly-noninteger-real? x)
	     (not (some #'(lambda (x) (and (not (integerp x)) (realp x)))
			enumerated-domain)))
	(setf (variable-possibly-noninteger-real? x) nil))
    (if (and (variable-possibly-integer? x)
	     (not (some #'integerp enumerated-domain)))
	(setf (variable-possibly-integer? x) nil))
    (if (variable-real? x)
	(let ((lower-bound (reduce #'min enumerated-domain))
	      (upper-bound (reduce #'max enumerated-domain)))
	 (if (or (null (variable-lower-bound x))
		 (> lower-bound (variable-lower-bound x)))
	     (setf (variable-lower-bound x) lower-bound))
	 (if (or (null (variable-upper-bound x))
		 (< upper-bound (variable-upper-bound x)))
	     (setf (variable-upper-bound x) upper-bound))))
    (if (null (rest enumerated-domain))
	(setf (variable-value x) (first enumerated-domain)))
    t)
   (t nil))))

(defun restrict-enumerated-domain! (x enumerated-domain)
 ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
 ;; note: ENUMERATED-DOMAIN must not be a variable.
 (unless (typep enumerated-domain 'sequence) (fail))
 (when (every #'ground? enumerated-domain)
  (setf enumerated-domain
	(remove-duplicates (map 'list #'eliminate-variables enumerated-domain)
			   :test #'equal))
  (unless (variable-possibly-boolean? x)
   (setf enumerated-domain (remove-if #'booleanp enumerated-domain)))
  (unless (variable-possibly-nonboolean-nonnumber? x)
   (setf enumerated-domain
	 (remove-if #'(lambda (x) (and (not (booleanp x)) (not (numberp x))))
		    enumerated-domain)))
  (unless (variable-possibly-nonreal-number? x)
   (setf enumerated-domain
	 (remove-if #'(lambda (x) (and (not (realp x)) (numberp x)))
		    enumerated-domain)))
  (unless (variable-possibly-noninteger-real? x)
   (setf enumerated-domain
	 (remove-if #'(lambda (x) (and (not (integerp x)) (realp x)))
		    enumerated-domain)))
  (unless (variable-possibly-integer? x)
   (setf enumerated-domain (remove-if #'integerp enumerated-domain)))
  (if (variable-upper-bound x)
      (let ((upper-bound (variable-upper-bound x)))
       (setf enumerated-domain
	     (remove-if #'(lambda (element) (> element upper-bound))
			enumerated-domain))))
  (if (variable-lower-bound x)
      (let ((lower-bound (variable-lower-bound x)))
       (setf enumerated-domain
	     (remove-if #'(lambda (element) (< element lower-bound))
			enumerated-domain))))
  (setf enumerated-domain
	(if (eq (variable-enumerated-domain x) t)
	    (set-difference enumerated-domain
			    (variable-enumerated-antidomain x)
			    :test #'equal)
	    (intersection (variable-enumerated-domain x) enumerated-domain
			  :test #'equal)))
  (if (set-enumerated-domain! x enumerated-domain) (run-noticers x))))

(defun restrict-enumerated-antidomain! (x enumerated-antidomain)
 ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
 ;; note: ENUMERATED-ANTIDOMAIN must not be a variable.
 (unless (typep enumerated-antidomain 'sequence) (fail))
 (when (every #'ground? enumerated-antidomain)
  (setf enumerated-antidomain
	(remove-duplicates
	 (map 'list #'eliminate-variables enumerated-antidomain)
	 :test #'equal))
  (cond
   ((eq (variable-enumerated-domain x) t)
    (setf enumerated-antidomain
	  (union (variable-enumerated-antidomain x) enumerated-antidomain
		 :test #'equal))
    (when (> (length enumerated-antidomain)
	     (length (variable-enumerated-antidomain x)))
     (local (setf (variable-enumerated-antidomain x) enumerated-antidomain))
     (run-noticers x)))
   ((set-enumerated-domain!
     x (set-difference (variable-enumerated-domain x) enumerated-antidomain
		       :test #'equal))
    (run-noticers x)))))

;;; Rules

(defun +-rule-up (z x y)
 (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
 ;; note: We can't assert that Z in not an integer when either X or Y are not
 ;;       integers since they may be Gaussian integers. But we can if either
 ;;       X or Y is real. If the Screamer type system could distinguish
 ;;       Gaussian integers from other complex numbers we could whenever X or
 ;;       Y was not a Gaussian integer.
 (if (and (or (variable-noninteger? x) (variable-noninteger? y))
	  (or (variable-real? x) (variable-real? y)))
     (restrict-noninteger! z))
 (if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
 ;; note: Ditto.
 (if (and (or (variable-nonreal? x) (variable-nonreal? y))
	  (or (variable-real? x) (variable-real? y)))
     (restrict-nonreal! z))
 (if (and (variable-real? x) (variable-real? y) (variable-real? z))
     (restrict-bounds!
      z
      (infinity-+ (variable-lower-bound x) (variable-lower-bound y))
      (infinity-+ (variable-upper-bound x) (variable-upper-bound y))))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? x))
	   (not (variable? y))
	   (not (variable? z))
	   (/= z (+ x y)))
      (fail))))

(defun +-rule-down (z x y)
 ;; note: We can't assert that X and Y are integers when Z is an integer since
 ;;       Z may be an integer when X and Y are Gaussian integers. But we can
 ;;       make such an assertion if either X or Y is real. If the Screamer
 ;;       type system could distinguish Gaussian integers from other complex
 ;;       numbers we could make such an assertion whenever either X or Y was
 ;;       not a Gaussian integer.
 (if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
     (restrict-integer! x))
 ;; note: Ditto.
 (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
     (restrict-real! x))
 (if (and (variable-real? x) (variable-real? y) (variable-real? z))
     (restrict-bounds!
      x
      (infinity-- (variable-lower-bound z) (variable-upper-bound y))
      (infinity-- (variable-upper-bound z) (variable-lower-bound y))))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? x))
	   (not (variable? y))
	   (not (variable? z))
	   (/= z (+ x y)))
      (fail))))

(defun /-rule (z x y)
 (when (and (variable-lower-bound x) (plusp (variable-lower-bound x)))
  (cond ((and (variable-upper-bound x) (not (zerop (variable-upper-bound x))))
	 (if (variable-lower-bound z)
	     (cond
	      ((minusp (variable-lower-bound z))
	       (restrict-lower-bound!
		y (/ (variable-lower-bound z) (variable-lower-bound x))))
	      (t (restrict-lower-bound! y 0)
		 (restrict-lower-bound!
		  y (/ (variable-lower-bound z) (variable-upper-bound x))))))
	 (if (variable-upper-bound z)
	     (cond
	      ((plusp (variable-upper-bound z))
	       (restrict-upper-bound!
		y (/ (variable-upper-bound z) (variable-lower-bound x))))
	      (t (restrict-upper-bound! y 0)
		 (restrict-upper-bound!
		  y (/ (variable-upper-bound z) (variable-upper-bound x)))))))
	(t (if (variable-lower-bound z)
	       (cond
		((minusp (variable-lower-bound z))
		 (restrict-lower-bound!
		  y (/ (variable-lower-bound z) (variable-lower-bound x))))
		(t (restrict-lower-bound! y 0))))
	   (if (variable-upper-bound z)
	       (cond
		((plusp (variable-upper-bound z))
		 (restrict-upper-bound!
		  y (/ (variable-upper-bound z) (variable-lower-bound x))))
		(t (restrict-upper-bound! y 0)))))))
 (when (and (variable-upper-bound x) (minusp (variable-upper-bound x)))
  (cond ((and (variable-lower-bound x) (not (zerop (variable-lower-bound x))))
	 (if (variable-upper-bound z)
	     (cond
	      ((plusp (variable-upper-bound z))
	       (restrict-lower-bound!
		y (/ (variable-upper-bound z) (variable-upper-bound x))))
	      (t (restrict-lower-bound! y 0)
		 (restrict-lower-bound!
		  y (/ (variable-upper-bound z) (variable-lower-bound x))))))
	 (if (variable-lower-bound z)
	     (cond
	      ((minusp (variable-lower-bound z))
	       (restrict-upper-bound!
		y (/ (variable-lower-bound z) (variable-upper-bound x))))
	      (t (restrict-upper-bound! y 0)
		 (restrict-upper-bound!
		  y (/ (variable-lower-bound z) (variable-lower-bound x)))))))
	(t (if (variable-upper-bound z)
	       (cond
		((plusp (variable-upper-bound z))
		 (restrict-lower-bound!
		  y (/ (variable-upper-bound z) (variable-upper-bound x))))
		(t (restrict-lower-bound! y 0))))
	   (if (variable-lower-bound z)
	       (cond
		((minusp (variable-lower-bound z))
		 (restrict-upper-bound!
		  y (/ (variable-lower-bound z) (variable-upper-bound x))))
		(t (restrict-upper-bound! y 0))))))))

(defun *-rule-up (z x y)
 (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
 ;; note: We can't assert that Z in not an integer when either X or Y are not
 ;;       integers since they may be Gaussian integers. But we can if either
 ;;       X or Y is real. If the Screamer type system could distinguish
 ;;       Gaussian integers from other complex numbers we could whenever X or
 ;;       Y was not a Gaussian integer.
 (if (and (or (variable-noninteger? x) (variable-noninteger? y))
	  (or (variable-real? x) (variable-real? y)))
     (restrict-noninteger! z))
 (if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
 ;; note: Ditto.
 (if (and (or (variable-nonreal? x) (variable-nonreal? y))
	  (or (variable-real? x) (variable-real? y)))
     (restrict-nonreal! z))
 (if (and (variable-real? x) (variable-real? y) (variable-real? z))
     ;; note: Can sometimes do better than the following even when ranges are
     ;;       not finite.
     (restrict-bounds!
      z
      (infinity-min
       (infinity-* (variable-lower-bound x) (variable-lower-bound y))
       (infinity-min
	(infinity-* (variable-lower-bound x) (variable-upper-bound y))
	(infinity-min
	 (infinity-* (variable-upper-bound x) (variable-lower-bound y))
	 (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))
      (infinity-max
       (infinity-* (variable-lower-bound x) (variable-lower-bound y))
       (infinity-max
	(infinity-* (variable-lower-bound x) (variable-upper-bound y))
	(infinity-max
	 (infinity-* (variable-upper-bound x) (variable-lower-bound y))
	 (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? x))
	   (not (variable? y))
	   (not (variable? z))
	   (/= z (* x y)))
      (fail))))

(defun *-rule-down (z x y)
 ;; note: We can't assert that X and Y are integers when Z is an integer since
 ;;       Z may be an integer when X and Y are Gaussian integers. But we can
 ;;       make such an assertion if either X or Y is real. If the Screamer
 ;;       type system could distinguish Gaussian integers from other complex
 ;;       numbers we could make such an assertion whenever either X or Y was
 ;;       not a Gaussian integer.
 (if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
     (restrict-integer! x))
 ;; note: Ditto.
 (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
     (restrict-real! x))
 (if (and (variable-real? x) (variable-real? y) (variable-real? z))
     (/-rule z y x))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? x))
	   (not (variable? y))
	   (not (variable? z))
	   (/= z (* x y)))
      (fail))))

(defun min-rule-up (z x y)
 (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
 (restrict-bounds!
  z
  (infinity-min (variable-lower-bound x) (variable-lower-bound y))
  (if (variable-upper-bound x)
      (if (variable-upper-bound y)
	  (min (variable-upper-bound x) (variable-upper-bound y))
	  (variable-upper-bound x))
      (variable-upper-bound y)))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? z))
	   (not (variable? x))
	   (not (variable? y))
	   (/= z (min x y)))
      (fail))))

(defun min-rule-down (z x y)
 ;; note: The analog of the following for upper bounds, namely restricting
 ;;       the upper bound of either X or Y to (VARIABLE-UPPER-BOUND Z) is
 ;;       nondeterministic.
 (if (variable-lower-bound z)
     (restrict-lower-bound! x (variable-lower-bound z)))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? z))
	   (not (variable? x))
	   (not (variable? y))
	   (/= z (min x y)))
      (fail))))

(defun max-rule-up (z x y)
 (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
 (restrict-bounds!
  z
  (if (variable-lower-bound x)
      (if (variable-lower-bound y)
	  (max (variable-lower-bound x) (variable-lower-bound y))
	  (variable-lower-bound x))
      (variable-lower-bound y))
  (infinity-max (variable-upper-bound x) (variable-upper-bound y)))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? z))
	   (not (variable? x))
	   (not (variable? y))
	   (/= z (max x y)))
      (fail))))

(defun max-rule-down (z x y)
 ;; note: The analog of the following for lower bounds, namely restricting
 ;;       the lower bound of either X or Y to (VARIABLE-LOWER-BOUND Z) is
 ;;       nondeterministic.
 (if (variable-upper-bound z)
     (restrict-upper-bound! x (variable-upper-bound z)))
 (let ((x (value-of x))
       (y (value-of y))
       (z (value-of z)))
  (if (and (not (variable? z))
	   (not (variable? x))
	   (not (variable? y))
	   (/= z (max x y)))
      (fail))))

(defun =-rule (x y)
 (cond
  ;; note: I forget why +-RULE *-RULE MIN-RULE and MAX-RULE must perform the
  ;;       check in the second COND clause irrespective of whether the first
  ;;       clause is executed.
  ((and (variable-real? x) (variable-real? y))
   (restrict-bounds! x (variable-lower-bound y) (variable-upper-bound y))
   (restrict-bounds! y (variable-lower-bound x) (variable-upper-bound x)))
  ((and (not (variable? x)) (not (variable? y)) (/= x y)) (fail))))

(defun <=-rule (x y)
 (if (variable-lower-bound x)
     (restrict-lower-bound! y (variable-lower-bound x)))
 (if (variable-upper-bound y)
     (restrict-upper-bound! x (variable-upper-bound y))))

(defun <-rule (x y)
 (if (variable-lower-bound x)
     (restrict-lower-bound! y (if (variable-integer? y)
				  (1+ (floor (variable-lower-bound x)))
				  (variable-lower-bound x))))
 (if (variable-upper-bound y)
     (restrict-upper-bound! x (if (variable-integer? x)
				  (1- (ceiling (variable-upper-bound y)))
				  (variable-upper-bound y))))
 (let ((x (value-of x))
       (y (value-of y)))
  (if (and (not (variable? x)) (not (variable? y)) (>= x y)) (fail))))

(defun /=-rule (x y)
 ;; note: Got rid of the nondeterministic version of /=-RULE.
 (let ((x (value-of x))
       (y (value-of y)))
  (if (and (not (variable? x)) (not (variable? y)) (= x y)) (fail))))

;;; Lifted Arithmetic Functions (Two argument optimized)

(defun +v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 ;; needs work: The first two optimizations below violate CommonLisp type
 ;;             propagation conventions.
 (cond ((and (bound? x) (zerop (value-of x))) (value-of y))
       ((and (bound? y) (zerop (value-of y))) (value-of x))
       ((and (bound? x) (bound? y)) (+ (value-of x) (value-of y)))
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-numberv)))
	   (attach-noticer!
	    #'(lambda () (+-rule-up z x y) (+-rule-down z y x)) x)
	   (attach-noticer!
	    #'(lambda () (+-rule-up z x y) (+-rule-down z x y)) y)
	   (attach-noticer!
	    #'(lambda () (+-rule-down z x y) (+-rule-down z y x)) z)
	   z))))

(defun -v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 ;; needs work: The first optimization below violates CommonLisp type
 ;;             propagation conventions.
 (cond ((and (bound? y) (zerop (value-of y))) (value-of x))
       ((and (bound? x) (bound? y)) (- (value-of x) (value-of y)))
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-numberv)))
	   (attach-noticer!
	    #'(lambda () (+-rule-down x y z) (+-rule-down x z y)) x)
	   (attach-noticer!
	    #'(lambda () (+-rule-up x y z) (+-rule-down x z y)) y)
	   (attach-noticer!
	    #'(lambda () (+-rule-up x y z) (+-rule-down x y z)) z)
	   z))))

(defun *v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 ;; needs work: The first four optimizations below violate CommonLisp type
 ;;             propagation conventions.
 (cond ((and (bound? x) (zerop (value-of x))) 0)
       ((and (bound? y) (zerop (value-of y))) 0)
       ((and (bound? x) (= (value-of x) 1)) (value-of y))
       ((and (bound? y) (= (value-of y) 1)) (value-of x))
       ((and (bound? x) (bound? y)) (* (value-of x) (value-of y)))
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-numberv)))
	   (attach-noticer!
	    #'(lambda () (*-rule-up z x y) (*-rule-down z y x)) x)
	   (attach-noticer!
	    #'(lambda () (*-rule-up z x y) (*-rule-down z x y)) y)
	   (attach-noticer!
	    #'(lambda () (*-rule-down z x y) (*-rule-down z y x)) z)
	   z))))

(defun /v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 ;; needs work: The first three optimizations below violate CommonLisp type
 ;;             propagation conventions.
 (cond ((and (bound? x) (zerop (value-of x))) 0)
       ((and (bound? y) (zerop (value-of y))) (fail))
       ((and (bound? y) (= (value-of y) 1)) (value-of x))
       ((and (bound? x) (bound? y)) (/ (value-of x) (value-of y)))
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-numberv)))
	   (attach-noticer!
	    #'(lambda () (*-rule-down x y z) (*-rule-down x z y)) x)
	   (attach-noticer!
	    #'(lambda () (*-rule-up x y z) (*-rule-down x z y)) y)
	   (attach-noticer!
	    #'(lambda () (*-rule-up x y z) (*-rule-down x y z)) z)
	   z))))

(defun minv2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (cond ((known?-<=v2-internal x y) (value-of x))
       ((known?-<=v2-internal y x) (value-of y))
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-realv)))
	   (attach-noticer!
	    #'(lambda () (min-rule-up z x y) (min-rule-down z y x)) x)
	   (attach-noticer!
	    #'(lambda () (min-rule-up z x y) (min-rule-down z x y)) y)
	   (attach-noticer!
	    #'(lambda () (min-rule-down z x y) (min-rule-down z y x)) z)
	   z))))

(defun maxv2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (cond ((known?-<=v2-internal y x) (value-of x))
       ((known?-<=v2-internal x y) (value-of y))
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-realv)))
	   (attach-noticer!
	    #'(lambda () (max-rule-up z x y) (max-rule-down z y x)) x)
	   (attach-noticer!
	    #'(lambda () (max-rule-up z x y) (max-rule-down z x y)) y)
	   (attach-noticer!
	    #'(lambda () (max-rule-down z x y) (max-rule-down z y x)) z)
	   z))))

;;; Lifted Type Functions (KNOWN? optimized)

(defun known?-integerpv (x)
 (let ((x (value-of x)))
  (typecase x
   (integer t)
   (variable (variable-integer? x))
   (otherwise nil))))

(defun known?-notv-integerpv (x)
 (let ((x (value-of x)))
  (typecase x
   (integer nil)
   (variable (variable-noninteger? x))
   (otherwise t))))

(defun known?-realpv (x)
 (let ((x (value-of x)))
  (typecase x
   (real t)
   (variable (variable-real? x))
   (otherwise nil))))

(defun known?-notv-realpv (x)
 (let ((x (value-of x)))
  (typecase x
   (real nil)
   (variable (variable-nonreal? x))
   (otherwise t))))

(defun known?-numberpv (x)
 (let ((x (value-of x)))
  (typecase x
   (number t)
   (variable (variable-number? x))
   (otherwise nil))))

(defun known?-notv-numberpv (x)
 (let ((x (value-of x)))
  (typecase x
   (number nil)
   (variable (variable-nonnumber? x))
   (otherwise t))))

(defun known?-booleanpv (x)
 (let ((x (value-of x)))
  (typecase x
   (boolean t)
   (variable (variable-boolean? x))
   (otherwise nil))))

(defun known?-notv-booleanpv (x)
 (let ((x (value-of x)))
  (typecase x
   (boolean nil)
   (variable (variable-nonboolean? x))
   (otherwise t))))

;;; Lifted Arithmetic Comparison Functions (Two argument KNOWN? optimized)

(defun known?-<=v2-variable (x y)
 (and (variable-upper-bound x)
      (variable-lower-bound y)
      (<= (variable-upper-bound x) (variable-lower-bound y))))

(defun known?-<v2-variable (x y)
 (and (variable-upper-bound x)
      (variable-lower-bound y)
      (< (variable-upper-bound x) (variable-lower-bound y))))

(defun known?-=v2-variable (x y)
 (or (and (variable-real? x)
	  (variable-real? y)
	  (known?-<=v2-variable x y)
	  (known?-<=v2-variable y x))
     (and (not (eq x (variable-value x)))
	  (not (eq y (variable-value y)))
	  (= (variable-value x) (variable-value y)))))

(defun known?-/=v2-variable (x y)
 (or (and (variable-real? x)
	  (variable-real? y)
	  (or (known?-<v2-variable x y) (known?-<v2-variable y x)))
     (and (not (eq x (variable-value x)))
	  (not (eq y (variable-value y)))
	  (/= (variable-value x) (variable-value y)))))

(defun known?-=v2-internal (x y)
 (known?-=v2-variable (variablize x) (variablize y)))

(defun known?-<=v2-internal (x y)
 (known?-<=v2-variable (variablize x) (variablize y)))

(defun known?-<v2-internal (x y)
 (known?-<v2-variable (variablize x) (variablize y)))

(defun known?-/=v2-internal (x y)
 (known?-/=v2-variable (variablize x) (variablize y)))

(defun known?-=v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 (known?-=v2-internal x y))

(defun known?-<=v2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (known?-<=v2-internal x y))

(defun known?-<v2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (known?-<v2-internal x y))

(defun known?-/=v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 (known?-/=v2-internal x y))

;;; Lifted Type Functions (ASSERT! optimized)

(defun assert!-integerpv (x)
 (let ((x (value-of x)))
  (typecase x
   (integer)
   (variable (restrict-integer! x))
   (otherwise (fail)))))

(defun assert!-notv-integerpv (x)
 (let ((x (value-of x)))
  (typecase x
   (integer (fail))
   (variable (restrict-noninteger! x))
   (otherwise))))

(defun assert!-realpv (x)
 (let ((x (value-of x)))
  (typecase x
   (real)
   (variable (restrict-real! x))
   (otherwise (fail)))))

(defun assert!-notv-realpv (x)
 (let ((x (value-of x)))
  (typecase x
   (real (fail))
   (variable (restrict-nonreal! x))
   (otherwise))))

(defun assert!-numberpv (x)
 (let ((x (value-of x)))
  (typecase x
   (number)
   (variable (restrict-number! x))
   (otherwise (fail)))))

(defun assert!-notv-numberpv (x)
 (let ((x (value-of x)))
  (typecase x
   (number (fail))
   (variable (restrict-nonnumber! x))
   (otherwise))))

(defun assert!-booleanpv (x)
 (let ((x (value-of x)))
  (typecase x
   (boolean)
   (variable (restrict-boolean! x))
   (otherwise (fail)))))

(defun assert!-notv-booleanpv (x)
 (let ((x (value-of x)))
  (typecase x
   (boolean (fail))
   (variable (restrict-nonboolean! x))
   (otherwise))))

;;; Lifted Arithmetic Comparison Functions (Two argument ASSERT! optimized)

(defun assert!-=v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 (let ((x (variablize x))
       (y (variablize y)))
  (attach-noticer! #'(lambda () (=-rule x y)) x)
  (attach-noticer! #'(lambda () (=-rule x y)) y)))

(defun assert!-<=v2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (let ((x (variablize x))
       (y (variablize y)))
  (attach-noticer! #'(lambda () (<=-rule x y)) x)
  (attach-noticer! #'(lambda () (<=-rule x y)) y)))

(defun assert!-<v2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (let ((x (variablize x))
       (y (variablize y)))
  (attach-noticer! #'(lambda () (<-rule x y)) x)
  (attach-noticer! #'(lambda () (<-rule x y)) y)))

(defun assert!-/=v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 (let ((x (variablize x))
       (y (variablize y)))
  ;; note: Got rid of the nondeterministic version that called the
  ;;       nondeterministic version of /=-RULE.
  (attach-noticer! #'(lambda () (/=-rule x y)) x)
  (attach-noticer! #'(lambda () (/=-rule x y)) y)))

;;; Lifted Type Functions

(defun integerpv (x)
 (cond ((known?-integerpv x) t)
       ((known?-notv-integerpv x) nil)
       (t (let ((x (variablize x))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-integer? x) (restrict-true! z))
		     ((variable-noninteger? x) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (restrict-integer! x))
		     ((variable-false? z) (restrict-noninteger! x))))
	    z)
	   z))))

(defun realpv (x)
 (cond ((known?-realpv x) t)
       ((known?-notv-realpv x) nil)
       (t (let ((x (variablize x))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-real? x) (restrict-true! z))
		     ((variable-nonreal? x) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (restrict-real! x))
		     ((variable-false? z) (restrict-nonreal! x))))
	    z)
	   z))))

(defun numberpv (x)
 (cond ((known?-numberpv x) t)
       ((known?-notv-numberpv x) nil)
       (t (let ((x (variablize x))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-number? x) (restrict-true! z))
		     ((variable-nonnumber? x) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (restrict-number! x))
		     ((variable-false? z) (restrict-nonnumber! x))))
	    z)
	   z))))

(defun booleanpv (x)
 (cond ((known?-booleanpv x) t)
       ((known?-notv-booleanpv x) nil)
       (t (let ((x (variablize x))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-boolean? x) (restrict-true! z))
		     ((variable-nonboolean? x) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (restrict-boolean! x))
		     ((variable-false? z) (restrict-nonboolean! x))))
	    z)
	   z))))

;;; Lifted MEMBERV

(defun known?-memberv-list-internal (x y)
 (and (consp y)
      (or (known?-equalv x (first y))
	  (known?-memberv-list-internal x (rest y)))))

(defun known?-memberv-list (x y)
 (typecase y
  (cons (or (known?-equalv x (first y)) (known?-memberv-list x (rest y))))
  (variable
   (if (eq (variable-value y) y)
       (and (not (eq (variable-enumerated-domain y) t))
	    (every
	     #'(lambda (element) (known?-memberv-list-internal x element))
	     (variable-enumerated-domain y)))
       (known?-memberv-list x (variable-value y))))
  (otherwise nil)))

(defun known?-memberv-internal (x y)
 (typecase y
  (list (known?-memberv-list x y))
  (vector (some #'(lambda (element) (known?-equalv x element)) y))
  (variable
   (if (eq (variable-value y) y)
       (and (not (eq (variable-enumerated-domain y) t))
	    (every
	     #'(lambda (element)
		(typecase element
		 (list (known?-memberv-list-internal x element))
		 (vector (some #'(lambda (e) (known?-equalv x e)) element))
		 (otherwise nil)))
	     (variable-enumerated-domain y)))
       (known?-memberv-internal x (variable-value y))))
  (otherwise (fail))))

(defun known?-memberv (x y)
 (cond ((and (variable? x) (not (eq (variable-value x) x)))
	(known?-memberv (variable-value x) y))
       ((and (variable? x) (not (eq (variable-enumerated-domain x) t)))
	;; note: This first alternative is an optimization in case membership
	;;       can be determined simply through sharing relationships.
	(or (known?-memberv-internal x y)
	    (every #'(lambda (element) (known?-memberv-internal element y))
		   (variable-enumerated-domain x))))
       (t (known?-memberv-internal x y))))

(defun known?-notv-memberv-list-internal (x y)
 (or (not (consp y))
     (and (known?-notv-equalv x (first y))
	  (known?-notv-memberv-list-internal x (rest y)))))

(defun known?-notv-memberv-list (x y)
 (typecase y
  (cons (and (known?-notv-equalv x (first y))
	     (known?-notv-memberv-list x (rest y))))
  (variable
   (if (eq (variable-value y) y)
       (and (not (eq (variable-enumerated-domain y) t))
	    (every #'(lambda (element)
		      (known?-notv-memberv-list-internal x element))
		   (variable-enumerated-domain y)))
       (known?-notv-memberv-list x (variable-value y))))
  (otherwise t)))

(defun known?-notv-memberv-internal (x y)
 (typecase y
  (list (known?-notv-memberv-list x y))
  (vector (every #'(lambda (element) (known?-notv-equalv x element)) y))
  (variable
   (if (eq (variable-value y) y)
       (and (not (eq (variable-enumerated-domain y) t))
	    (every
	     #'(lambda (element)
		(typecase element
		 (list (known?-notv-memberv-list-internal x element))
		 (vector
		  (every #'(lambda (e) (known?-notv-equalv x e)) element))
		 (otherwise nil)))
	     (variable-enumerated-domain y)))
       (known?-notv-memberv-internal x (variable-value y))))
  (otherwise (fail))))

(defun known?-notv-memberv (x y)
 (cond
  ((and (variable? x) (not (eq (variable-value x) x)))
   (known?-notv-memberv (variable-value x) y))
  ((and (variable? x) (not (eq (variable-enumerated-domain x) t)))
   ;; note: This first alternative is an optimization in case membership
   ;;       can be determined simply through sharing relationships.
   (or (known?-notv-memberv-internal x y)
       (every #'(lambda (element) (known?-notv-memberv-internal element y))
	      (variable-enumerated-domain x))))
  (t (known?-notv-memberv-internal x y))))

(defun assert!-memberv-internal (x y)
 (let ((x (value-of x)))
  (if (known?-notv-memberv x y) (fail))
  (if (variable? x)
      (let ((y (value-of y)))
       (unless (variable? y) (restrict-enumerated-domain! x y))))))

(defun assert!-memberv (x y)
 (let ((y (value-of y)))
  (if (vectorp y)
      (dotimes (i (length y))
       (attach-noticer! #'(lambda () (assert!-memberv-internal x y))
			(aref y i)))
      (attach-noticer! #'(lambda () (assert!-memberv-internal x y)) y))))

(defun assert!-notv-memberv-internal (x y)
 (let ((x (value-of x)))
  (if (known?-memberv x y) (fail))
  (if (variable? x)
      (let ((y (value-of y)))
       (unless (variable? y) (restrict-enumerated-antidomain! x y))))))

(defun assert!-notv-memberv (x y)
 (let ((y (value-of y)))
  (if (vectorp y)
      (dotimes (i (length y))
       (attach-noticer! #'(lambda () (assert!-notv-memberv-internal x y))
			(aref y i)))
      (attach-noticer! #'(lambda () (assert!-notv-memberv-internal x y)) y))))

(defun memberv (x y)
 (cond ((known?-memberv x y) t)
       ((known?-notv-memberv x y) nil)
       (t (let ((x (variablize x))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-memberv x y) (restrict-true! z))
		     ((known?-notv-memberv x y) (restrict-false! z))))
	    x)
	   (if (vectorp y)
	       (dolist (element y)
		(attach-noticer!
		 #'(lambda ()
		    (cond ((known?-memberv x y) (restrict-true! z))
			  ((known?-notv-memberv x y) (restrict-false! z))))
		 element))
	       (attach-noticer!
		#'(lambda ()
		   (cond ((known?-memberv x y) (restrict-true! z))
			 ((known?-notv-memberv x y) (restrict-false! z))))
		y))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (assert!-memberv x y))
		     ((variable-false? z) (assert!-notv-memberv x y))))
	    z)
	   z))))

;;; Lifted Arithmetic Comparison Functions (Two argument optimized)

(defun =v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 (cond ((known?-=v2-internal x y) t)
       ((known?-/=v2-internal x y) nil)
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-=v2-variable x y) (restrict-true! z))
		     ((known?-/=v2-variable x y) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-=v2-variable x y) (restrict-true! z))
		     ((known?-/=v2-variable x y) (restrict-false! z))))
	    y)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (assert!-=v2 x y))
		     ((variable-false? z) (assert!-/=v2 x y))))
	    z)
	   z))))

(defun <=v2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (cond ((known?-<=v2-internal x y) t)
       ((known?-<v2-internal y x) nil)
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-<=v2-variable x y) (restrict-true! z))
		     ((known?-<v2-variable y x) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-<=v2-variable x y) (restrict-true! z))
		     ((known?-<v2-variable y x) (restrict-false! z))))
	    y)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (assert!-<=v2 x y))
		     ((variable-false? z) (assert!-<v2 y x))))
	    z)
	   z))))

(defun <v2 (x y)
 (assert!-realpv x)
 (assert!-realpv y)
 (cond ((known?-<v2-internal x y) t)
       ((known?-<=v2-internal y x) nil)
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-<v2-variable x y) (restrict-true! z))
		     ((known?-<=v2-variable y x) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-<v2-variable x y) (restrict-true! z))
		     ((known?-<=v2-variable y x) (restrict-false! z))))
	    y)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (assert!-<v2 x y))
		     ((variable-false? z) (assert!-<=v2 y x))))
	    z)
	   z))))

(defun /=v2 (x y)
 (assert!-numberpv x)
 (assert!-numberpv y)
 (cond ((known?-/=v2-internal x y) t)
       ((known?-=v2-internal x y) nil)
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-/=v2-variable x y) (restrict-true! z))
		     ((known?-=v2-variable x y) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-/=v2-variable x y) (restrict-true! z))
		     ((known?-=v2-variable x y) (restrict-false! z))))
	    y)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (assert!-/=v2 x y))
		     ((variable-false? z) (assert!-=v2 x y))))
	    z)
	   z))))

;;; Lifted NOTV, ANDV and ORV

(defun notv (x)
 (assert!-booleanpv x)
 (let ((x (value-of x)))
  (cond ((eq x t) nil)
	((eq x nil) t)
	(t (let ((z (a-booleanv)))
	    (attach-noticer!
	     #'(lambda ()
		(cond ((variable-true? x) (restrict-false! z))
		      ((variable-false? x) (restrict-true! z))))
	     x)
	    (attach-noticer!
	     #'(lambda ()
		(cond ((variable-true? z) (restrict-false! x))
		      ((variable-false? z) (restrict-true! x))))
	     z)
	    z)))))

(defun andv-internal (xs)
 (dolist (x xs) (assert!-booleanpv x))
 (let ((xs (mapcar #'value-of xs)))
  (if (member nil xs :test #'eq)
      nil
      (let* ((xs (remove t xs :test #'eq))
	     (count (length xs)))
       (cond
	((zerop count) t)
	((= count 1) (first xs))
	(t (let ((z (a-booleanv)))
	    (attach-noticer!
	     #'(lambda ()
		(cond ((variable-true? z) (dolist (x xs) (restrict-true! x)))
		      ((and (= count 1) (variable-false? z))
		       (dolist (x xs)
			(unless (variable-true? x) (restrict-false! x))))))
	     z)
	    (dolist (x xs)
	     (let ((x x))
	      (attach-noticer!-internal
	       #'(lambda ()
		  (cond ((variable-false? x) (restrict-false! z))
			((variable-true? x)
			 (local (decf count))
			 (cond ((zerop count) (restrict-true! z))
			       ((and (= count 1) (variable-false? z))
				(dolist (x xs)
				 (unless (variable-true? x)
				  (restrict-false! x))))))))
	       x)))
	    z)))))))

(defun andv (&rest xs) (andv-internal xs))

(defun assert!-notv-andv-internal (xs)
 (dolist (x xs) (assert!-booleanpv x))
 (let ((xs (mapcar #'value-of xs)))
  (unless (member nil xs :test #'eq)
   (let* ((xs (remove t xs :test #'eq))
	  (count (length xs)))
    (cond ((zerop count) (fail))
	  ((= count 1) (restrict-false! (first xs)))
	  (t (dolist (x xs)
	      (let ((x x))
	       (attach-noticer!-internal
		#'(lambda ()
		   (cond ((variable-false? x))
			 ((variable-true? x)
			  (local (decf count))
			  (cond ((zerop count) (fail))
				((= count 1)
				 (dolist (x xs)
				  (unless (variable-true? x)
				   (restrict-false! x))))))))
		x)))))))))

(defun assert!-notv-andv (&rest xs) (assert!-notv-andv-internal xs))

(defun orv-internal (xs)
 (dolist (x xs) (assert!-booleanpv x))
 (let ((xs (mapcar #'value-of xs)))
  (if (member t xs :test #'eq)
      t
      (let* ((xs (remove nil xs :test #'eq))
	     (count (length xs)))
       (cond
	((zerop count) nil)
	((= count 1) (first xs))
	(t (let ((z (a-booleanv)))
	    (attach-noticer!
	     #'(lambda ()
		(cond ((variable-false? z)
		       (dolist (x xs) (restrict-false! x)))
		      ((and (= count 1) (variable-true? z))
		       (dolist (x xs)
			(unless (variable-false? x) (restrict-true! x))))))
	     z)
	    (dolist (x xs)
	     (let ((x x))
	      (attach-noticer!-internal
	       #'(lambda ()
		  (cond ((variable-true? x) (restrict-true! z))
			((variable-false? x)
			 (local (decf count))
			 (cond ((zerop count) (restrict-false! z))
			       ((and (= count 1) (variable-true? z))
				(dolist (x xs)
				 (unless (variable-false? x)
				  (restrict-true! x))))))))
	       x)))
	    z)))))))

(defun orv (&rest xs) (orv-internal xs))

(defun assert!-orv-internal (xs)
 (dolist (x xs) (assert!-booleanpv x))
 (let ((xs (mapcar #'value-of xs)))
  (unless (member t xs :test #'eq)
   (let* ((xs (remove nil xs :test #'eq))
	  (count (length xs)))
    (cond ((zerop count) (fail))
	  ((= count 1) (restrict-true! (first xs)))
	  (t (dolist (x xs)
	      (let ((x x))
	       (attach-noticer!-internal
		#'(lambda ()
		   (cond ((variable-true? x))
			 ((variable-false? x)
			  (local (decf count))
			  (cond ((zerop count) (fail))
				((= count 1)
				 (dolist (x xs)
				  (unless (variable-false? x)
				   (restrict-true! x))))))))
		x)))))))))

(defun assert!-orv (&rest xs) (assert!-orv-internal xs))

(defun assert!-clause (xs ps)
 (dolist (x xs) (assert!-booleanpv x))
 (let ((xs (mapcar #'value-of xs)))
  (unless (some #'eq xs ps)
   (let (new-xs new-ps)
    (do ((xrest xs (rest xrest))
	 (prest ps (rest prest)))
	((or (null xrest) (null prest)))
	(let ((x (first xrest))
	      (p (first prest)))
	 (unless (eq x (not p))
	  (push x new-xs)
	  (push p new-ps))))
    (let ((count (length new-xs)))
     (cond ((zerop count) (fail))
	   ((= count 1)
	    (if (first new-ps)
		(restrict-true! (first new-xs))
		(restrict-false! (first new-xs))))
	   (t (do ((xrest new-xs (rest xrest))
		   (prest new-ps (rest prest)))
		  ((null xrest))
		  (let ((x (first xrest)))
		   (attach-noticer!-internal
		    (if (first prest)
			#'(lambda ()
			   (cond ((variable-true? x))
				 ((variable-false? x)
				  (local (decf count))
				  (cond ((zerop count) (fail))
					((= count 1)
					 (do ((xrest new-xs (rest xrest))
					      (prest new-ps (rest prest)))
					     ((null xrest))
					     (let ((x (first xrest)))
					      (unless (bound? x)
					       (if (first prest)
						   (restrict-true! x)
						   (restrict-false! x))))))))))
			#'(lambda ()
			   (cond ((variable-false? x))
				 ((variable-true? x)
				  (local (decf count))
				  (cond
				   ((zerop count) (fail))
				   ((= count 1)
				    (do ((xrest new-xs (rest xrest))
					 (prest new-ps (rest prest)))
					((null xrest))
					(let ((x (first xrest)))
					 (unless (bound? x)
					  (if (first prest)
					      (restrict-true! x)
					      (restrict-false! x)))))))))))
		    x))))))))))

(defun count-trues-internal (xs) (count-if #'identity xs))

(defun count-trues (&rest xs) (count-trues-internal xs))

(defun count-truesv-internal (xs)
 (dolist (x xs) (assert!-booleanpv x))
 (let ((xs (mapcar #'value-of xs))
       (lower 0)
       (upper (length xs)))
  (dolist (x xs)
   (cond ((eq x t) (incf lower))
	 ((eq x nil) (decf upper))))
  (if (= lower upper)
      lower
      (let ((z (an-integer-betweenv lower upper))
	    (xs (remove-if #'bound? xs)))
       (attach-noticer!
	#'(lambda ()
	   (if (= upper (variable-lower-bound z))
	       (dolist (x xs)
		(unless (variable-false? x) (restrict-true! x))))
	   (if (= lower (variable-upper-bound z))
	       (dolist (x xs)
		(unless (variable-true? x) (restrict-false! x)))))
	z)
       (dolist (x xs)
	(let ((x x))
	 (attach-noticer!
	  #'(lambda ()
	     (cond ((variable-false? x)
		    (local (decf upper))
		    (restrict-upper-bound! z upper)
		    (if (= upper (variable-lower-bound z))
			(dolist (x xs)
			 (unless (variable-false? x) (restrict-true! x)))))
		   ((variable-true? x)
		    (local (incf lower))
		    (restrict-lower-bound! z lower)
		    (if (= lower (variable-upper-bound z))
			(dolist (x xs)
			 (unless (variable-true? x) (restrict-false! x)))))))
	  x)))
       z))))

(defun count-truesv (&rest xs) (count-truesv-internal xs))

;;; Lifted FUNCALLV and APPLYV

(defun finite-domain? (variable)
 (let ((variable (value-of variable)))
  (or (not (variable? variable))
      (not (eq (variable-enumerated-domain variable) t))
      (and (variable-integer? variable)
	   (variable-lower-bound variable)
	   (variable-upper-bound variable)))))

;;; note: SOLUTION, LINEAR-FORCE and STATIC-ORDERING were moved here to be
;;;       before KNOWN?-CONSTRAINT to avoid forward references to
;;;       nondeterministic functions.

(defun solution (x force-function)
 (funcall-nondeterministic
  (value-of force-function) (variables-in (value-of x)))
 (apply-substitution x))

(defun linear-force (variable)
 (let ((variable (value-of variable)))
  (if (variable? variable)
      (restrict-value!
       variable
       (cond ((not (eq (variable-enumerated-domain variable) t))
	      (a-member-of (variable-enumerated-domain variable)))
	     ((variable-integer? variable)
	      (if (variable-lower-bound variable)
		  (if (variable-upper-bound variable)
		      (an-integer-between
		       (variable-lower-bound variable)
		       (variable-upper-bound variable))
		      (an-integer-above (variable-lower-bound variable)))
		  (if (variable-upper-bound variable)
		      (an-integer-below (variable-upper-bound variable))
		      (an-integer))))
	     (t (error "It is only possible to linear force a variable that~%~
                        has a countable domain"))))))
 (value-of variable))

(defun static-ordering-internal (variables force-function)
 (if variables
     (let ((variable (value-of (first variables))))
      (cond ((variable? variable)
	     (funcall-nondeterministic force-function variable)
	     (static-ordering-internal variables force-function))
	    (t (static-ordering-internal (rest variables) force-function))))))

(defun static-ordering (force-function)
 ;; note: This closure will heap cons.
 (let ((force-function (value-of force-function)))
  #'(lambda (variables) (static-ordering-internal variables force-function))))

(defun known?-constraint (f polarity? x)
 (let ((f (value-of f)))
  (if (variable? f)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
  (unless (functionp f)
   (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
  (and (every #'finite-domain? x)
       (block exit
	(for-effects
	 (if (if polarity?
		 (not (apply f (solution x (static-ordering #'linear-force))))
		 (apply f (solution x (static-ordering #'linear-force))))
	     (return-from exit nil)))
	t))))

(defun propagate-gfc (predicate polarity? variables unassigned-variable)
 ;; note: UNASSIGNED-VARIABLE must be a variable which is not bound and
 ;;       all of the VARIABLES except the UNASSIGNED-VARIABLE must be bound.
 (let ((unassigned-variable (value-of unassigned-variable)))
  ;; There is no way to propagate a value to a variable that doesn't have an
  ;; enumerated domain.
  (if (and (not (eq (variable-enumerated-domain unassigned-variable) t))
	   (not (null (rest (variable-enumerated-domain
			     unassigned-variable)))))
      ;; note: Could do less consing if had LOCAL DELETE-IF-NOT.
      ;; note: Consing.
      (let* ((variable-values (mapcar #'value-of variables))
	     (new-enumerated-domain
	      (if polarity?
		  (remove-if-not
		   #'(lambda (value)
		      (apply predicate
			     ;; note: Consing.
			     (mapcar #'(lambda (variable variable-value)
					(if (eq variable unassigned-variable)
					    value
					    variable-value))
				     variables
				     variable-values)))
		   (variable-enumerated-domain unassigned-variable))
		  (remove-if
		   #'(lambda (value)
		      (apply predicate
			     ;; note: Consing.
			     (mapcar #'(lambda (variable variable-value)
					(if (eq variable unassigned-variable)
					    value
					    variable-value))
				     variables
				     variable-values)))
		   (variable-enumerated-domain unassigned-variable)))))
       (if (set-enumerated-domain! unassigned-variable new-enumerated-domain)
	   (run-noticers unassigned-variable))))))

(defun a-tuple (variables variable value)
 (if (null variables)
     nil
     (cons (cond ((eq (first variables) variable) value)
		 ((variable? (first variables))
		  (a-member-of (variable-enumerated-domain (first variables))))
		 (t (first variables)))
	   (a-tuple (rest variables) variable value))))

(defun propagate-ac (predicate polarity? variables)
 (unless (some #'(lambda (variable)
		  (and (variable? variable)
		       (eq (variable-enumerated-domain variable) t)))
	       variables)
  (dolist (variable variables)
   ;; note: Could do less consing if had LOCAL DELETE-IF-NOT.
   (if (variable? variable)
       (let ((new-enumerated-domain
	      (if polarity?
		  (remove-if-not
		   #'(lambda (value)
		      (possibly?
		       ;; note: Consing.
		       (apply predicate (a-tuple variables variable value))))
		   (variable-enumerated-domain variable))
		  (remove-if
		   #'(lambda (value)
		      (possibly?
		       ;; note: Consing.
		       (apply predicate (a-tuple variables variable value))))
		   (variable-enumerated-domain variable)))))
	(if (set-enumerated-domain! variable new-enumerated-domain)
	    (run-noticers variable)))))))

(defun assert!-constraint-gfc (predicate polarity? variables)
 (let ((predicate (value-of predicate))
       (multiple-unassigned-variables? nil)
       (unassigned-variable nil))
  (if (variable? predicate)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
  (unless (functionp predicate)
   (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
  (dolist (variable variables)
   (unless (bound? variable)
    (if unassigned-variable (setf multiple-unassigned-variables? t))
    (setf unassigned-variable variable)))
  (cond
   (multiple-unassigned-variables?
    ;; The case where two or more variables are unbound
    (let ((variables (copy-list variables)))
     (dolist (variable variables)
      (unless (bound? variable)
       (let ((variable variable))
	(attach-noticer!
	 #'(lambda ()
	    (global
	     (block exit
	      (let ((unassigned-variable nil))
	       (dolist (variable variables)
		(unless (bound? variable)
		 (if unassigned-variable (return-from exit))
		 (setf unassigned-variable variable)))
	       (if unassigned-variable
		   (propagate-gfc
		    predicate polarity? variables unassigned-variable)
		   (unless (if polarity?
			       (apply predicate (mapcar #'value-of variables))
			       (not (apply predicate
					   (mapcar #'value-of variables))))
		    (fail)))))))
	 variable))))))
   (unassigned-variable
    ;; The case where all but one of the variables are bound
    (propagate-gfc predicate polarity? variables unassigned-variable))
   ;; The case where all variables are bound
   ;; note: Consing.
   (t (unless (if polarity?
		  (apply predicate (mapcar #'value-of variables))
		  (not (apply predicate (mapcar #'value-of variables))))
       (fail))))))

(defun assert!-constraint-ac (predicate polarity? variables)
 (let ((predicate (value-of predicate)))
  (if (variable? predicate)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
  (unless (functionp predicate)
   (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
  (dolist (variable variables)
   (attach-noticer!
    #'(lambda () (propagate-ac predicate polarity? variables))
    variable))
  (propagate-ac predicate polarity? variables)))

(defun assert!-constraint (predicate polarity? variables)
 (ecase *strategy*
  (:gfc (assert!-constraint-gfc predicate polarity? variables))
  (:ac (assert!-constraint-ac predicate polarity? variables))))

(defun known?-funcallv (f &rest x) (known?-constraint f t x))

(defun known?-notv-funcallv (f &rest x) (known?-constraint f nil x))

(defun assert!-funcallv (f &rest x) (assert!-constraint f t x))

(defun assert!-notv-funcallv (f &rest x) (assert!-constraint f nil x))

(defun funcallv (f &rest x)
 (let ((f (value-of f)))
  (if (variable? f)
      (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
  (unless (functionp f)
   (error "The first argument to FUNCALLV must be a deterministic function"))
  (if (every #'bound? x)
      (apply f (mapcar #'value-of x))
      (let ((z (make-variable)))
       (assert!-constraint
	#'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
       (dolist (argument x)
	(attach-noticer!
	 #'(lambda ()
	    (if (every #'bound? x)
		(assert!-equalv z (apply f (mapcar #'value-of x)))))
	 argument))
       z))))

(defun arguments-for-applyv (x xs)
 (unless (bound? (first (last (cons x xs))))
  (error "The current implementation does not allow the last argument to~%~
          APPLYV to be an unbound variable"))
 (apply #'list* (mapcar #'value-of (cons x xs))))

(defun known?-applyv (f x &rest xs)
 (known?-constraint f t (arguments-for-applyv x xs)))

(defun known?-notv-applyv (f x &rest xs)
 (known?-constraint f nil (arguments-for-applyv x xs)))

(defun assert!-applyv (f x &rest xs)
 (assert!-constraint f t (arguments-for-applyv x xs)))

(defun assert!-notv-applyv (f x &rest xs)
 (assert!-constraint f nil (arguments-for-applyv x xs)))

(defun applyv (f x &rest xs)
 (let ((f (value-of f)))
  (if (variable? f)
      (error "The current implementation does not allow the first argument~%~
              of APPLYV to be an unbound variable"))
  (unless (functionp f)
   (error "The first argument to APPLYV must be a deterministic function"))
  (let ((arguments (arguments-for-applyv x xs)))
   (if (every #'bound? arguments)
       (apply f (mapcar #'value-of arguments))
       (let ((z (make-variable)))
	(assert!-constraint
	 #'(lambda (&rest x) (equal (first x) (apply f (rest x))))
	 t
	 (cons z arguments))
	(dolist (argument arguments)
	 (attach-noticer!
	  #'(lambda ()
	     (if (every #'bound? arguments)
		 (assert!-equalv z (apply f (mapcar #'value-of arguments)))))
	  argument))
	z)))))

;;; Lifted EQUALV

(defun known?-equalv (x y)
 (or (eql x y)
     (cond ((variable? x)
	    (and (not (eq (variable-value x) x))
		 (known?-equalv (variable-value x) y)))
	   ((variable? y)
	    (and (not (eq (variable-value y) y))
		 (known?-equalv x (variable-value y))))
	   (t (and (consp x)
		   (consp y)
		   (known?-equalv (car x) (car y))
		   (known?-equalv (cdr x) (cdr y)))))))

(defun assert!-equalv (x y)
 (unless (eql x y)
  (cond ((variable? x)
	 (cond ((not (eq (variable-value x) x))
		(assert!-equalv (variable-value x) y))
	       ((variable? y)
		(if (eq (variable-value y) y)
		    (share! x y)
		    (assert!-equalv x (variable-value y))))
	       (t (restrict-value! x y))))
	((variable? y)
	 (if (eq (variable-value y) y)
	     (restrict-value! y x)
	     (assert!-equalv x (variable-value y))))
	((and (consp x) (consp y))
	 (assert!-equalv (car x) (car y))
	 (assert!-equalv (cdr x) (cdr y)))
	(t (fail)))))

(defun known?-notv-equalv (x y) (one-value (progn (assert!-equalv x y) nil) t))

(defun assert!-notv-equalv (x y)
 ;; note: Can be made more efficient so that if you later find out that
 ;;       X and Y are KNOWN?-NUMBERPV you can then ASSERT!-/=V2.
 (if (known?-equalv x y) (fail))
 (unless (known?-notv-equalv x y)
  (let ((x (variablize x))
	(y (variablize y)))
   (attach-noticer! #'(lambda () (if (known?-equalv x y) (fail))) x)
   (attach-noticer! #'(lambda () (if (known?-equalv x y) (fail))) y))))

(defun equalv (x y)
 ;; note: Can be made more efficient and return an AND tree of individual
 ;;       constraints needed to make EQUALV true. This can be done also for
 ;;       the KNOWN? and ASSERT! versions.
 (cond ((known?-equalv x y) t)
       ((known?-notv-equalv x y) nil)
       (t (let ((x (variablize x))
		(y (variablize y))
		(z (a-booleanv)))
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-equalv x y) (restrict-true! z))
		     ((known?-notv-equalv x y) (restrict-false! z))))
	    x)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((known?-equalv x y) (restrict-true! z))
		     ((known?-notv-equalv x y) (restrict-false! z))))
	    y)
	   (attach-noticer!
	    #'(lambda ()
	       (cond ((variable-true? z) (assert!-equalv x y))
		     ((variable-false? z) (assert!-notv-equalv x y))))
	    z)
	   z))))

;;; Lifted Arithmetic Functions

(defun +v-internal (xs)
 (if (null xs) 0 (+v2 (first xs) (+v-internal (rest xs)))))

(defun +v (&rest xs) (+v-internal xs))

(defun -v-internal (x xs)
 (if (null xs) x (-v-internal (-v2 x (first xs)) (rest xs))))

(defun -v (x &rest xs) (if (null xs) (-v2 0 x) (-v-internal x xs)))

(defun *v-internal (xs)
 (if (null xs) 1 (*v2 (first xs) (*v-internal (rest xs)))))

(defun *v (&rest xs) (*v-internal xs))

(defun /v-internal (x xs)
 (if (null xs) x (/v-internal (/v2 x (first xs)) (rest xs))))

(defun /v (x &rest xs) (if (null xs) (/v2 1 x) (/v-internal x xs)))

(defun minv-internal (x xs)
 (if (null xs) x (minv-internal (minv2 x (first xs)) (rest xs))))

(defun minv (x &rest xs) (if (null xs) x (minv-internal x xs)))

(defun maxv-internal (x xs)
 (if (null xs) x (maxv-internal (maxv2 x (first xs)) (rest xs))))

(defun maxv (x &rest xs) (if (null xs) x (maxv-internal x xs)))

;;; Lifted Arithmetic Comparison Functions (KNOWN? optimized)

(defun known?-=v-internal (x xs)
 (if (null xs)
     t
     (and (known?-=v2 x (first xs))
	  (known?-=v-internal (first xs) (rest xs)))))

(defun known?-=v (x &rest xs) (known?-=v-internal x xs))

(defun known?-<v-internal (x xs)
 (if (null xs)
     t
     (and (known?-<v2 x (first xs))
	  (known?-<v-internal (first xs) (rest xs)))))

(defun known?-<v (x &rest xs) (known?-<v-internal x xs))

(defun known?-<=v-internal (x xs)
 (if (null xs)
     t
     (and (known?-<=v2 x (first xs))
	  (known?-<=v-internal (first xs) (rest xs)))))

(defun known?-<=v (x &rest xs) (known?-<=v-internal x xs))

(defun known?->v-internal (x xs)
 (if (null xs)
     t
     (and (known?-<v2 (first xs) x)
	  (known?->v-internal (first xs) (rest xs)))))

(defun known?->v (x &rest xs) (known?->v-internal x xs))

(defun known?->=v-internal (x xs)
 (if (null xs)
     t
     (and (known?-<=v2 (first xs) x)
	  (known?->=v-internal (first xs) (rest xs)))))

(defun known?->=v (x &rest xs) (known?->=v-internal x xs))

(defun known?-/=v-internal (x xs)
 (if (null xs)
     t
     (and (known?-/=v2 x (first xs))
	  (known?-/=v-internal x (rest xs))
	  (known?-/=v-internal (first xs) (rest xs)))))

(defun known?-/=v (x &rest xs) (known?-/=v-internal x xs))

;;; Lifted Arithmetic Comparison Functions (ASSERT! optimized)

(defun assert!-=v-internal (x xs)
 (unless (null xs)
  (assert!-=v2 x (first xs))
  (assert!-=v-internal (first xs) (rest xs))))

(defun assert!-=v (x &rest xs) (assert!-=v-internal x xs))

(defun assert!-<v-internal (x xs)
 (unless (null xs)
  (assert!-<v2 x (first xs))
  (assert!-<v-internal (first xs) (rest xs))))

(defun assert!-<v (x &rest xs) (assert!-<v-internal x xs))

(defun assert!-<=v-internal (x xs)
 (unless (null xs)
  (assert!-<=v2 x (first xs))
  (assert!-<=v-internal (first xs) (rest xs))))

(defun assert!-<=v (x &rest xs) (assert!-<=v-internal x xs))

(defun assert!->v-internal (x xs)
 (unless (null xs)
  (assert!-<v2 (first xs) x)
  (assert!->v-internal (first xs) (rest xs))))

(defun assert!->v (x &rest xs) (assert!->v-internal x xs))

(defun assert!->=v-internal (x xs)
 (unless (null xs)
  (assert!-<=v2 (first xs) x)
  (assert!->=v-internal (first xs) (rest xs))))

(defun assert!->=v (x &rest xs) (assert!->=v-internal x xs))

(defun assert!-/=v-internal (x xs)
 (unless (null xs)
  (assert!-/=v2 x (first xs))
  (assert!-/=v-internal x (rest xs))
  (assert!-/=v-internal (first xs) (rest xs))))

(defun assert!-/=v (x &rest xs) (assert!-/=v-internal x xs))

;;; Lifted Arithmetic Comparisons Functions

(defun =v-internal (x xs)
 (if (null xs)
     t
     (andv (=v2 x (first xs)) (=v-internal (first xs) (rest xs)))))

(defun =v (x &rest xs) (=v-internal x xs))

(defun <v-internal (x xs)
 (if (null xs)
     t
     (andv (<v2 x (first xs)) (<v-internal (first xs) (rest xs)))))

(defun <v (x &rest xs) (<v-internal x xs))

(defun <=v-internal (x xs)
 (if (null xs)
     t
     (andv (<=v2 x (first xs)) (<=v-internal (first xs) (rest xs)))))

(defun <=v (x &rest xs) (<=v-internal x xs))

(defun >v-internal (x xs)
 (if (null xs)
     t
     (andv (<v2 (first xs) x) (>v-internal (first xs) (rest xs)))))

(defun >v (x &rest xs) (>v-internal x xs))

(defun >=v-internal (x xs)
 (if (null xs)
     t
     (andv (<=v2 (first xs) x) (>=v-internal (first xs) (rest xs)))))

(defun >=v (x &rest xs) (>=v-internal x xs))

(defun /=v-internal (x xs)
 (if (null xs)
     t
     (andv (/=v2 x (first xs))
	   (/=v-internal x (rest xs))
	   (/=v-internal (first xs) (rest xs)))))

(defun /=v (x &rest xs) (/=v-internal x xs))

;;; The Optimizer Macros for ASSERT!, KNOWN? and DECIDE

(defun known?-true (x) (assert!-booleanpv x) (eq (value-of x) t))

(defun known?-false (x) (assert!-booleanpv x) (null (value-of x)))

(defun-compile-time transform-known? (form polarity?)
 (if (and (consp form) (null (rest (last form))))
     (cond
      ((and (eq (first form) 'notv)
	    (= (length form) 2))
       (transform-known? (second form) (not polarity?)))
      ((eq (first form) 'andv)
       (cons (if polarity? 'and 'or)
	     (mapcar #'(lambda (form) (transform-known? form polarity?))
		     (rest form))))
      ((eq (first form) 'orv)
       (cons (if polarity? 'or 'and)
	     (mapcar #'(lambda (form) (transform-known? form polarity?))
		     (rest form))))
      ((member (first form)
	       '(integerpv realpv numberpv memberv booleanpv
			   =v <v <=v >v >=v /=v funcallv applyv equalv)
	       :test #'eq)
       (cons (cdr (assoc (first form)
			 (if polarity?
			     '((integerpv . known?-integerpv)
			       (realpv . known?-realpv)
			       (numberpv . known?-numberpv)
			       (memberv . known?-memberv)
			       (booleanpv . known?-booleanpv)
			       (=v . known?-=v)
			       (<v . known?-<v)
			       (<=v . known?-<=v)
			       (>v . known?->v)
			       (>=v . known?->=v)
			       (/=v . known?-/=v)
			       (funcallv . known?-funcallv)
			       (applyv . known?-applyv)
			       (equalv . known?-equalv))
			     '((integerpv . known?-notv-integerpv)
			       (realpv . known?-notv-realpv)
			       (numberpv . known?-notv-numberpv)
			       (memberv . known?-notv-memberv)
			       (booleanpv . known?-notv-booleanpv)
			       (=v . known?-/=v)
			       (<v . known?->=v)
			       (<=v . known?->v)
			       (>v . known?-<=v)
			       (>=v . known?-<v)
			       (/=v . known?-=v)
			       (funcallv . known?-notv-funcallv)
			       (applyv . known?-notv-applyv)
			       (equalv . known?-notv-equalv)))
			 :test #'eq))
	     (rest form)))
      (polarity? `(known?-true ,form))
      (t `(known?-false ,form)))
     (if polarity? `(known?-true ,form) `(known?-false ,form))))

(defmacro-compile-time known? (form) (transform-known? form t))

(defun assert!-true (x) (assert!-equalv x t))

(defun assert!-false (x) (assert!-equalv x nil))

(defun-compile-time transform-assert! (form polarity?)
 (if (and (consp form) (null (rest (last form))))
     (cond
      ((and (eq (first form) 'notv)
	    (= (length form) 2))
       (transform-assert! (second form) (not polarity?)))
      ((eq (first form) 'andv)
       (if polarity?
	   `(progn ,@(mapcar
		      #'(lambda (form) (transform-assert! form polarity?))
		      (rest form)))
	   (cond ((null (rest form)) `(fail))
		 ((null (rest (rest form))) `(assert!-false ,(second form)))
		 (t `(assert!-notv-andv ,@(rest form))))))
      ((eq (first form) 'orv)
       (if polarity?
	   (cond ((null (rest form)) `(fail))
		 ((null (rest (rest form))) `(assert!-true ,(second form)))
		 (t `(assert!-orv ,@(rest form))))
	   `(progn ,@(mapcar
		      #'(lambda (form) (transform-assert! form polarity?))
		      (rest form)))))
      ((member (first form)
	       '(integerpv realpv numberpv memberv booleanpv
			   =v <v <=v >v >=v /=v funcallv applyv equalv)
	       :test #'eq)
       (cons (cdr (assoc (first form)
			 (if polarity?
			     '((integerpv . assert!-integerpv)
			       (realpv . assert!-realpv)
			       (numberpv . assert!-numberpv)
			       (memberv . assert!-memberv)
			       (booleanpv . assert!-booleanpv)
			       (=v . assert!-=v)
			       (<v . assert!-<v)
			       (<=v . assert!-<=v)
			       (>v . assert!->v)
			       (>=v . assert!->=v)
			       (/=v . assert!-/=v)
			       (funcallv . assert!-funcallv)
			       (applyv . assert!-applyv)
			       (equalv . assert!-equalv))
			     '((integerpv . assert!-notv-integerpv)
			       (realpv . assert!-notv-realpv)
			       (numberpv . assert!-notv-numberpv)
			       (memberv . assert!-notv-memberv)
			       (booleanpv . assert!-notv-booleanpv)
			       (=v . assert!-/=v)
			       (<v . assert!->=v)
			       (<=v . assert!->v)
			       (>v . assert!-<=v)
			       (>=v . assert!-<v)
			       (/=v . assert!-=v)
			       (funcallv . assert!-notv-funcallv)
			       (applyv . assert!-notv-applyv)
			       (equalv . assert!-notv-equalv)))
			 :test #'eq))
	     (rest form)))
      (polarity? `(assert!-true ,form))
      (t `(assert!-false ,form)))
     (if polarity? `(assert!-true ,form) `(assert!-false ,form))))

(defmacro-compile-time assert! (form) (transform-assert! form t))

(defun-compile-time transform-decide (form polarity?)
 (if (and (consp form) (null (rest (last form))))
     (cond
      ((and (eq (first form) 'notv)
	    (= (length form) 2))
       (transform-decide (second form) (not polarity?)))
      ((eq (first form) 'andv)
       (let ((result (mapcar #'(lambda (form)
				(multiple-value-list
				 (transform-decide form polarity?)))
			     (rest form))))
	(values (reduce #'append (mapcar #'first result))
		(cons (if polarity? 'progn 'either)
		      (mapcar #'second result))
		(cons (if polarity? 'either 'progn)
		      (mapcar #'third result)))))
      ((eq (first form) 'orv)
       (let ((result (mapcar #'(lambda (form)
				(multiple-value-list
				 (transform-decide form polarity?)))
			     (rest form))))
	(values (reduce #'append (mapcar #'first result))
		(cons (if polarity? 'either 'progn)
		      (mapcar #'second result))
		(cons (if polarity? 'progn 'either)
		      (mapcar #'third result)))))
      ((member (first form)
	       '(integerpv realpv numberpv memberv booleanpv
			   =v <v <=v >v >=v /=v funcallv applyv equalv)
	       :test #'eq)
       (let ((arguments (mapcar #'(lambda (argument)
				   (declare (ignore argument))
				   (gensym "ARGUMENT-"))
				(rest form))))
	(values (mapcar #'list arguments (rest form))
		(cons (cdr (assoc (first form)
				  (if polarity?
				      '((integerpv . assert!-integerpv)
					(realpv . assert!-realpv)
					(numberpv . assert!-numberpv)
					(memberv . assert!-memberv)
					(booleanpv . assert!-booleanpv)
					(=v . assert!-=v)
					(<v . assert!-<v)
					(<=v . assert!-<=v)
					(>v . assert!->v)
					(>=v . assert!->=v)
					(/=v . assert!-/=v)
					(funcallv . assert!-funcallv)
					(applyv . assert!-applyv)
					(equalv . assert!-equalv))
				      '((integerpv . assert!-notv-integerpv)
					(realpv . assert!-notv-realpv)
					(numberpv . assert!-notv-numberpv)
					(memberv . assert!-notv-memberv)
					(booleanpv . assert!-notv-booleanpv)
					(=v . assert!-/=v)
					(<v . assert!->=v)
					(<=v . assert!->v)
					(>v . assert!-<=v)
					(>=v . assert!-<v)
					(/=v . assert!-=v)
					(funcallv . assert!-notv-funcallv)
					(applyv . assert!-notv-applyv)
					(equalv . assert!-notv-equalv)))
				  :test #'eq))
		      arguments)
		(cons (cdr (assoc (first form)
				  (if polarity?
				      '((integerpv . assert!-notv-integerpv)
					(realpv . assert!-notv-realpv)
					(numberpv . assert!-notv-numberpv)
					(memberv . assert!-notv-memberv)
					(booleanpv . assert!-notv-booleanpv)
					(=v . assert!-/=v)
					(<v . assert!->=v)
					(<=v . assert!->v)
					(>v . assert!-<=v)
					(>=v . assert!-<v)
					(/=v . assert!-=v)
					(funcallv . assert!-notv-funcallv)
					(applyv . assert!-notv-applyv)
					(equalv . assert!-notv-equalv))
				      '((integerpv . assert!-integerpv)
					(realpv . assert!-realpv)
					(numberpv . assert!-numberpv)
					(memberv . assert!-memberv)
					(booleanpv . assert!-booleanpv)
					(=v . assert!-=v)
					(<v . assert!-<v)
					(<=v . assert!-<=v)
					(>v . assert!->v)
					(>=v . assert!->=v)
					(/=v . assert!-/=v)
					(funcallv . assert!-funcallv)
					(applyv . assert!-applyv)
					(equalv . assert!-equalv)))
				  :test #'eq))
		      arguments))))
      (t (let ((argument (gensym "ARGUMENT-")))
	  (values (list (list argument form))
		  (if polarity?
		      `(assert!-true ,argument)
		      `(assert!-false ,argument))
		  (if polarity?
		      `(assert!-false ,argument)
		      `(assert!-true ,argument))))))
     (let ((argument (gensym "ARGUMENT-")))
      (values
       (list (list argument form))
       (if polarity? `(assert!-true ,argument) `(assert!-false ,argument))
       (if polarity? `(assert!-false ,argument) `(assert!-true ,argument))))))

(defmacro-compile-time decide (form)
 (cl:multiple-value-bind (arguments true false)
   (transform-decide form t)
  `(let ,arguments (either (progn ,true t) (progn ,false nil)))))

;;; Lifted Generators
;;; note: The following functions could be handled more efficiently as special
;;;       cases.

(defun a-booleanv (&optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (booleanpv v))
  v))

(defun an-integerv (&optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (integerpv v))
  v))

(defun an-integer-abovev (low &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (andv (integerpv v) (>=v v low)))
  v))

(defun an-integer-belowv (high &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (andv (integerpv v) (<=v v high)))
  v))

(defun an-integer-betweenv (low high &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (andv (integerpv v) (>=v v low) (<=v v high)))
  (value-of v)))

(defun a-realv (&optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (realpv v))
  v))

(defun a-real-abovev (low &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (andv (realpv v) (>=v v low)))
  v))

(defun a-real-belowv (high &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (andv (realpv v) (<=v v high)))
  v))

(defun a-real-betweenv (low high &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (andv (realpv v) (>=v v low) (<=v v high)))
  v))

(defun a-numberv (&optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (numberpv v))
  v))

(defun a-member-ofv (values &optional (name nil name?))
 (let ((v (if name? (make-variable name) (make-variable))))
  (assert! (memberv v values))
  (value-of v)))

;;; Search Control

(defun variables-in (x)
 (typecase x
  (cons (append (variables-in (car x)) (variables-in (cdr x))))
  (variable (list x))
  (otherwise nil)))

;;; note: SOLUTION and LINEAR-FORCE used to be here but was moved to be before
;;;       KNOWN?-CONSTRAINT to avoid forward references to nondeterministic
;;;       functions.

(defun divide-and-conquer-force (variable)
 (let ((variable (value-of variable)))
  (if (variable? variable)
      (cond
       ((not (eq (variable-enumerated-domain variable) t))
	(let ((n (floor (length (variable-enumerated-domain variable)) 2)))
	 (set-enumerated-domain!
	  variable
	  (either (subseq (variable-enumerated-domain variable) 0 n)
		  (subseq (variable-enumerated-domain variable) n)))
	 (run-noticers variable)))
       ((and (variable-real? variable)
	     (variable-lower-bound variable)
	     (variable-upper-bound variable))
	(if (variable-integer? variable)
	    (let ((midpoint (floor (+ (variable-lower-bound variable)
				      (variable-upper-bound variable))
				   2)))
	     (either (let ((old-bound (variable-upper-bound variable)))
		      (restrict-upper-bound! variable midpoint)
		      (if (= old-bound (variable-upper-bound variable))
			  (fail)))
		     (let ((old-bound (variable-lower-bound variable)))
		      (restrict-lower-bound! variable (1+ midpoint))
		      (if (= old-bound (variable-lower-bound variable))
			  (fail)))))
	    (let ((midpoint (/ (+ (variable-lower-bound variable)
				  (variable-upper-bound variable))
			       2)))
	     (either (let ((old-bound (variable-upper-bound variable)))
		      (restrict-upper-bound! variable midpoint)
		      (if (= old-bound (variable-upper-bound variable))
			  (fail)))
		     (let ((old-bound (variable-lower-bound variable)))
		      (restrict-lower-bound! variable midpoint)
		      (if (= old-bound (variable-lower-bound variable))
			  (fail)))))))
       (t (error "It is only possible to divide and conquer force a~%~
                  variable that has a countable domain or a finite range")))))
 (value-of variable))

;;; note: STATIC-ORDERING used to be here but was moved to be before
;;;       KNOWN?-CONSTRAINT to avoid a forward reference to a nondeterministic
;;;       function.

(defun domain-size (x)
 (let ((x (value-of x)))
  (typecase x
   (cons (infinity-* (domain-size (car x)) (domain-size (cdr x))))
   (variable
    (cond ((not (eq (variable-enumerated-domain x) t))
	   (length (variable-enumerated-domain x)))
	  ((and (variable-lower-bound x)
		(variable-upper-bound x)
		(variable-integer? x))
	   (1+ (- (variable-upper-bound x) (variable-lower-bound x))))
	  (t nil)))
   (otherwise 1))))

(defun range-size (x)
 (let ((x (value-of x)))
  (typecase x
   (integer 0)
   (real 0.0)
   (variable (and (variable-real? x)
		  (variable-lower-bound x)
		  (variable-upper-bound x)
		  (- (variable-upper-bound x) (variable-lower-bound x))))
   (otherwise nil))))

(defun corrupted? (variable)
 (let* ((lower-bound (variable-lower-bound variable))
	(upper-bound (variable-upper-bound variable)))
  (and lower-bound
       upper-bound
       (/= lower-bound upper-bound)
       (let ((midpoint (/ (+ lower-bound upper-bound) 2)))
	(or (= midpoint lower-bound) (= midpoint upper-bound))))))

(defun find-best (cost order list)
 (let ((best nil)
       (best-cost nil))
  (dolist (x list)
   (let ((x (value-of x)))
    (if (and (variable? x) (not (corrupted? x)))
	(let ((cost (funcall cost x)))
	 (when (and (not (null cost))
		    (or (null best-cost) (funcall order cost best-cost)))
	  (setf best x)
	  (setf best-cost cost))))))
  best))

(defun reorder-internal
  (variables cost-function terminate? order force-function)
 (let ((variable (find-best cost-function order variables)))
  (when (and variable
	     (not (funcall terminate? (funcall cost-function variable))))
   (funcall-nondeterministic force-function (value-of variable))
   (reorder-internal
    variables cost-function terminate? order force-function))))

(defun reorder (cost-function terminate? order force-function)
 ;; note: This closure will heap cons.
 (let ((cost-function (value-of cost-function))
       (terminate? (value-of terminate?))
       (order (value-of order))
       (force-function (value-of force-function)))
  #'(lambda (variables)
     (reorder-internal
      variables cost-function terminate? order force-function))))

(defmacro-compile-time best-value
 (form1 objective-form &optional (form2 nil form2?))
 (let ((bound (gensym "BOUND-"))
       (best (gensym "BEST-"))
       (objective (gensym "OBJECTIVE-")))
  `(let ((,bound nil)
	 (,best nil)
	 (,objective (variablize ,objective-form)))
    (attach-noticer!
     #'(lambda ()
	(if (and ,bound (<= (variable-upper-bound ,objective) ,bound)) (fail)))
     ,objective)
    (for-effects
     (let ((value ,form1))
      (global (setf ,bound (variable-upper-bound ,objective))
	      (setf ,best value))))
    (if ,bound (list ,best ,bound) ,(if form2? form2 '(fail))))))

;;; Harlequin expands MUTLIPLE-VALUE-BIND into a Harlequin-specific special
;;; form SYSTEM::MULTIPLE-VALUE-BIND-CALL which we don't yet know how to walk.
;;; So for now, we don't support TEMPLATE under Harlequin.
#-harlequin-common-lisp
(defun template-internal (template variables)
 (cond
  ((and (symbolp template) (char= #\? (aref (string template) 0)))
   (let ((binding (assoc template variables :test #'eq)))
    (if binding
	(values (cdr binding) variables)
	(let ((variable (make-variable template)))
	 (values variable (cons (cons template variable) variables))))))
  ((consp template)
   (cl:multiple-value-bind (car-template car-variables)
     (template-internal (car template) variables)
    (cl:multiple-value-bind (cdr-template cdr-variables)
      (template-internal (cdr template) car-variables)
     (values (cons car-template cdr-template) cdr-variables))))
  (t (values template variables))))

#-harlequin-common-lisp
(defun template (template) (template-internal (value-of template) '()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *screamer?* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :screamer *features* :test #'eq))

;;; Tam V'Nishlam Shevah L'El Borei Olam
