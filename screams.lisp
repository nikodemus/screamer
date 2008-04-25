;;; -*- Mode: LISP; Package: (SCREAMS :USE CL :COLON-MODE :EXTERNAL); Base: 10; Syntax: Ansi-common-lisp -*-

;;; LaHaShem HaAretz U'Mloah

#||#(in-package :screamer-user)

#-(or poplog akcl)
(screamer:define-screamer-package :screams (:use :iterate))

#-(or poplog akcl)
(in-package :screams)

#+(or poplog akcl)
(use-package :iterate)

(defun pythagorean-triples (n)
 (all-values
  (let ((a (an-integer-between 1 n))
        (b (an-integer-between 1 n))
        (c (an-integer-between 1 n)))
   (unless (= (+ (* a a) (* b b)) (* c c)) (fail))
   (list a b c))))

(defun pythagorean-triplesv (n)
 (all-values
  (solution
   (let ((a (an-integer-betweenv 1 n))
	 (b (an-integer-betweenv 1 n))
	 (c (an-integer-betweenv 1 n)))
    (assert! (=v (+v (*v a a) (*v b b)) (*v c c)))
    (list a b c))
   (reorder #'range-size
	    #'(lambda (x) (< x 1e-6))
	    #'>
	    #'divide-and-conquer-force))))

(defun attacks? (qi qj distance)
 (or (= qi qj) (= (abs (- qi qj)) distance)))
 
(defun check-queens (queen queens &optional (distance 1))
 (unless (null queens)
  (if (attacks? queen (first queens) distance) (fail))
  (check-queens queen (rest queens) (1+ distance))))
 
(defun n-queens (n &optional queens)
 (if (= (length queens) n)
     queens
     (let ((queen (an-integer-between 1 n)))
      (check-queens queen queens)
      (n-queens n (cons queen queens)))))

(defun n-queensv (n)
 (solution
  (let ((q (make-array n)))
   (dotimes (i n) (setf (aref q i) (an-integer-betweenv 1 n)))
   (dotimes (i n)
    (dotimes (j n)
     (if (> j i)
	 (assert!
	  (notv (funcallv #'attacks? (aref q i) (aref q j) (- j i)))))))
   (coerce q 'list))
  (reorder #'domain-size
	   #'(lambda (x) (declare (ignore x)) nil)
	   #'<
	   #'linear-force)))

(defun a-subset-of (x)
 (if (null x)
     nil
     (let ((y (a-subset-of (rest x)))) (either (cons (first x) y) y))))

(defun a-partition-of (x)
 (if (null x)
     x
     (let ((y (a-partition-of (rest x))))
      (either (cons (list (first x)) y)
	      (let ((z (a-member-of y)))
	       (cons (cons (first x) z) (remove z y :test #'eq :count 1)))))))

(defstruct (node (:conc-name nil) (:print-function print-node))
  name next-nodes (visited? nil) (visits 0))

(defun print-node (node stream print-level)
 (declare (ignore print-level))
 (princ (name node) stream))

(defun simple-path (u v)
 (if (visited? u) (fail))
 (local (setf (visited? u) t))
 (either (progn (unless (eq u v) (fail)) (list u))
         (cons u (simple-path (a-member-of (next-nodes u)) v))))

(defun k-simple-path (u v k)
 (if (= (visits u) k) (fail))
 ;; This can't be (LOCAL (INCF (VISITS U))) since Lucid screws up here.
 (local (setf (visits u) (1+ (visits u))))
 (either (progn (unless (eq u v) (fail)) (list u))
         (cons u (k-simple-path (a-member-of (next-nodes u)) v k))))

(defun graph ()
 (let ((a (make-node :name 'a))
       (b (make-node :name 'b))
       (c (make-node :name 'c))
       (d (make-node :name 'd))
       (e (make-node :name 'e))
       (f (make-node :name 'f)))
  (setf (next-nodes a) (list b c))
  (setf (next-nodes b) (list a d e))
  (setf (next-nodes c) (list a d e))
  (setf (next-nodes d) (list b c f))
  (setf (next-nodes e) (list b c f))
  (setf (next-nodes f) (list d e))
  (list (all-values (simple-path a f))
	(all-values (k-simple-path a f 2)))))

(defstruct (boolean-variable (:conc-name nil)) (value :unassigned) noticers)

(defun notb (x)
 (let ((z (make-boolean-variable)))
  (local (push #'(lambda () (set-value x (not (value z)))) (noticers z))
         (push #'(lambda () (set-value z (not (value x)))) (noticers x)))
  z))

(defun andb (x y)
 (let ((z (make-boolean-variable)))
  (local
   (push #'(lambda ()
            (cond ((value x)
                   (unless (eq (value y) :unassigned) (set-value z (value y)))
                   (unless (eq (value z) :unassigned) (set-value y (value z))))
                  (t (set-value z nil))))
         (noticers x))
   (push #'(lambda ()
            (cond ((value y)
                   (unless (eq (value x) :unassigned) (set-value z (value x)))
                   (unless (eq (value z) :unassigned) (set-value x (value z))))
                  (t (set-value z nil))))
         (noticers y))
   (push #'(lambda ()
            (cond ((value z) (set-value x t) (set-value y t))
                  (t (if (eq (value x) t) (set-value y nil))
                     (if (eq (value y) t) (set-value x nil)))))
         (noticers z))
   z)))

(defun orb (x y) (notb (andb (notb x) (notb y))))

(defun set-value (variable value)
 (cond ((eq (value variable) :unassigned)
        (local (setf (value variable) value))
        (dolist (noticer (noticers variable)) (funcall noticer)))
       (t (unless (eq (value variable) value) (fail)))))

(defun boolean-solution (variables)
 (if (null variables)
     '()
     (let ((variable (first variables)))
      (when (eq (value variable) :unassigned)
       (set-value variable (either t nil)))
      (cons (value variable) (boolean-solution (rest variables))))))

(defun sat-problem ()
 (all-values
  (let ((x (make-boolean-variable))
	(y (make-boolean-variable))
	(z (make-boolean-variable)))
   (set-value (andb (orb x (notb y)) (orb y (notb z))) t)
   (boolean-solution (list x y z)))))

(defvar *grammar* '((s np vp)
		    (np det n)
		    (np n)
		    (vp v)
		    (vp v np)
		    (vp v np np)
		    (vp v pp)
		    (vp v np pp)
		    (pp p np)))

(defun lhs (rule) (car rule))

(defun rhs (rule) (cdr rule))

(defun categories (grammar)
 (remove-duplicates
  (set-difference (reduce #'append grammar) (mapcar #'first grammar)
		  :test #'eq)
  :test #'eq))

(defun parse-categories (categories words1 &optional words2)
 (if (null categories)
     (if (and (null words1) (null words2)) t (fail))
     (either (progn (parse (first categories) words1)
                    (parse-categories (rest categories) words2))
             (if (null words1)
                 (fail)
                 (parse-categories
                  categories
                  (reverse (rest (reverse words1)))
                  (append (last words1) words2))))))

(defun parse-rule (category words rules)
 (if (null rules)
     (fail)
     (either (if (eq (lhs (first rules)) category)
                 (parse-categories (rhs (first rules)) words)
                 (fail))
             (parse-rule category words (rest rules)))))

(defun parse (category words)
 (if (null (rest words))
     (if (eq category (category (first words))) t (fail))
     (parse-rule category words *grammar*)))

(defun category (word)
 (declare (special lexicon))
 (let ((category (gethash word lexicon)))
  (if category
      category
      (local (setf (gethash word lexicon)
		   (a-member-of (categories *grammar*)))))))

(defun grow-up ()
 (all-values
  (let ((lexicon (make-hash-table :test #'eq)))
   (declare (special lexicon))
   (progn (parse 's '(the cup slid from john to mary))
	  (parse 's '(john walked to the table)))
   (iterate (for (word category) in-hashtable lexicon)
	    (format t "~%~S: ~S" word category)))))

(defun parse-categoriesv (categories words1 &optional words2)
 (if (null categories)
     (if (and (null words1) (null words2)) t NIL)
     (ORV (progn (parsev (first categories) words1)
                 (parse-categoriesv (rest categories) words2))
          (if (null words1)
              NIL
              (parse-categoriesv
               categories
               (reverse (rest (reverse words1)))
               (append (last words1) words2))))))

(defun parse-rulev (category words rules)
 (if (null rules)
     NIL
     (ORV (if (eq (lhs (first rules)) category)
              (parse-categoriesv (rhs (first rules)) words)
              NIL)
          (parse-rulev category words (rest rules)))))

(defun parsev (category words)
 (if (null (rest words))
     (EQUALV CATEGORY (CATEGORYV (FIRST WORDS)))
     (parse-rulev category words *grammar*)))

(defun categoryv (word)
 (declare (special lexicon))
 (let ((category (gethash word lexicon)))
  (if category
      category
      (setf (gethash word lexicon) (A-MEMBER-OFV (categories *grammar*))))))

(defun grow-upv ()
 (let ((lexicon (make-hash-table :test #'eq)))
  (declare (special lexicon))
  (ASSERT! (ANDV (parsev 's '(the cup slid from john to mary))
		 (parsev 's '(john walked to the table))))
  (all-values
   (FUNCALL (REORDER #'DOMAIN-SIZE
		     #'(LAMBDA (X) (DECLARE (IGNORE X)) NIL)
		     #'<
		     #'LINEAR-FORCE)
	    (ITERATE (FOR (WORD CATEGORY) IN-HASHTABLE LEXICON)
		     ;; note: This declaration causes a warning under MCL 2.0
		     ;;       but if you leave it out you still get a warning
		     ;;       so I don't see what one can do here.
		     (DECLARE (IGNORE WORD))
		     (COLLECT CATEGORY)))
   (iterate (for (word category) in-hashtable lexicon)
	    (format t "~%~S: ~S" word category)))))

(defvar *puzzle1* '((1 1 across 5)
                    (1 12 across 2)
                    (2 4 across 4)
                    (2 10 across 6)
                    (3 1 across 4)
                    (3 12 across 2)
                    (4 1 across 2)
                    (4 4 across 2)
                    (4 8 across 4)
                    (5 8 across 5)
                    (6 1 across 3)
                    (6 10 across 4)
                    (7 7 across 4)
                    (7 13 across 3)
                    (8 1 across 2)
                    (8 5 across 4)
                    (10 6 across 2)
                    (10 14 across 2)
                    (11 3 across 4)
                    (12 6 across 3)
                    (12 10 across 5)
                    (13 1 across 6)
                    (14 1 across 4)
                    (3 1 down 6)
                    (12 1 down 4)
                    (3 2 down 2)
                    (13 2 down 2)
                    (13 3 down 2)
                    (1 4 down 4)
                    (10 4 down 5)
                    (1 5 down 2)
                    (4 5 down 5)
                    (8 6 down 6)
                    (7 7 down 2)
                    (4 8 down 2)
                    (7 8 down 2)
                    (4 9 down 2)
                    (2 10 down 6)
                    (12 10 down 3)
                    (4 11 down 3)
                    (1 12 down 3)
                    (5 12 down 2)
                    (1 13 down 3)
                    (6 13 down 2)
                    (10 14 down 3)
                    (6 15 down 5)))

(defvar *words1* '("ache"
                   "adults"
                   "am"
                   "an"
                   "ax"
                   "bandit"
                   "bath"
                   "below"
                   "cave"
                   "dean"
                   "dig"
                   "do"
                   "dots"
                   "ef"
                   "eh"
                   "enjoys"
                   "era"
                   "es"
                   "fade"
                   "fee"
                   "him"
                   "incur"
                   "jo"
                   "knee"
                   "la"
                   "large"
                   "lie"
                   "ma"
                   "mops"
                   "on"
                   "ow"
                   "owe"
                   "pair"
                   "pi"
                   "re"
                   "royal"
                   "run"
                   "squad"
                   "sticks"
                   "string"
                   "ti"
                   "ut"
                   "veils"
                   "you"
                   "zero"))

(defvar *puzzle2* '((1 1 across 3)
                    (1 5 across 5)
                    (1 11 across 4)
                    (2 1 across 3)
                    (2 5 across 5)
                    (2 11 across 5)
                    (3 1 across 7)
                    (3 9 across 4)
                    (3 14 across 2)
                    (4 4 across 3)
                    (4 8 across 4)
                    (4 13 across 3)
                    (5 1 across 5)
                    (5 7 across 4)
                    (5 12 across 4)
                    (6 1 across 4)
                    (6 6 across 4)
                    (6 11 across 3)
                    (7 1 across 3)
                    (7 5 across 4)
                    (7 10 across 6)
                    (8 1 across 2)
                    (8 4 across 4)
                    (8 9 across 4)
                    (8 14 across 2)
                    (9 1 across 6)
                    (9 8 across 4)
                    (9 13 across 3)
                    (10 3 across 3)
                    (10 7 across 4)
                    (10 12 across 4)
                    (11 1 across 4)
                    (11 6 across 4)
                    (11 11 across 5)
                    (12 1 across 3)
                    (12 5 across 4)
                    (12 10 across 3)
                    (13 1 across 2)
                    (13 4 across 4)
                    (13 9 across 7)
                    (14 1 across 5)
                    (14 7 across 5)
                    (14 13 across 3)
                    (15 2 across 4)
                    (15 7 across 5)
                    (15 13 across 3)
                    (1 1 down 3)
                    (5 1 down 5)
                    (11 1 down 4)
                    (1 2 down 3)
                    (5 2 down 5)
                    (11 2 down 5)
                    (1 3 down 3)
                    (5 3 down 3)
                    (9 3 down 4)
                    (14 3 down 2)
                    (3 4 down 4)
                    (8 4 down 4)
                    (13 4 down 3)
                    (1 5 down 5)
                    (7 5 down 4)
                    (12 5 down 4)
                    (1 6 down 4)
                    (6 6 down 4)
                    (11 6 down 3)
                    (1 7 down 3)
                    (5 7 down 4)
                    (10 7 down 6)
                    (1 8 down 2)
                    (4 8 down 4)
                    (9 8 down 4)
                    (14 8 down 2)
                    (1 9 down 6)
                    (8 9 down 4)
                    (13 9 down 3)
                    (3 10 down 3)
                    (7 10 down 4)
                    (12 10 down 4)
                    (1 11 down 4)
                    (6 11 down 4)
                    (11 11 down 5)
                    (1 12 down 3)
                    (5 12 down 4)
                    (10 12 down 4)
                    (1 13 down 2)
                    (4 13 down 4)
                    (9 13 down 3)
                    (13 13 down 3)
                    (1 14 down 5)
                    (7 14 down 5)
                    (13 14 down 3)
                    (2 15 down 4)
                    (7 15 down 5)
                    (13 15 down 3)))

(defvar *words2* '("ad"
                   "al"
                   "alas"
                   "aloha"
                   "art"
                   "at"
                   "atl"
                   "bags"
                   "bang"
                   "base"
                   "bore"
                   "coat"
                   "dad"
                   "dart"
                   "dime"
                   "dine"
                   "dive"
                   "do"
                   "eh"
                   "elf"
                   "er"
                   "evade"
                   "even"
                   "fan"
                   "fee"
                   "fine"
                   "gate"
                   "goat"
                   "happy"
                   "hares"
                   "hem"
                   "hide"
                   "hire"
                   "hive"
                   "hoe"
                   "hone"
                   "inn"
                   "largest"
                   "learned"
                   "lee"
                   "lemons"
                   "lid"
                   "lilac"
                   "lip"
                   "lo"
                   "load"
                   "mates"
                   "mile"
                   "mirror"
                   "mist"
                   "moon"
                   "more"
                   "oak"
                   "olive"
                   "ore"
                   "pans"
                   "paris"
                   "pay"
                   "pea"
                   "pedal"
                   "penny"
                   "pier"
                   "pile"
                   "pins"
                   "pits"
                   "raise"
                   "rips"
                   "roe"
                   "ropes"
                   "roy"
                   "salads"
                   "see"
                   "slam"
                   "slat"
                   "some"
                   "spot"
                   "steer"
                   "stew"
                   "tag"
                   "tame"
                   "tan"
                   "tank"
                   "tea"
                   "tee"
                   "tie"
                   "tigers"
                   "tire"
                   "to"
                   "toe"
                   "wager"
                   "wave"
                   "wider"
                   "win"
                   "wires"))

(defun row (placement) (first placement))

(defun column (placement) (second placement))

(defun direction (placement) (third placement))

(defun placement-length (placement) (fourth placement))

(defun intersect? (placement1 placement2)
 (and
  (not (eq (direction placement1) (direction placement2)))
  (if (eq (direction placement1) 'across)
      (and (>= (row placement1) (row placement2))
	   (<= (row placement1)
	       (+ (row placement2) (placement-length placement2) -1))
	   (>= (column placement2) (column placement1))
	   (<= (column placement2)
	       (+ (column placement1) (placement-length placement1) -1)))
      (and (>= (row placement2) (row placement1))
	   (<= (row placement2)
	       (+ (row placement1) (placement-length placement1) -1))
	   (>= (column placement1) (column placement2))
	   (<= (column placement1)
	       (+ (column placement2) (placement-length placement2) -1))))))

(defun consistent-placements?
    (placement1 placement2 placement1-word placement2-word)
 (or (not (intersect? placement1 placement2))
     (if (eq (direction placement1) 'across)
	 (char= (aref placement1-word
		      (- (column placement2) (column placement1)))
		(aref placement2-word
		      (- (row placement1) (row placement2))))
	 (char= (aref placement2-word
		      (- (column placement1) (column placement2)))
		(aref placement1-word
		      (- (row placement2) (row placement1)))))))

(defun word-of-length (n dictionary)
 (if (null dictionary)
     (fail)
     (if (= (length (first dictionary)) n)
	 (either (first dictionary) (word-of-length n (rest dictionary)))
	 (word-of-length n (rest dictionary)))))

(defun check-placement (placement word solution)
 (dolist (placement-word solution)
  (if (not (consistent-placements?
	    (first placement-word) placement (second placement-word) word))
      (fail))))

(defun choose-placement (placements solution)
 (block exit
  (dolist (placement placements)
   (if (some #'(lambda (placement-word)
		(intersect? (first placement-word) placement))
	     solution)
       (return-from exit placement)))
  (return-from exit (first placements))))

(defun crossword (placements dictionary &optional solution)
 (if (null placements)
     solution
     (let* ((placement (choose-placement placements solution))
	    (word (word-of-length (placement-length placement) dictionary)))
      (check-placement placement word solution)
      (crossword (remove placement placements)
		 dictionary
		 (cons (list placement word) solution)))))

(defun crossword-variables (placements dictionary)
 (iterate
  (with variables =
        (iterate
         (for placement in placements)
         (collect
          (a-member-ofv
           (all-values
            (let ((word (a-member-of dictionary)))
             (unless (= (length word)
                        (placement-length placement))
              (fail))
             word))))))
  (for (variable1 . remaining-variables) on variables)
  (for (placement1 . remaining-placements) on placements)
  (iterate
   (for variable2 in remaining-variables)
   (for placement2 in remaining-placements)
   (if (intersect? placement1 placement2)
       (let ((placement1 placement1)
             (placement2 placement2))
        (assert!
         (funcallv #'(lambda (word1 word2)
                      (consistent-placements?
		       placement1 placement2 word1 word2))
                   variable1
                   variable2)))))
  (finally (return variables))))

(defun crosswordv (placements dictionary)
 (mapcar #'list
         placements
         (solution (crossword-variables placements dictionary)
                   (reorder #'domain-size
			    #'(lambda (x) (declare (ignore x)) nil)
			    #'<
			    #'linear-force))))

(defun nonlinear ()
 (for-effects
  (print
   (solution
    (let ((x (a-real-betweenv -1e38 1e38))
	  (y (a-real-betweenv -1e38 1e38))
	  (z (a-real-betweenv -1e38 1e38)))
     (assert!
      (andv (orv (=v (+v (*v 4 x x y) (*v 7 y z z) (*v 6 x x z z)) 2)
		 (=v (+v (*v 3 x y) (*v 2 y y) (*v 5 x y z)) -4))
	    (>=v (*v (+v x y) (+v y z)) -5)))
     (list x y z))
    (reorder #'range-size
	     #'(lambda (x) (< x 1e-6))
	     #'>
	     #'divide-and-conquer-force)))))

(defun prolog-append (x y z)
 (either (progn (assert! (equalv x nil))
                (assert! (equalv y z)))
         (let ((x1 (make-variable))
               (y1 (make-variable))
               (z1 (make-variable))
               (a (make-variable)))
          (assert! (equalv x (cons a x1)))
          (assert! (equalv y y1))
          (assert! (equalv z (cons a z1)))
          (prolog-append x1 y1 z1))))

(defun split-list ()
 (all-values
  (let ((x (make-variable))
	(y (make-variable)))
   (prolog-append x y '(a b c d))
   ;; Note how lists with variables in their CDR print out as dotted pairs
   ;; since the Common Lisp printer for cons cells won't dereference bound
   ;; variables to determine that a cons cell can be printed in list notation.
   ;; Also note that the value returned by SPLIT-LIST contains variables which
   ;; are unbound outside the context of ALL-VALUES.
   (print (list x y)))))

;;; Tam V'Nishlam Shevah L'El Borei Olam
