;;;; Example: "The Zebra Puzzle"
;;;;
;;;; Classic riddle popularly attributed to Lewis Carrol. (Essentially the
;;;; same as "The Einstein's Riddle".)
;;;;
;;;; This solution, contributed by Stephan Frank, uses the constraint
;;;; propagation features of Screamer.
;;;;
;;;; Five men with different nationalities live in the first five houses of a
;;;; street. They practise five distinct professions, and each of them has a
;;;; favourite animal and a favourite drink, all of them different. The five
;;;; houses are painted in different colours.
;;;;
;;;;    1. The Englishman lives in a red house.
;;;;    2. The Spaniard owns a dog.
;;;;    3. The Japanese is a painter.
;;;;    4. The Italian drinks tea.
;;;;    5. The Norwegian lives in the first house on the left.
;;;;    6. The owner of the green house drinks coffee.
;;;;    7. The green house is on the right of the white one.
;;;;    8. The sculptor breeds snails.
;;;;    9. The diplomat lives in the yellow house.
;;;;   10. Milk is drunk in the middle house.
;;;;   11. The Norwegian's house is next to the blue one.
;;;;   12. The violinist drinks fruit juice.
;;;;   13. The fox is in a house next to that of the doctor.
;;;;   14. The horse is in a house next to that of the diplomat.
;;;;
;;;; Question: Who owns a Zebra, and who drinks water?

(eval-when (:compile-toplevel :load-toplevel)
  (require :screamer))

(in-package :screamer-user)

;;;; Utility definitions
;;;;
;;;; LET-INTEGERS-BETWEENV binds names in VAR-LIST to integer
;;;; variables constrained in the range [min, max].
;;;;
;;;; ALL-DIFFERENTV returns a variable constrained to be true if variables in
;;;; the list received all have different values.

(defmacro let-integers-betweenv (((min max) var-list) body)
  `(let ,(loop for i in var-list
               collect (list i `(an-integer-betweenv ,min ,max)))
     ,body))

(defun all-differentv (list)
  ;; Functionally the same as (apply #'/=v list), but faster.
  (labels ((all-different (x xs)
             (if (null xs)
                 t
                 (andv (notv (=v x (car xs)))
                       (all-different x (cdr xs))
                       (all-different (car xs) (cdr xs))))))
    (all-different (car list) (cdr list))))

;;;; Constraint model
;;;;
;;;; Houses are identified by integers from 1 to 5.
;;;;
;;;; We first bind logic variables taking values in that range in
;;;; LET-INTEGERS-BETWEEN for all properties. We group them and assert that
;;;; every value is unique in its group using ALL-DIFFERENTV.
;;;;
;;;; We create three mode logic variables to represent distances between
;;;; certain houses.
;;;;
;;;; We assert the constraints stated in the riddle.
;;;;
;;;; Finally, we ask Screamer to provide a solution that satisfies
;;;; all given constraints.

(defun zebra-problem ()
  (let-integers-betweenv ((1 5)
                          (English Spaniard Japanese Italian Norwegian
                           Red Green White Yellow Blue
                           Painter Sculptor Diplomat Violinist
                           Doctor Dog Snails Fox Horse Zebra
                           Tea Coffee Milk Juice Water))
    (let ((Nationality (list English Spaniard Japanese Italian Norwegian))
          (Color       (list Red Green White Yellow Blue))
          (Profession  (list Painter Sculptor Diplomat Violinist Doctor))
          (Pet         (list Zebra Dog Snails Fox Horse))
          (Drink       (list Water Tea Coffee Milk Juice)))
      ;; The values in each list are exclusive.
      (assert! (all-differentv Nationality))
      (assert! (all-differentv Color))
      (assert! (all-differentv Profession))
      (assert! (all-differentv Pet))
      (assert! (all-differentv Drink))
      ;; Setup auxiliary vars needed for neigbouring relations.
      (let ((distance-1 (an-integer-betweenv -1 1))
            (distance-2 (an-integer-betweenv -1 1))
            (distance-3 (an-integer-betweenv -1 1)))
        (assert! (/=v distance-1 0))
        (assert! (/=v distance-2 0))
        (assert! (/=v distance-3 0))
        ;; The actual constraints from the problem description.
        (assert! (=v English Red))
        (assert! (=v Spaniard Dog))
        (assert! (=v Japanese Painter))
        (assert! (=v Italian Tea))
        (assert! (=v Norwegian 1))
        (assert! (=v Green Coffee))
        (assert! (=v Green (+v White 1)))
        (assert! (=v Sculptor Snails))
        (assert! (=v Diplomat Yellow))
        (assert! (=v Milk 3))
        (assert! (=v distance-1 (-v Norwegian Blue)))
        (assert! (=v Violinist Juice))
        (assert! (=v distance-2 (-v Fox Doctor)))
        (assert! (=v distance-3 (-v Horse Diplomat)))
        ;; Force a solution.
        (let ((result
               (one-value
                ;; Feed all the primary values into the solution: we don't
                ;; want any uncertainty to remain regarding these. If we
                ;; merely fed in NATIONALITY, ZEBRA, and WATER, we might
                ;; return false results where some of the constraints were
                ;; not fully checked because eg. PROFESSION wasn't solved.
                ;;
                ;; We're using STATIC-ORDERING here for simplicity. REORDER
                ;; using DOMAIN-SIZE would be faster.
                (solution (list Nationality Pet Drink Color Profession)
                          (static-ordering #'linear-force)))))
          ;; Human-readable labels: results we have back are integers identifying
          ;; the houses. Map back to nationalities.
          (destructuring-bind (Nationality Pet Drink &rest rest) result
            (declare (ignore rest))
            (let ((names (vector "Brit" "Spaniard" "Japanese" "Italian" "Norwegian")))
              (format t "The ~A owns the zebra.~%"
                      (aref names (position (first Pet) Nationality)))
              (format t "The ~A drinks water.~%"
                      (aref names (position (first Drink) Nationality))))))))))

#+nil
(time (screamer-user::zebra-problem))
