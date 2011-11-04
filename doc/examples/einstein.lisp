;;;; Example: "The Einstein's Riddle".
;;;;
;;;; There are five houses in a row, each of different color.
;;;;
;;;; Each has an owner of a different nationality.
;;;;
;;;; Each owner has a unique favorite drink, type of cigarette, and a pet.
;;;;
;;;;    1. The Brit lives in the red house
;;;;    2. The Swede keeps dogs as pets
;;;;    3. The Dane drinks tea
;;;;    4. The green house is on the left of the white house
;;;;    5. The green house's owner drinks coffee
;;;;    6. The person who smokes Pall Mall rears birds
;;;;    7. The owner of the yellow house smokes Dunhill
;;;;    8. The man living in the centre house drinks milk
;;;;    9. The Norwegian lives in the first house
;;;;   10. The person who smokes Marlboro lives next to the one who keeps cats
;;;;   11. The person who keeps horses lives next to the person who smokes Dunhill
;;;;   12. The person who smokes Winfield drinks beer
;;;;   13. The German smokes Rothmans
;;;;   14. The Norwegian lives next to the blue house
;;;;   15. The person who smokes Marlboro has a neigbor who drinks water
;;;;
;;;; Question: Who owns the fish?

(eval-when (:compile-toplevel :load-toplevel)
  (require :screamer))

(in-package :screamer-user)

;;;; Asserting things, failing when they don't hold.

(defun fact! (bool)
  (unless bool
    (fail)))

(defun not! (bool)
  (when bool
    (fail)))

(defun and! (fact1 fact2)
  (or (and fact1 fact2)
      (and (not fact1) (not fact2))
      (fail)))

;;;; A house.

(defstruct house
  color
  owner
  pet
  drink
  cigarette
  position)

;;;; Generators for house properties. Each element is unique, so if it has
;;;; been generated before we immediately backtrack -- that's what the OTHERS
;;;; argument is for.

(macrolet ((def (slot name &rest options)
             (assert (= 5 (length options)))
             `(defun ,name (others)
                (let ((this (either ,@options)))
                  (not! (member this others
                                :key #',(intern (format nil "HOUSE-~A" slot))
                                :test #'eq))
                  this))))
  (def color a-color :red :green :white :yellow :blue)
  (def owner an-owner :brit :swede :dane :norwegian :german)
  (def pet a-pet :dog :bird :cat :horse :fish)
  (def drink a-drink :tea :coffee :milk :beer :water)
  (def cigarette a-cigarette :pallmall :dunhill :marlboro :winfield :rothmans))

;;;; Generator for houses. Immediately checks facts about the house.
;;;; We could optimize here in two ways:
;;;;
;;;; 1. Order the asserts so that we assert as much as possible
;;;;    before entering a choice-point. Eg. #9 would be better done
;;;;    immediately after generating the owner.
;;;;
;;;; 2. Change the property generators to accept a required value.
;;;;    Then instead of
;;;;      (AN-OWNER OTHERS)
;;;;    we would have
;;;;      (AN-OWNER OTHERS (WHEN (ZEROP POSITION) :NORWEGIAN))
;;;;    etc.
;;;;
;;;; ...but even keeping the order of assertions the same as in the puzzle we
;;;; win big by asserting as many of them as possible before generating new
;;;; house properties.
;;;;
;;;;   (let ((owner (an-owner others))
;;;;         (color (a-color others))
;;;;         (pet (a-pet others))
;;;;         (drink (a-drink others))
;;;;         (cigarette (a-cigarette others)))
;;;;     ...all the assertions...
;;;;     (make-house ...))
;;;;
;;;; would be pretty catastrophic, since most assertions would then need to
;;;; backtrack over almost the entire search-space, as opposed to a tiny
;;;; subset of it.

(defun a-house (position &rest others)
  (let ((owner (an-owner others))
        (color (a-color others)))
    ;; 1.
    (and! (eq :brit owner) (eq :red color))
    (let ((pet (a-pet others)))
      ;; 2.
      (and! (eq :swede owner) (eq :dog pet))
      (let ((drink (a-drink others)))
        ;; 3.
        (and! (eq :dane owner) (eq :tea drink))
        ;; 5.
        (and! (eq :green color) (eq :coffee drink))
        (let ((cigarette (a-cigarette others)))
          ;; 6.
          (and! (eq :pallmall cigarette) (eq :bird pet))
          ;; 7.
          (and! (eq :dunhill cigarette) (eq :yellow color))
          ;; 8.
          (and! (= 2 position) (eq :milk drink))
          ;; 9.
          (and! (= 0 position) (eq :norwegian owner))
          ;; 12.
          (and! (eq :winfield cigarette) (eq :beer drink))
          ;; 13.
          (and! (eq :rothmans cigarette) (eq :german owner))
          ;; OK!
          (make-house :color color
                      :owner owner
                      :pet pet
                      :drink drink
                      :cigarette cigarette
                      :position position))))))

;;;; Street generator: pass already created houses to generators so that
;;;; we get unique properties.

(defun a-street ()
  (let* ((a (a-house 0))
         (b (a-house 1 a))
         (c (a-house 2 a b))
         (d (a-house 3 a b c))
         (e (a-house 4 a b c d)))
    (list a b c d e)))

;;;; Anaproric macro for picking a house based on a property. Captures HOUSES.

(defmacro house (key value)
  `(or (car (member ,value houses :key #',(intern (format nil "HOUSE-~A" key))
                    :test #'eq))
       (error "No ~S ~S!" ,key ,value)))

;;;; Solving the whole riddle: generate the street, then assert relationships
;;;; between houses. Here changing the order of asserts wouldn't really
;;;; help, since there are no choice-points after A-STREET.

(defun riddle ()
  (let ((houses (a-street)))
    (let ((left-of-white (1- (house-position (house :color :white))))
          (green (house :color :green)))
      ;; 4.
      (fact! (= left-of-white (house-position green))))
    (let ((marlboro (house-position (house :cigarette :marlboro)))
          (cat (house-position (house :pet :cat))))
      ;; 10.
      (fact! (= 1 (abs (- marlboro cat))))
      (let ((horses (house-position (house :pet :horse)))
            (dunhill (house-position (house :cigarette :dunhill))))
        ;; 11.
        (fact! (= 1 (abs (- horses dunhill)))))
      ;; 14.
      (let ((norwegian (house :owner :norwegian))
            (blue (house :color :blue)))
        (fact! (= 1 (abs (- (house-position norwegian) (house-position blue))))))
      ;; 15.
      (let* ((left (when (plusp marlboro)
                     (house-drink (elt houses (1- marlboro)))))
             (right (when (< marlboro (1- (length houses)))
                      (house-drink (elt houses (1+ marlboro))))))
        (fact! (or (eq :water left) (eq :water right)))))
    (house :pet :fish)))

#+nil
(time (one-value (riddle)))
