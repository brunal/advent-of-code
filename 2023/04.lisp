(require :uiop)

(defparameter *input* (uiop:read-file-lines "04.input"))
(defparameter *input-test* (uiop:split-string
			    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
			    :separator (string #\newline)))

(defun split-numbers (string)
  (mapcar #'parse-integer
	  (remove-if (lambda (s) (zerop (length s)))
		     (uiop:split-string string
					:separator (string #\Space)))))

(defun parse-card (line)
  (let* ((card-and-number (uiop:split-string line :separator (string #\:)))
	 (numbers-str (uiop:split-string (second card-and-number) :separator (string #\|)))
	 (winning-numbers (split-numbers (first numbers-str)))
	 (numbers-i-got (split-numbers (second numbers-str))))
    (length (common-numbers winning-numbers numbers-i-got))))

(defun common-numbers (set1 set2)
 (intersection set1 set2 :test #'=))

(defun card-value-part1 (card-value)
    (if (zerop card-value)
	0
	(expt 2 (1- card-value))))

(defun part1 (input)
  (apply #'+ (mapcar #'card-value-part1 (mapcar #'parse-card input))))

(print (part1 *input*))

(defun sum-lists (l1 l2)
  (nconc
   (loop for i1 in l1
	 for i2 in l2
	 collect (+ i1 i2))
   (subseq (if (> (length l1) (length l2)) l1 l2)
	   (min (length l1) (length l2)))))

(defun play-cards (cards-to-pick card-values total-card-count)
  (if (null cards-to-pick)
      total-card-count
      (let* ((how-many-cards (first card-values))
	     (current-card-count (first cards-to-pick))
	     (new-cards (loop for i from 1 upto how-many-cards collect current-card-count)))
	(play-cards
	 (sum-lists (rest cards-to-pick) new-cards)
	 (rest card-values)
	 (+ total-card-count current-card-count)))))

(defun part2 (input)
  (play-cards (loop for card in input collect 1)
	      (mapcar #'parse-card input)
	      0))

(print (part2 *input*))
