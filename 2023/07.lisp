(require :uiop)

(defparameter *input* (uiop:read-file-lines "07.input"))
(defparameter *input-test* (uiop:split-string "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483" :separator (string #\newline)))

(defun parse-input (input)
  "Returns a list of (hand . value)."
  (loop for line in input
	for (hand value) = (uiop:split-string line :separator (string #\Space))
	collect (cons (parse-hand hand) (parse-integer value))))

(defun parse-hand (hand)
  "Returns a 5-list of card values (elements of *card-order*)."
  (mapcar (lambda (char) (or (digit-char-p char) (intern (string char))))
	  (coerce hand 'list)))

(defparameter *card-order* '(2 3 4 5 6 7 8 9 T J Q K A))

(defun count-most-frequent-card (hand)
  "Returns the frequency of the most frequent card, +.5 if there is a pair in the other cards."
  (let ((ht (make-hash-table)))
    (loop for card in hand do (incf (gethash card ht 0)))
    (let* ((frequencies (sort (loop for v being the hash-values of ht collect v) #'>))
	   (max-count (first frequencies))
	   (second-max-count (second frequencies)))
      ;; handle double pairs, as well as full house
      (cond
	((and second-max-count (= 2 second-max-count)) (+ max-count 0.5))
	(max-count max-count)
	;; empty hand happens in part2 when we got '(J J J J J).
	(t 0)))))

(defun hands<= (h1 h2 count-freq card-order)
  (let ((f1 (funcall count-freq h1))
	(f2 (funcall count-freq h2)))
    (cond
      ((< f1 f2) t)
      ((> f1 f2) nil)
      ((= f1 f2) (loop for c1 in h1
		       for c2 in h2
		       for p1 = (position c1 card-order)
		       for p2 = (position c2 card-order)
		       if (< p1 p2)
			 return t
		       if (> p1 p2)
			 return nil
		       finally
			  (return t))))))

(defun parse-and-sort-input (input count-freq card-order)
  (sort (parse-input input)
	(lambda (h1 h2) (hands<= h1 h2 count-freq card-order))
	:key #'first))

(defun hands-value (sorted-hands)
  (loop for (hand . value) in sorted-hands
	for i from 1
	sum (* i value)))

(defun part1 (input)
  (hands-value
   (parse-and-sort-input input
			 #'count-most-frequent-card
			 *card-order*)))

(print (part1 *input*))

(defparameter *card-order-part2* '(J 2 3 4 5 6 7 8 9 T Q K A))

(defun count-most-frequent-card-part2 (hand)
  ;; Fist remove J, then add them to the most frequent card.
  (loop for card in hand
	if (eq card 'J)
	  sum 1 into j-count
	else
	  collect card into hand-no-j
	finally (return (+ j-count (count-most-frequent-card hand-no-j)))))

(defun part2 (input)
  (hands-value
   (parse-and-sort-input input
			 #'count-most-frequent-card-part2
			 *card-order-part2*)))

(print (part2 *input*))
