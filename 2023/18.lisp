(require :uiop)

(defun parse-input (input)
  (loop for line in input
	for (direction length color) = (uiop:split-string line :separator " ")
	collect (list (intern direction)
		      (parse-integer length)
		      (subseq color 2 (- (length color) 1)))))

(defparameter *input* (parse-input (uiop:read-file-lines "18.input")))
(defparameter *input-test* (parse-input (uiop:split-string "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)" :separator (string #\newline))))

(defun dig (instructions)
  ;; Returns the list of corners.
  (loop with i = 0
	with j = 0
	for (dir count) in instructions
	do (case dir
	     (U (decf i count))
	     (D (incf i count))
	     (L (decf j count))
	     (R (incf j count)))
	collect (list i j)))

(defun count-total-excavation (instructions)
  ;; pick's theorem + shoelace formula, just like day 10.
  (let* ((digs (dig instructions))
	 (total-area (/ (loop for (d1 d2) on digs
			      if (null d2)
				do (setf d2 (first digs))
			      sum (- (* (second d1) (first d2))
				     (* (second d2) (first d1))))
			2))
	 (boundary-size (loop for (dir count) in instructions
			      sum count)))
    (+ boundary-size
       (1+ (- total-area  (/ boundary-size 2))))))

(defun part1 (input)
  (count-total-excavation input))

(assert (= 62 (part1 *input-test*)))
(print (part1 *input*))

(defparameter *colors* #(R D L U))
(defun color-to-instruction (color)
  ;; Returns (direction number)
  (list
   (aref *colors* (parse-integer (subseq color (1- (length color)))))
   (parse-integer (subseq color 0 (1- (length color)))
		  :radix 16)))

(defun part2 (input)
  (count-total-excavation (mapcar #'color-to-instruction (mapcar #'third input))))

(assert (= 952408144115 (part2 *input-test*)))
(print (part2 *input*))
