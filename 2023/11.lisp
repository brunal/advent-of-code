(require :uiop)

(defparameter *input* (uiop:read-file-lines "11.input"))
(defparameter *input-test* (uiop:split-string "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....." :separator (string #\newline)))

(defun parse-input (input)
  (make-array (list (length input) (length (first input)))
	      :initial-contents
	      (loop for line in input
		    collect (loop for char in (coerce line 'list)
				  collect (char= char #\#)))))

(defun empty-lines-and-columns-p (map)
  "Returns '((line-empty-p) (col-empty-p))."
  (destructuring-bind (lines cols) (array-dimensions map)
    (list
     (loop for i from 0 below lines
	   collect (not (loop for j from 0 below cols
			      if (aref map i j)
				return t)))
     (loop for j from 0 below cols
	   collect (not (loop for i from 0 below lines
			      if (aref map i j)
				return t))))))

(defun galaxies-distance (g1 g2)
  ;; this is just the Manhattan distance
  (loop for p1 in g1
	for p2 in g2
	sum (abs (- p1 p2))))

(defun galaxies-location (map gap empty-lines-p empty-cols-p)
  (loop with actual-i = 0
	for i from 0 below (array-dimension map 0)
	for empty-line-p in empty-lines-p
	do (incf actual-i (if empty-line-p gap 1))
	nconc (loop with actual-j = 0
		    for j from 0 below (array-dimension map 1)
		    for empty-col-p in empty-cols-p
		    do (incf actual-j (if empty-col-p gap 1))
		    if (aref map i j)
		      collect (list actual-i actual-j))))

(defun sum-galaxies-distance (map &key gap)
  (let* ((empty-p (empty-lines-and-columns-p map))
	 (galaxies (apply #'galaxies-location map gap empty-p)))
    (loop for (gal1 . rest) on galaxies
	  sum (loop for gal2 in rest
		    sum (galaxies-distance gal1 gal2)))))

(defun part1 (input)
  (sum-galaxies-distance (parse-input input) :gap 2))

(print (part1 *input*))

(defun part2 (input)
  (sum-galaxies-distance (parse-input input) :gap 100000))

(print (part2 *input*))
