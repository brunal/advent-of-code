
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
  (loop with galaxy-id = 1
	for line in input
	collect (loop for char in (coerce line 'list)
		      if (char= char #\#)
			collect galaxy-id
			and
			  do (incf galaxy-id)
		      else
			collect nil)))

(defun expand (map)
  ;; first expand lines
  (let ((expanded (loop for line in map
			collect (copy-list line)
			if (every #'null line) 
			  collect (copy-list line))))
    ;; then columns
    (loop for i from (1- (length (first expanded))) downto 0
	  if (not (loop for row in expanded if (nth i row) return t))
	    do (loop for row in expanded
		     do (push nil (cdr (nthcdr i row)))))
    expanded))

(defun to-array (map-list)
  (make-array (list (length map-list) (length (first map-list)))
	      :initial-contents map-list))

(defun galaxies-location (map-array)
  (loop for i from 0 below (array-dimension map-array 0)
	nconc (loop for j from 0 below (array-dimension map-array 1)
		    if (aref map-array i j)
		      collect (list i j))))

(defun galaxies-distance (g1 g2)
  ;; this is just the Manhattan distance
  (loop for p1 in g1
	for p2 in g2
	sum (abs (- p1 p2))))

(defun part1 (input)
  (let ((galaxies (galaxies-location (to-array (expand (parse-input input))))))
    (loop for (gal1 . rest) on galaxies
	  sum (loop for gal2 in rest
		    sum (galaxies-distance gal1 gal2)))))

(print (part1 *input*))

;; for part2, we cannot actually expand like that
;; however, we can record the indices of lines & columns that need to be expanded
;; when computing distance, crossing each recorded line/column
;; increases the distance by the expansion amount.

(defun expand-part2 (map)
  "Returns '((line-empty-p) (col-empty-p))."
  (list
   (loop for i from 0
	 for line in map
	 collect (every #'null line) )
   (loop for j from 0 below (length (first map))
	 collect (not (loop for row in map if (nth j row) return t)))))

(defun galaxies-location-part2 (map-array gap-distance empty-lines-p empty-cols-p)
  (loop with actual-i = 0
	for i from 0 below (array-dimension map-array 0)
	for empty-line-p in empty-lines-p
	do (incf actual-i (if empty-line-p gap-distance 1))
	nconc (loop with actual-j = 0
		    for j from 0 below (array-dimension map-array 1)
		    for empty-col-p in empty-cols-p
		    do (incf actual-j (if empty-col-p gap-distance 1))
		    if (aref map-array i j)
		      collect (list actual-i actual-j))))

(defun part2 (input &optional (gap-distance 1000000))
  (let* ((map-list (parse-input input))
	 (empty-p (expand-part2 map-list))
	 (galaxies (apply #'galaxies-location-part2 (to-array map-list) gap-distance empty-p)))
    (loop for (gal1 . rest) on galaxies
	  sum (loop for gal2 in rest
		    sum (galaxies-distance gal1 gal2)))))

(print (part2 *input*))

(defun part1-as-part2-logic (input)
  (part2 input 2))

(print (part1-as-part2-logic *input*))
