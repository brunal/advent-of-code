(require :uiop)

(defparameter *input* (uiop:read-file-lines "10.input"))
(defparameter *input-test* (uiop:split-string ".....
.S-7.
.|.|.
.L-J.
....." :separator (string #\newline)))

(defparameter *tiles*
  '((\| . (N S))
    (- . (W E))
    (L . (N E))
    (J . (N W))
    (|7| . (S W))
    (F . (S E))
    (\. . ())))

;; (i j) is i-th line j-th column.
(defparameter *direction-offset*
  '((N . (-1 0))
    (W . (0 -1))
    (S . (1 0))
    (E . (0 1))))

(defun tile-neighbours (tile)
  ;; Returns a list of the (i j) of the neighbours of the tile.
  (let ((neighbours-directions (cdr (assoc tile *tiles*))))
    (mapcar (lambda (d) (cdr (assoc d *direction-offset*))) neighbours-directions)))
  
(defun fix-tile-content (array i j)
  ;; Determine what's on array[i][j] based on its neighbours.
  ;; check the 4 neighbours, so that they have array[i][j] in their own connections.
  (let* ((directions (loop for (direction di dj) in *direction-offset*
			   if (loop for (di2 dj2) in (tile-neighbours (aref array (+ i di) (+ j dj)))
				    if (and (zerop (+ di di2)) (zerop (+ dj dj2)))
				      return t)
			     collect direction))
	 ;; directions is a 2-list of 'N 'S 'W 'E.
	 ;; find it in tiles, and replace array[i][j] with it.
	 (actual-tile (loop for (tile . dirs) in *tiles*
			    if (or (equal dirs directions) (equal dirs (reverse directions)))
			      return tile)))
    (setf (aref array i j) actual-tile)
    array))
  
(defun extract-start (array)
  ;; 2d array with S -> replace S by its tile, returns (array start-i start-j)
  (loop for i from 0 below (array-dimension array 0)
	if (loop for j from 0 below (array-dimension array 1)
		 ;; do (format t "Array[~A][~A] = ~A~%" i j (aref array i j))
		 if (eq (aref array i j) 'S)
		   return (list (fix-tile-content array i j) i j))
	  return it))

(defun parse-input (input)
  ;; Turns the map into a 2D array of symbols.
  ;; Replaces S with the tile it actually is.
  ;; Returns (map start-i start-j).
  (loop with array = (make-array (list (length input) (length (first input))))
	for i from 0
	for line in input
	do (loop for j from 0
		 for col in (coerce line 'list)
		 do (setf (aref array i j) (intern (string col))))
	finally (return (extract-start array))))

(defun find-next-tile (map pos prev)
  ;; Returns the (i j) of following the tile that's not prev.
  (let* ((neighbours-delta (tile-neighbours (apply #'aref map pos)))
	 (neighbours (mapcar (lambda (delta) (mapcar #'+ delta pos)) neighbours-delta)))
    (loop for n in neighbours unless (equal prev n) return n)))

(defun find-loop-tiles (map start-i start-j)
  ;; Start at (i j), follow the loop until we come back to (i j),
  ;; returns all the (i j) that make the loop.
  (loop with start = (list start-i start-j)
	with pos = start
	with prev-pos = nil
	for neighbour = (find-next-tile map pos prev-pos)
	collect neighbour into loop-tiles
	if (equal neighbour start)
	  return loop-tiles
	do (progn
	     (setf prev-pos pos)
	     (setf pos neighbour))))

(defun part1 (input)
  (/ (length (apply #'find-loop-tiles (parse-input input))) 2))

(print (part1 *input*))

(defun part2 (input)
  (let* ((map-and-start-pos (parse-input input))
	 (loop-elements (apply #'find-loop-tiles map-and-start-pos))
	 ;; use the shoelace formula to get the total area of the
	 ;; loop: https://en.wikipedia.org/wiki/Shoelace_formula
	 (total-area (/ (loop for (t1 t2) on loop-elements
			      if (null t2)
				do (setf t2 (first loop-elements))
			      sum (- (* (second t1) (first t2))
				     (* (second t2) (first t1))))
			2)))
    ;; use pick's theorem to relate the # of integer points on the
    ;; boundary of the loop, the area of the loop and the # of
    ;; integer point inside the loop.
    (1+ (- total-area (/ (length loop-elements) 2)))))

(print (part2 *input*))
