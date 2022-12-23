(require :uiop)

(defparameter *elf* #\#)
(defparameter *empty* #\.)

(defun parse-input (input)
  (make-array (list (length input) (length (first input)))
	      :initial-contents (loop for row in input
				      collect (coerce row 'list))))

(defparameter *input* (parse-input (uiop:read-file-lines "23.input")))

(defparameter *input-test0* (parse-input (uiop:split-string ".....
..##.
..#..
.....
..##.
....." :separator (string #\newline))))

(defparameter *input-test* (parse-input (uiop:split-string "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.." :separator (string #\newline))))

(defun pad-input (input by-how-much)
  "Adds by-how-much of emptiness on each side of input."
  (destructuring-bind (rows cols) (array-dimensions input)
    (let ((new-array (make-array (list (+ (* 2 by-how-much) rows) (+ (* 2 by-how-much) cols))
				 :initial-element *empty*)))
      (loop for i below rows
	    do (loop for j below cols
		     if (eql *elf* (aref input i j))
		       do (setf (aref new-array (+ by-how-much i) (+ by-how-much j)) *elf*)))
      new-array)))

(defparameter *north* -1)
(defparameter *south* 1)
(defparameter *west* #c(0 -1))
(defparameter *east* #c(0 1))

(defparameter *move-directions*
  (list *north* *south* *west* *east*))

(defparameter *around*
  (remove 0 (loop for i from -1 upto 1 nconc (loop for j from -1 upto 1 collect (complex i j)))))

(defparameter *check-and-move-directions*
  (flet ((expand (dir) (if (zerop (realpart dir))
			   (list (+ *north* dir) dir (+ *south* dir))
			   (list (+ *west* dir) dir (+ *east* dir)))))
    (loop for d in *move-directions*
	  collect (cons d (expand d))))
  "A list of (direction . directions-to-check)")

(defun locations-are-empty-p (elves elf deltas)
  "Checks if deltas around elf are empty."
  (every (lambda (p)
	   (eql *empty* (aref elves
			      (realpart (+ elf p))
			      (imagpart (+ elf p)))))
	 deltas))

(defun where-do-elves-want-to-move (elves direction-and-places-to-check &aux desires)
  "Returns a list of (elf . desired new location)."
  (destructuring-bind (ROWS COLS) (array-dimensions elves)
    (loop for row below ROWS
	  do (loop for col below COLS
		   for elf = (complex row col)
		   if (and (eql *elf* (aref elves row col))
			   ;; maybe it doesn't want to move.
			   ;; then we don't need to record it as its existence prevents other
			   ;; elves from trying to come close.
			   (not (locations-are-empty-p elves elf *around*)))
		     ;; check where elf wants to move. try all directions.
		     do (loop for (direction . places-to-check) in direction-and-places-to-check
			      if (locations-are-empty-p elves elf places-to-check)
				;; record we want to move in direction.
				do (push (cons elf (+ elf direction)) desires)
				and do (return)))))
  desires)
  
(defun allowed-moves (desired-moves)
  "Returns an (elf . new-position) list from an (elf . desired-position) list."
  ;; build a hash-table of new-position to # of candidates, then return only
  ;; conses when # of candidates == 1.
  (let ((position-candidates-count (make-hash-table)))
    (loop for (elf . new-position) in desired-moves
	  do (incf (gethash new-position position-candidates-count 0)))
    (loop for elf-and-new-position in desired-moves
	  for new-position = (cdr elf-and-new-position)
	  if (= 1 (gethash new-position position-candidates-count))
	    collect elf-and-new-position)))

(defun execute-moves (elves allowed-moves)
  "Plays allowed-moves onto elves."
  (loop for (elf . new-position) in allowed-moves
	do (setf (aref elves (realpart elf) (imagpart elf)) *empty*)
	do (setf (aref elves (realpart new-position) (imagpart new-position)) *elf*)))
	
(defun play-round (elves direction-and-places-to-check)
  "Try to move all elves (a 2d array of chars) once."
  (execute-moves elves
		 (allowed-moves (where-do-elves-want-to-move elves
							     direction-and-places-to-check))))

(defun shift-list (list)
  (let ((new-list (cdr (copy-list list))))
    (rplacd (last new-list) (cons (first list) nil))
    new-list))

(defparameter *debug* nil)

(defun play-rounds (elves round-count)
  (loop with dirs = *check-and-move-directions*
	for i below round-count
	do (play-round elves dirs)
	do (setf dirs (shift-list dirs))
	if *debug* do (draw elves)))

(defun elf-extrema (elves)
  "Returns (xmin xmax ymin ymax) where there are elves."
  (destructuring-bind (ROWS COLS) (array-dimensions elves)
    (values
     (loop for row below ROWS
	   for has-elf = (loop for col below COLS thereis (eql *elf* (aref elves row col)))
	   if has-elf do (return row))
     (loop for row from (1- ROWS) downto 0
	   for has-elf = (loop for col below COLS thereis (eql *elf* (aref elves row col)))
	   if has-elf do (return row))
     (loop for col below COLS
	   for has-elf = (loop for row below ROWS thereis (eql *elf* (aref elves row col)))
	   if has-elf do (return col))
     (loop for col from (1- COLS) downto 0
	   for has-elf = (loop for row below ROWS thereis (eql *elf* (aref elves row col)))
	   if has-elf do (return col)))))

(defun count-empty-tiles-inside (elves)
  (multiple-value-bind (xmin xmax ymin ymax) (elf-extrema elves)
    (loop for row from xmin upto xmax
	  sum (loop for col from ymin upto ymax
		    count (eql *empty* (aref elves row col))))))

(defun draw (elves)
  (destructuring-bind (ROWS COLS) (array-dimensions elves)
    (loop for row below ROWS
	  do (write-line (coerce (loop for col below COLS
				       collect (aref elves row col))
				 'string)))))

(defun part1 (&optional (input *input*))
  (let ((expanded (pad-input input 1)))
    (play-rounds expanded 10)
    (values (count-empty-tiles-inside expanded)
	    expanded)))

(defun move-until-all-static (elves)
  (loop with dirs = *check-and-move-directions*
	for rounds from 1
	for desires = (handler-case (where-do-elves-want-to-move elves dirs)
			(error (c)
			  (progn
			    (format t "Need to grow: ~A~%" c)
			    (setf elves (pad-input elves 10))
			    (where-do-elves-want-to-move elves dirs))))
	if (null desires)
	  do (return rounds)
	do (play-round elves dirs)
	do (setf dirs (shift-list dirs))))

(defun part2 (&optional (input *input*))
  (move-until-all-static (pad-input input 1)))
