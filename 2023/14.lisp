(require :uiop)

(defparameter *input* (uiop:read-file-lines "14.input"))
(defparameter *input-test* (uiop:split-string "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...." :separator (string #\newline)))

(defun parse-input (input)
  ;; we pad the map with cube-shaped boulders on all sides.
  (make-array
   (list (+ 2 (length input)) (+ 2 (length (first input))))
   :initial-contents (nconc
		      (list (loop repeat (+ 2 (length (first input))) collect '|#|))
		      (loop for line in input
			    collect (nconc
				     (list '|#|)
				     (loop for char in (coerce line 'list)
					   collect (intern (string char)))
				     (list '|#|)))
		      (list (loop repeat (+ 2 (length (first input))) collect '|#|)))))

(defun move-boulder (array start-i start-j end-i end-j)
  (when (or (/= start-i end-i) (/= start-j end-j))
    (setf (aref array start-i start-j) '|.|)
    (setf (aref array end-i end-j) 'O)))

(defun is-empty (array i j)
  (eq '|.| (aref array i j)))

(defun roll-boulder-north (array i j)
  (loop for row from i downto 1
	unless (is-empty array (1- row) j)
	  return (move-boulder array i j row j)))

(defun roll-boulder-south (array i j)
  (loop for row from i upto (1- (array-dimension array 0))
	unless (is-empty array (1+ row) j)
	  return (move-boulder array i j row j)))

(defun roll-boulder-west (array i j)
  (loop for col from j downto 1
	unless (is-empty array i (1- col))
	  return (move-boulder array i j i col)))

(defun roll-boulder-east (array i j)
  (loop for col from j upto (1- (array-dimension array 1))
	unless (is-empty array i (1+ col))
	  return (move-boulder array i j i col)))

(defun roll-boulders-north (array)
  (loop for i from 0 below (array-dimension array 0)
	do (loop for j from 0 below (array-dimension array 1)
		 if (eq (aref array i j) 'o)
		   do (roll-boulder-north array i j))))

(defun roll-boulders-south (array)
  (loop for i from (1- (array-dimension array 0)) above 0
	do (loop for j from 0 below (array-dimension array 1)
		 if (eq (aref array i j) 'o)
		   do (roll-boulder-south array i j))))

(defun roll-boulders-west (array)
  (loop for j from 0 below (array-dimension array 1)
	do (loop for i from 0 below (array-dimension array 1)
		 if (eq (aref array i j) 'O)
		   do (roll-boulder-west array i j))))

(defun roll-boulders-east (array)
  (loop for j from (1- (array-dimension array 1)) above 0
	do (loop for i from 0 below (array-dimension array 1)
		 if (eq (aref array i j) 'O)
		   do (roll-boulder-east array i j))))

(defun compute-load (array)
  (loop for i from 0 below (array-dimension array 0)
	sum (loop for j from 0 below (array-dimension array 1)
		  if (eq (aref array i j) 'O)
		    ;; make sure to -1 to remove the effect of the padding.
		    sum (- (array-dimension array 0) i 1))))

(defun part1 (input)
  (let ((array (parse-input input)))
    (roll-boulders-north array)
    (compute-load array)))
	
(assert (= 136 (part1 *input-test*)))

(print (part1 *input*))

(defun cycle (array)
  (roll-boulders-north array)
  (roll-boulders-west array)
  (roll-boulders-south array)
  (roll-boulders-east array))

(defun format-array (array)
  (loop for i from 0 below (array-dimension array 0)
	do (format t "~A~%" (loop for j from 0 below (array-dimension array 1)
				  collect (aref array i j)))))

(defun part2 (input n)
  (let ((array (parse-input input)))
    (loop repeat n do (cycle array))
    (compute-load array)))

(defparameter *n* 1000000000)
