(require :uiop)
(require :alexandria)

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

(defun roll-boulder (array i j di dj)
  (loop for ii = i then (+ di ii)
	for jj = j then (+ dj jj)
	unless (is-empty array (+ di ii) (+ dj jj))
	  return (move-boulder array i j ii jj)))

(defmacro roll-boulders (array-name direction)
  (let ((positive (> (apply #'+ direction) 0))
	(first-axis (if (zerop (first direction)) 1 0)))
    `(loop for ,(if (zerop first-axis) 'i 'j)
	   ,@(if positive
		 `(from (1- (array-dimension ,array-name ,first-axis)) downto 1)
		 `(from 1 below (array-dimension ,array-name ,first-axis)))
	   do (loop for ,(if (zerop first-axis) 'j 'i)
		    from 1 below (array-dimension ,array-name ,(- 1 first-axis))
		    if (eq (aref ,array-name i j) 'O)
		      do (roll-boulder ,array-name i j ,@direction)))))

(defun roll-boulders-north (array)
  (roll-boulders array (-1 0)))

(defun roll-boulders-south (array)
  (roll-boulders array (1 0)))

(defun roll-boulders-west (array)
  (roll-boulders array (0 -1)))

(defun roll-boulders-east (array)
  (roll-boulders array (0 1)))

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

(defun part2 (input n)
  (let ((array (parse-input input))
	(hash-table (make-hash-table :test #'equalp)))
    (loop for step from 0 below n
	  do (progn
	       (setf (gethash (alexandria:copy-array array) hash-table) step)
	       (cycle array)
	       (let ((already-seen (gethash array hash-table)))
		 (when already-seen
		   (let* ((new-step (1+ step))
			  (cycle-length (- new-step already-seen))
			  (remaining-steps (- n new-step))
			  (necessary-steps (mod remaining-steps cycle-length)))
		     (loop repeat necessary-steps do (cycle array))
		     (return (compute-load array)))))))))

(defparameter *n* 1000000000)

(assert (= 64 (part2 *input-test* *n*)))

(print (part2 *input* *n*))
