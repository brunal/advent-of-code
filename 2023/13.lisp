(require :uiop)

(defparameter *input* (uiop:read-file-lines "13.input"))
(defparameter *input-test* (uiop:split-string "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#" :separator (string #\newline)))

(defun parse-block (lines)
  (loop for line in lines
	collect (loop for char in (coerce line 'list)
		      collect (if (char= char #\#) :rock :ash))))

(defun format-block (block)
  (loop for line in block
	do (print (coerce (loop for spot in line
				collect (if (eq spot :rock) #\# #\.))
			  'string))))

(defun parse-input (input)
  (loop with current-block = nil
	for line in input
	if (uiop:emptyp line)
	  collect (parse-block (reverse current-block)) into blocks
	  and do (setf current-block nil)
	else
	  do (push line current-block)
	finally
	   (return (append blocks (list (parse-block (reverse current-block)))))))

(defun list-to-int (list)
  (reduce (lambda (x y) (+ (* 2 x) (if (eq y :rock) 1 0))) list :initial-value 0))

(defun block-ints (block)
  "Returns ((int signature for rows) (int signature for cols))."
  (list
   (loop for row in block collect (list-to-int row))
   (loop for col-idx from 0 below (length (first block))
	 collect (list-to-int
		  (loop for row in block collect (nth col-idx row))))))

(defun list-prefix-equal (l1 l2)
  (reduce (lambda (x y) (and x y))
	  (mapcar #'eq l1 l2)))

(defun find-symmetry (list)
  (loop with past = nil
	for idx from 1
	for (cur . rest) on list
	do (setf past (cons cur past))
	if (and rest (list-prefix-equal past rest))
	  return idx))

(defun block-symmetry (block)
  (destructuring-bind (row-sig col-sig) (block-ints block)
    (let ((row-sym (find-symmetry row-sig)))
      (if row-sym
	  (list :row row-sym)
	  (list :col (find-symmetry col-sig))))))

(defun part1 (input)
  (loop for block in (parse-input input)
	for (sym-dim sym-idx) = (block-symmetry block)
	sum (* sym-idx (if (eq sym-dim :row) 100 1))))

;; part2: need to tweak each cell until we get a different end result??
;; maybe start from "reflection is *THERE*" (different from the default one)
;; and compute the difference? single power of 2 = OK. 

