(require :uiop)

(defparameter *input* (uiop:read-file-lines "09.input"))
(defparameter *input-test* (uiop:split-string "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45" :separator (string #\newline)))

(defun parse-input (input)
  (loop for line in input
	collect (mapcar #'parse-integer (uiop:split-string line :separator " "))))

(defparameter *data* (parse-input *input*))
(defparameter *data-test* (parse-input *input-test*))

(defun differentiate (list)
  (loop for (i j) on list by #'cdr
	if j
	  collect (- j i)))

(defun differentiate-rec (list)
  (cons list
	(loop initially (setf l list)
	      for l = (differentiate l)
	      collect l into diffs
	      if (every #'zerop l)
		return diffs)))

(defun find-next-value (list)
  (loop for seq in (differentiate-rec list)
	sum (first (last seq))))

(defun part1 (data)
  (loop for line in data sum (find-next-value line)))

(print (part1 *data*))

(defun part2 (data)
  (part1 (mapcar #'reverse data)))

(print (part2 *data*))
