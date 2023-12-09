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
	(loop for next-list = (differentiate list)
	      collect next-list into diffs
	      if (every #'zerop next-list)
		return diffs
	      do (setf list next-list))))

(defun find-next-value (list)
  (loop for seq in (differentiate-rec list)
	sum (first (last seq))))

(defun part1 (data)
  (loop for line in data sum (find-next-value line)))

(print (part1 *data*))

(defun find-previous-value (list)
  (loop initially (setf sign -1)
	for seq in (differentiate-rec list)
	for sign = (* -1 sign)
	sum (* sign (first seq))))

(defun part2 (data)
  (loop for line in data sum (find-previous-value line)))

(print (part2 *data*))
