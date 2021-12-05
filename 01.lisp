(defparameter *depths* (mapcar #'parse-integer (uiop:read-file-lines "01.input")))

(defun part1 (depths)
  (loop for (a b) on depths
	while b
	count (< a b)))

(print (part1 *depths*))

(defun part2 (depths)
  (part1 (loop for (a b c) on depths
	       while c
	       collect (+ a b c))))

(print (part2 *depths*))
