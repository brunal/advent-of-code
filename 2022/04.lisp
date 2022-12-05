(ql:quickload "cl-utilities")

(defun parse-lines (lines)
  (mapcar
   (lambda (line)
     (mapcar (lambda (pair)
	       (mapcar (lambda (number) (parse-integer number))
		       (cl-utilities:split-sequence #\- pair)))
	     (cl-utilities:split-sequence #\, line)))
   lines))

(defparameter *input* (parse-lines (uiop:read-file-lines "04.input")))

(defun left-contains-right (x1 x2 y1 y2)
  "Returns t if [x1, x2] fully contains [y1, y2]"
  (and (<= x1 y1) (>= x2 y2)))

(defun range-fully-contains (x y)
  (let ((x1 (car x))
	(x2 (cadr x))
	(y1 (car y))
	(y2 (cadr y)))
    (or (left-contains-right x1 x2 y1 y2)
	(left-contains-right y1 y2 x1 x2))))

(defun count-pairs-that (input predicate)
  (reduce #'+
	  (loop for pair in input
		when (apply predicate pair)
		  collect 1)))

(defun part1 (input)
  (count-pairs-that input #'range-fully-contains))

(defun range-overlaps (x y)
  (let ((x1 (car x))
	(x2 (cadr x))
	(y1 (car y))
	(y2 (cadr y)))
    (or (and (<= x1 y1) (>= x2 y1))
	(and (<= x1 y2) (>= x2 y2))
	(and (>= x1 y1) (<= x2 y2)))))

(defun part2 (input)
  (count-pairs-that input #'range-overlaps))

(print (part1 *input*))
(print (part2 *input*))
