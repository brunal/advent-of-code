(require 'uiop)

(defun parse-lines (lines)
  (mapcar
   (lambda (line)
     (mapcar #'parse-integer (uiop:split-string line :separator "-,")))
   lines))

(defparameter *input* (parse-lines (uiop:read-file-lines "04.input")))

(defun left-contains-right (x1 x2 y1 y2)
  "Returns t if [x1, x2] fully contains [y1, y2]"
  (and (<= x1 y1) (>= x2 y2)))

(defun range-fully-contains (x1 x2 y1 y2)
  (or (left-contains-right x1 x2 y1 y2)
      (left-contains-right y1 y2 x1 x2)))

(defun count-pairs-that (input predicate)
  (loop for pair in input
	counting (apply predicate pair)))

(defun part1 (input)
  (count-pairs-that input #'range-fully-contains))

(defun range-overlaps (x1 x2 y1 y2)
  (or (and (<= x1 y1) (>= x2 y1))
      (and (<= x1 y2) (>= x2 y2))
      (and (>= x1 y1) (<= x2 y2))))

(defun part2 (input)
  (count-pairs-that input #'range-overlaps))

(print (part1 *input*))
(print (part2 *input*))
