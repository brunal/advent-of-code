(require :uiop)

(defparameter *input* (uiop:read-file-lines "12.input"))
(defparameter *input-test* (uiop:split-string "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi" :separator (string #\newline)))

(defun char-to-height (char)
  (- (char-int char) (char-int #\a)))

(defun parse-input (input)
  (let ((array (make-array (list (length input) (length (first input)))))
	(start nil)
	(end nil))
    (loop for line in input
	  for i from 0 below (length input)
	  do (loop for char in (coerce line 'list)
		   for j from 0 below (length line)
		   do (progn
			(when (char= #\S char)
			  (setf start (cons i j)) (setf char #\a))
			(when (char= #\E char)
			  (setf end (cons i j)) (setf char #\z))
			(setf (aref array i j) (char-to-height char)))))
    (values array start end)))

(defun neighbours (map location)
  "Returns neighbours in map with at max +1 height"
  (destructuring-bind ((x . y) (xmax ymax)) (list location (array-dimensions map))
    (loop for dx in '(-1 0 1 0)
	  for dy in '(0 1 0 -1)
	  for xx = (+ x dx)
	  for yy = (+ y dy)
	  if (<= 0 xx (1- xmax))
	    if (<= 0 yy (1- ymax))
	      if (<= (aref map xx yy) (1+ (aref map x y)))
		collect (cons xx yy))))

;; simple queue definition
;; backed by a list. represented by 2 pointers: first cons cell, last cons cell.
(defun make-queue () (list nil nil))

(defun pushq (q elem)
  ;; push it & update the last cons cell.
  (setf (cdr q)
	(setf (cddr q) (cons elem nil)))
  (when (emptyq q)
    ;; fix the head pointer.
    (setf (car q) (cdr q)))
  q)

(defun popq (q)
  (prog1 (caar q)
    (setf (car q) (cdar q))))

(defun emptyq (q) (null (car q)))

(defun find-shortest-path (map start end &aux
					   (queue (make-queue))
					   (visited (make-hash-table :test #'equal)))
  (pushq queue (cons start 0))
  (setf (gethash start visited) t)
  (loop while (not (emptyq queue))
	for (current . score) = (popq queue)
	do (if (equal current end)
	       (return score)
	       (loop for n in (neighbours map current)
		     if (null (gethash n visited))
		       do (progn
			    (setf (gethash n visited) t)
			    (pushq queue (cons n (1+ score))))))))

(defun part1 (&optional (input *input*))
  (multiple-value-bind (map start end) (parse-input input)
    (find-shortest-path map start end)))

(defun starting-points-list (map)
  (loop for i from 0 below (array-dimension map 0)
	nconc (loop for j from 0 below (array-dimension map 1)
		    if (eql 0 (aref map i j))
		      collect (cons i j))))

(defun part2 (&optional (input *input*))
  (multiple-value-bind (map start end) (parse-input input)
    (declare (ignore start))
    (apply #'min
	   (remove nil
		   (loop for start in (starting-points-list map)
			 collect (find-shortest-path map start end))))))
