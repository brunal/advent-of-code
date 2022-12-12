(require :uiop)
;; (require :queues)

(defparameter *input* (uiop:read-file-lines "12.input"))

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
	      if (>= 1 (aref map xx yy) (aref map x y))
		collect (cons xx yy))))

;; simple queue definition
;; backed by a list. represented by 2 pointers: first cons cell, last cons cell.
(defun make-queue (initial-element)
  (let ((queue (list initial-element)))
    (cons queue queue)))

(defun pushq (q elem)
  ;; push it
  (setf (cdr (cdr q)) (cons elem nil))
  ;; update the last cons cell (todo: merge with line above)
  (setf (cdr q) (cdr (cdr q)))
  (when (emptyq q)
    (setf (car q) (cdr q))))

(defun popq (q)
  (let ((start (car q)))
    (setf (car q) (cdr (car q)))
    (car start)))

(defun emptyq (q)
  (null (car q)))

(defun find-shortest-path (map start end &aux
					   (queue (make-queue (cons start 0))); (queues:make-queue :simple-queue))
					   (visited (make-hash-table)))
  (setf (gethash start visited) t)
  ;; (loop for (current . score) = (queues:qpop queue)
  (loop while (not (emptyq queue))
	for (current . score) = (popq queue)
	do (if (equal current end)
	       score
	       (loop for n in (neighbours map current)
		     if (null (gethash n visited))
		       do (progn
			    (setf (gethash n visited) t)
			    (pushq queue (cons n (1+ score))))))))
			    ;; (queues:qpush queue (cons n (1+ score))))))))

(defun part1 (&optional (input *input*))
  (multiple-value-bind (map start end) (parse-input input)
    (find-shortest-path map start end)))

;; (defun find-shortest-path (map start end current &optional
;; 						   (queue (queues:make-queue))
;; 						   (visited (make-hash-table)))
;;  (unless (gethash current map)
;;    (setf (gethash current map) t)
;;    (loop for neighbour in (neighbours map current)
;;	  (find-shortest-path (map start end neighbour visited)))))


