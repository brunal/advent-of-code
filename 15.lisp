
(defun parse-input (input)
  (let ((array (make-array
		(list (length input) (length (first input)))
		:initial-element nil)))
    (loop for line in input and i from 0
	  do (loop for item in (coerce line 'list) and j from 0
		   do (setf (aref array i j) (digit-char-p item))))
    array))

(defparameter *input* (parse-input (uiop:read-file-lines "15.input")))
(defparameter *input-test* (parse-input (uiop:read-file-lines "15.input.test")))


;; Let's do a Dijkstra where each cell is a vertex, with up to 4 edges. Those 4
;; edges have the same weight: the "risk level" of the cell.
(defun visit (table best-so-far unvisited i j)
  (loop for (di dj) in '((-1 0) (1 0) (0 -1) (0 1))
	for ii = (+ i di)
	for jj = (+ j dj)
	;; update the neighbours' weights
	if (member (cons ii jj) unvisited)
	  do (setf (aref best-so-far ii jj)
		   (min
		    (aref best-so-far ii jj)
		    (+ (aref best-so-far i j) (aref table ii jj)))))
  ;; mark the current node visited
  (delete (cons i j) unvisited))

(defun next-to-visit (best-so-far-table unvisited)
  "Returns the (i . j) in unvisited that minimizes the distance."
  (loop with lowest-value = most-positive-fixnum
	with best-so-far = nil
	for unv in unvisited for (unv-x . unv-y) = unv
	for value = (aref best-so-far-table unv-x unv-y)
	if (< value lowest-value)
	  do (setf best-so-far unv)
	finally (return best-so-far)))

(defun shortest-path (table best-so-far unvisited current end)
  (let ((new-unvisited (visit table best-so-far unvisited (car current) (cdr current))))
    (unless (equal current end)
      (shortest-path table best-so-far new-unvisited (next-to-visit best-so-far new-unvisited) end))))


(defun shortest-path-from-start-to-end (table)
  (let* ((best-so-far
	   (make-array (array-dimensions table) :initial-element most-positive-fixnum))
	 (max-i (1- (array-dimension table 0)))
	 (max-j (1- (array-dimension table 1)))
	 (unvisited
	   (loop for i from 0 upto max-i
		 nconc (loop for j from 0 upto max-j
			     collect (cons i j)))))
    (setf (aref best-so-far 0 0) 0)
    (shortest-path table best-so-far unvisited '(0 . 0) (cons max-i max-j))
    (aref best-so-far max-i max-j)))
