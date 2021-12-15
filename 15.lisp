(ql:quickload 'priority-queue)

(defun parse-input (input)
  (let ((array (make-array
		(list (length input) (length (first input))))))
    (loop for line in input and i from 0
	  do (loop for item in (coerce line 'list) and j from 0
		   do (setf (aref array i j) (digit-char-p item))))
    array))

(defparameter *input* (parse-input (uiop:read-file-lines "15.input")))
(defparameter *input-test* (parse-input (uiop:read-file-lines "15.input.test")))

(defun update-neighbours (table best-so-far unvisited me)
  "Update (i j)'s neighbours weigths in best-so-far and unvisited."
  (loop with (i . j) = me
	for (di dj) in '((-1 0) (1 0) (0 -1) (0 1))
	for ii = (+ i di)
	for jj = (+ j dj)
	if (and (< -1 ii (array-dimension best-so-far 0))
		(< -1 jj (array-dimension best-so-far 1)))
	  do (let ((suggested-value
		     (+ (aref best-so-far i j) (aref table ii jj))))
	       ;; update the neighbour's weight
	       (when (< suggested-value (aref best-so-far ii jj))
		 (setf (aref best-so-far ii jj) suggested-value)
		 (priority-queue:pqueue-push
		  (cons ii jj)
		  suggested-value
		  unvisited)))))

(defun shortest-path (table best-so-far unvisited current end)
  "Recursively visits from current until end."
  (update-neighbours table best-so-far unvisited current)
  (unless (equal current end)
    (shortest-path table best-so-far unvisited
		   (priority-queue:pqueue-pop unvisited) end)))

(defun shortest-path-from-start-to-end (table)
  (let* ((best-so-far
	   (make-array (array-dimensions table)
		       :initial-element most-positive-fixnum))
	 (max-i (1- (array-dimension table 0)))
	 (max-j (1- (array-dimension table 1)))
	 (unvisited (priority-queue:make-pqueue #'<)))
    (setf (aref best-so-far 0 0) 0)
    (loop for i from 0 upto max-i
	  do (loop for j from 0 upto max-j
		   do (priority-queue:pqueue-push
		       (cons i j)
		       (aref best-so-far i j)
		       unvisited)))
    (shortest-path table best-so-far unvisited '(0 . 0) `(,max-i . ,max-j))
    (aref best-so-far max-i max-j)))

(defun part1 (&optional (input *input*))
  (shortest-path-from-start-to-end input))
(assert (= (part1 *input-test*) 40))

(print (part1))

(defun replicate-map (table times)
  (let* ((max-i (array-dimension table 0))
	 (max-j (array-dimension table 1))
	 (new-table (make-array
		     (mapcar (lambda (n) (* times n))
			     (array-dimensions table))))
	 (loop-after-9 (lambda (n) (1+ (mod (1- n) 9)))))
    (loop for i from 0 below max-i do
      (loop for j from 0 below max-j do
	(dotimes (horizontal times)
	  (dotimes (vertical times)
	    (setf
	     (aref new-table
		   (+ i (* max-i horizontal))
		   (+ j (* max-j vertical)))
	     (funcall loop-after-9
		      (+ horizontal vertical (aref table i j))))))))
    new-table))

(defun part2 (&optional (input *input*))
  (part1 (replicate-map input 5)))
(assert (= (part2 *input-test*) 315))

(print (part2))
