(ql:quickload 'priority-queue)

(defun parse-input (input)
  (let ((table (make-hash-table)))
    (loop for line in input and i from 0 do
      (loop for item in (coerce line 'list) and j from 0
	    do (setf (gethash (complex i j) table) (digit-char-p item))))
    (list
     (length input)
     (length (first input))
     table)))

(defparameter *input* (parse-input (uiop:read-file-lines "15.input")))
(defparameter *input-test* (parse-input (uiop:read-file-lines "15.input.test")))

(defun update-neighbours (table best-so-far unvisited me)
  "Update neighbours weigths in best-so-far and unvisited."
  (loop for delta in '(#C(0 1) #C(0 -1) #C(1 0) #C(-1 0))
	for neighbour = (+ me delta)
	if (gethash neighbour table) ; it's inside the boundaries
	  do (let ((suggested-value (+ (gethash me best-so-far)
				       (gethash neighbour table))))
	       (when (< suggested-value
			(gethash neighbour best-so-far most-positive-fixnum))
		 (setf (gethash neighbour best-so-far) suggested-value)
		 (priority-queue:pqueue-push
		  neighbour
		  suggested-value
		  unvisited)))))

(defun shortest-path (table best-so-far unvisited current goal)
  "Recursively visits from current until goal."
  (update-neighbours table best-so-far unvisited current)
  (unless (= current goal)
    (shortest-path table best-so-far unvisited
		   (priority-queue:pqueue-pop unvisited) goal)))

(defun shortest-path-from-start-to-goal (table start goal)
  (let* ((best-so-far (make-hash-table :size (hash-table-count table)))
	 (unvisited (priority-queue:make-pqueue #'<)))
    (setf (gethash start best-so-far) 0)
    (loop for cell being the hash-keys of table
	  do (priority-queue:pqueue-push
	      cell
	      (gethash cell best-so-far most-positive-fixnum)
	      unvisited))
    (shortest-path table best-so-far unvisited start goal)
    (gethash goal best-so-far)))

(defun part1 (&optional (input *input*))
  (destructuring-bind (max-i max-j risks-table) input
    (shortest-path-from-start-to-goal
     risks-table #C(0 0) (- (complex max-i max-j) #C(1 1)))))
(assert (= (part1 *input-test*) 40))

(print (part1))

(defun draw-map (max-i max-j table)
  (loop for i from 0 below max-i do
    (progn
      (loop for j from 0 below max-j do
	(format t "~a " (gethash (complex i j) table)))
      (format t "~%"))))

(defun replicate-map (times max-i max-j table)
  (let* ((new-table (make-hash-table))
	 (loop-after-9 (lambda (n) (1+ (mod (1- n) 9)))))
    (loop for cell being the hash-keys of table
	    using (hash-value value)
	  for i = (realpart cell)
	  for j = (imagpart cell)
	  do (dotimes (horizontal times)
	       (dotimes (vertical times)
		 (setf
		  (gethash (+ cell
			      (complex (* max-i horizontal) (* max-j vertical)))
			   new-table)
		  (funcall loop-after-9
			   (+ horizontal vertical value))))))
    (list (* times max-i)
	  (* times max-j)
	  new-table)))

(defun part2 (&optional (input *input*))
  (part1 (apply #'replicate-map 5 input)))
(assert (= (part2 *input-test*) 315))

(print (part2))
