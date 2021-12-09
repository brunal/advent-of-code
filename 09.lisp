
(defun parse-input (input)
  "Turns the lines into a 2D array of int."
  (make-array
   (list (length input) (length (first input)))
   :initial-contents
   (mapcar (lambda (line) (mapcar #'digit-char-p (coerce line 'list))) input)))

(defparameter *input* (parse-input (uiop:read-file-lines "09.input")))

(defparameter *test-input* (parse-input (uiop:read-file-lines "09.input.test")))

(defun neighbours-are-higher (input i j)
  (and
   ;; above
   (or (<= i 0) (< (aref input i j) (aref input (1- i) j)))
   ;; left
   (or (<= j 0) (< (aref input i j) (aref input i (1- j))))
   ;; below
   (or (>= i (1- (array-dimension input 0))) (< (aref input i j) (aref input (1+ i) j)))
   ;; right
   (or (>= j (1- (array-dimension input 1))) (< (aref input i j) (aref input i (1+ j))))))

(defun find-low-points (input)
  (loop for i from 0 below (array-dimension input 0)
	nconc (loop for j from 0 below (array-dimension input 1)
		    if (neighbours-are-higher input i j)
		      collect (cons i j))))

(defun part1 (input)
  (loop for low-point in (find-low-points input)
	sum (1+ (aref input (car low-point) (cdr low-point)))))

(print (part1 *input*))

(defun is-valid (heights i j)
  (let ((dims (array-dimensions heights)))
    (and
     (< -1 i (first dims))
     (< -1 j (second dims))
     (< (aref heights i j) 9))))

  
(defun add-basin-neighbour (heights basins low-point current-x current-y)
  (setf (aref basins current-x current-y) low-point)
  (loop for (delta-x delta-y) in '((-1 0) (1 0) (0 -1) (0 1))
	if (and (is-valid heights (+ current-x delta-x) (+ current-y delta-y))
		(not (aref basins (+ current-x delta-x) (+ current-y delta-y))))
	  do (add-basin-neighbour heights basins low-point
				  (+ current-x delta-x)
				  (+ current-y delta-y))))

(defun build-basins (input low-points)
  "Builds an array of the same size as input where each value is the coords of a low point."
  (let ((basins (make-array (array-dimensions input) :initial-element nil)))
    ;; for each low point: start from the low point, make all its neighbours point to it,
    ;; then their neighbours
    (loop for low-point in low-points
	  do (add-basin-neighbour input basins low-point (car low-point) (cdr low-point)))
    basins))

(defun basins-sizes (basins)
  (let ((sizes (make-hash-table :test #'equal)))
    (loop for x below (array-dimension basins 0) do
      (loop for y below (array-dimension basins 1) do
	(incf (gethash (aref basins x y) sizes 0))))
	sizes))

(defun largest-basins-sizes (basins-hash-table)
  (subseq 
   (sort
    (loop for key being the hash-keys of basins-hash-table
	    using (hash-value value)
	  if key
	  collect value)
    #'>)
   0 3))

(defun part2 (input)
  (apply #'* (largest-basins-sizes
	      (basins-sizes
	       (build-basins input (find-low-points input))))))

(print part2 *input*)
