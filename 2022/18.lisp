(defun parse-input (input)
  (loop for line in input
	collect (mapcar #'parse-integer (uiop:split-string line :separator ","))))

(defparameter *input* (parse-input (uiop:read-file-lines "18.input")))
(defparameter *input-test* (parse-input (uiop:split-string "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5" :separator (string #\newline))))

(defun get-sides (array)
  "For each t in array, returns all the spots with nil around it."
  (loop for x from 1 below (array-dimension array 0)
	nconc (loop for y from 1 below (array-dimension array 1)
		    nconc (loop for z from 1 below (array-dimension array 2)
				if (aref array x y z)
				  nconc (loop for delta in '((1 0 0) (-1 0 0)
							     (0 1 0) (0 -1 0)
							     (0 0 1) (0 0 -1))
					      for spot = (mapcar #'+ delta (list x y z))
					      if (not (apply #'aref array spot))
						collect spot)))))

(defun droplets-array (input)
  "Returns an array of the droplets from a list of droplet positions."
  (let* ((mins (reduce (lambda (a b) (mapcar #'min a b)) input))
	 (maxes (reduce (lambda (a b) (mapcar #'max a b)) input))
	 (array (make-array (loop for min in mins
				  for max in maxes
				  collect (+ 3 (- max min)))
			    :initial-element nil)))
    (loop for point in input
	  for point-offset = (mapcar (lambda (p min) (1+ (- p min))) point mins)
	  do (setf (apply #'aref array point-offset) t))
    array))
  
(defun part1 (&optional (input *input*))
  (length (get-sides (droplets-array input))))

(assert (= (part1 *input-test*) 64))

(defun get-connected-empty-spots (array spot &optional (visited (make-hash-table :test #'equal)))
  "Returns a hash table with all empty spots connected to `spot`."
  (setf (gethash spot visited) t)
  (loop for delta in '((1 0 0) (-1 0 0)
		       (0 1 0) (0 -1 0)
		       (0 0 1) (0 0 -1))
	for next = (mapcar #'+ spot delta)
	if (and (apply #'array-in-bounds-p array next)
		(not (apply #'aref array next))
		(not (gethash next visited)))
	  do (get-connected-empty-spots array next visited))
  visited)

;; take an outside empty spots. find all empty spots it can reach. Mark the other
;; empty spots as non-empty. Count sides.
(defun part2 (&optional (input *input*))
  (let* ((array (droplets-array input))
	 (outside-empty-spots (get-connected-empty-spots array '(0 0 0))))
    (loop for x below (array-dimension array 0)
	  do (loop for y below (array-dimension array 1)
		   do (loop for z below (array-dimension array 2)
			    if (not (or (aref array x y z)
					(gethash (list x y z) outside-empty-spots)))
			      do (setf (aref array x y z) t))))
    (length (get-sides array))))

(assert (= (part2 *input-test*) 58))
