(defun parse (lines)
  (let ((array (make-array (list (length lines) (length (first lines)))
			   :initial-element nil)))
    (loop for line in lines
	  for i from 0
	  do (loop for char in (coerce line 'list)
		   for j from 0
		   do (setf (aref array i j)
			    (cond
			      ((char= char #\>) 'east)
			      ((char= char #\v) 'south)
			      ((char= char #\.) nil)
			      (t (error "invalid character"))))))
    array))

(defun draw (cucumbers)
  (loop for i from 0 below (array-dimension cucumbers 0)
	do (loop for j from 0 below (array-dimension cucumbers 1)
		 do (format t "~a" (ecase (aref cucumbers i j)
				     (east #\>)
				     (south #\v)
				     ((nil) #\.)))
		 finally (format t "~%"))))

(defun input-test () (parse (uiop:read-file-lines "25.input.test")))
(defun input () (parse (uiop:read-file-lines "25.input")))

(defun move-east (cucumbers)
  (let ((can-move-array (make-array (array-dimensions cucumbers) :initial-element nil)))
    (loop with imax = (array-dimension cucumbers 0)
	  for i from 0 below imax
	  do (loop with jmax = (array-dimension cucumbers 1)
		   for j from 0 below jmax
		   do (setf (aref can-move-array i j)
			    (and (eq (aref cucumbers i j) 'east)
				 (null (aref cucumbers i (mod (1+ j) jmax)))))))
    (loop with count-moved = 0		      
	  with imax = (array-dimension cucumbers 0)
	  for i from 0 below imax
	  do (loop with jmax = (array-dimension cucumbers 1)
		   for j from 0 below jmax
		   if (aref can-move-array i j)
		     do (progn
			  (incf count-moved)
			  (setf (aref cucumbers i j) nil)
			  (setf (aref cucumbers i (mod (1+ j) jmax)) 'east)))
	  finally (return count-moved))))

(defun move-south (cucumbers)
  (let ((can-move-array (make-array (array-dimensions cucumbers) :initial-element nil)))
    (loop with imax = (array-dimension cucumbers 0)
	  for i from 0 below imax
	  do (loop with jmax = (array-dimension cucumbers 1)
		   for j from 0 below jmax
		   do (setf (aref can-move-array i j)
			    (and (eq (aref cucumbers i j) 'south)
				 (null (aref cucumbers (mod (1+ i) imax) j))))))
    (loop with count-moved = 0		      
	  with imax = (array-dimension cucumbers 0)
	  for i from 0 below imax
	  do (loop with jmax = (array-dimension cucumbers 1)
		   for j from 0 below jmax
		   if (aref can-move-array i j)
		     do (progn
			  (incf count-moved)
			  (setf (aref cucumbers i j) nil)
			  (setf (aref cucumbers (mod (1+ i) imax) j) 'south)))
	  finally (return count-moved))))

(defun step-once (cucumbers)
  (+ (move-east cucumbers)
     (move-south cucumbers)))

(defun step-times (cucumbers total-times &optional (so-far 0))
  (if (= total-times so-far)
      (draw cucumbers)
      (progn
	(draw cucumbers)
	(format t "Step ~a, ~a move:~%" (incf so-far) (step-once cucumbers))
	(step-times cucumbers total-times so-far))))

(defun count-steps (cucumbers &optional (count 0))
  (if (> (step-once cucumbers) 0)
      (count-steps cucumbers (1+ count))
      (1+ count)))

(defun part1 () (print (count-steps (input))))
