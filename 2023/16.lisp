(require :uiop)

(defun parse-input (input)
  (make-array (list (length input) (length (first input)))
	      :initial-contents
	      (loop for line in input
		    collect (coerce line 'list))))

(defparameter *input* (parse-input (uiop:read-file-lines "16.input")))
(defparameter *input-test* (parse-input (uiop:read-file-lines "16.input.test")))

(defparameter *right* '(0 1))
(defparameter *left* '(0 -1))
(defparameter *up* '(-1 0))
(defparameter *down* '(1 0))

(defun get-next-directions (map location direction)
  (case (apply #'aref map location)
    (#\. (list direction))
    (#\/ (list (mapcar #'- (reverse direction))))
    (#\\ (list (reverse direction)))
    (#\- (if (zerop (second direction)) (list *left* *right*) (list direction)))
    (#\| (if (zerop (first direction)) (list *up* *down*) (list direction)))))

(defun get-next-beams (map location direction)
  (loop with new-directions = (get-next-directions map location direction)
	for dir in new-directions
	collect (list (mapcar #'+ dir location) dir)))

(defparameter *direction-to-visited-record-index*
  (list (cons *left* 0)
	(cons *down* 1)
	(cons *right* 2)
	(cons *up* 3)))

(defun direction-to-visited-record-index (direction)
  (cdr (assoc direction *direction-to-visited-record-index* :test #'equal)))

(defun set-visited (visited location direction)
  (setf
   (nth (direction-to-visited-record-index direction)
	(apply #'aref visited location))
   t))

(defun visited-p (visited location direction)
  (nth (direction-to-visited-record-index direction)
       (apply #'aref visited location)))

(defun in-bounds (map location)
  (and (< -1 (first location) (array-dimension map 0))
       (< -1 (second location) (array-dimension map 1))))

(defun visit (map visited location direction)
  (set-visited visited location direction)
  (loop for (new-location new-direction) in (get-next-beams map location direction)
	if (and (in-bounds map new-location)
		(not (visited-p visited new-location new-direction)))
	  do (visit map visited new-location new-direction)))

(defun get-energy-score (map &key start-location start-direction)
  (let ((visited (make-array (array-dimensions map)
			     :initial-contents
			     (loop for i from 0 below (array-dimension map 0)
				   collect (loop for j from 0 below (array-dimension map 1)
						 collect (list nil nil nil nil))))))
    ;; play beams & record them until no new beam.
    (visit map visited start-location start-direction)
    ;; finally compute the energy score.
    (loop for i from 0 below (array-dimension map 0)
	  sum (loop for j from 0 below (array-dimension map 1)
		    count (notevery #'null (aref visited i j))))))

(defun part1 (input)
  (get-energy-score input
		    :start-location '(0 0)
		    :start-direction *right*))

(assert (= 46 (part1 *input-test*)))

(print (part1 *input*))

(defun part2 (input)
  (let ((max-row (1- (array-dimension input 0)))
	(max-col (1- (array-dimension input 1))))
    (max
     ;; from the top
     (loop for col from 0 upto max-col
	   maximize (get-energy-score input :start-location (list 0 col) :start-direction *down*))
     ;; from the bottom
     (loop for col from 0 upto max-col
	   maximize (get-energy-score input :start-location (list max-row col) :start-direction *up*))
     ;; from the left
     (loop for row from 0 upto max-row
	   maximize (get-energy-score input :start-location (list row 0) :start-direction *right*))
     ;; from the left
     (loop for row from 0 upto max-row
	   maximize (get-energy-score input :start-location (list row max-col) :start-direction *left*)))))

(assert (= 51 (part2 *input-test*)))

(print (part2 *input*))
