(ql:quickload 'cl-utilities)

(defun parse-folds (lines)
  (loop for line in lines
	for instruction = (third (cl-utilities:split-sequence #\Space line))
	for (axis value-str) = (cl-utilities:split-sequence #\= instruction)
	collect (cons (read-from-string axis) (parse-integer value-str))))

;; parse input: x1,y1\nx2,x2...\n\nfold along axis=value\n
;; parse output: (dots . folds)
;; dots: list of (x . y)
;; folds: list of ('x or 'y . value)
(defun parse-input (lines)
  (loop for (head . tail) on lines
	until (zerop (length head))
	for (x y) = (mapcar #'parse-integer (cl-utilities:split-sequence #\, head))
	collect (cons x y) into coords
	finally (return (cons coords (parse-folds tail)))))

(defparameter *input* (parse-input (uiop:read-file-lines "13.input"))) 
(defparameter *input-test* (parse-input (uiop:read-file-lines "13.input.test"))) 

(defun new-position (current fold)
  (if (< current fold)
      current
      (- (* 2 fold) current)))

(defun do-fold (fold coords)
  (let ((fold-side (car fold))
	(fold-position (cdr fold))
	(x (car coords))
	(y (cdr coords)))
    (ccase fold-side
      (x (cons (new-position x fold-position) y))
      (y (cons x (new-position y fold-position))))))

(defun apply-fold (fold coords-list)
  "Returns the new coords after applying the fold."
  (remove-duplicates
   (loop for x-y in coords-list
	 collect (do-fold fold x-y))
   :test #'equal))

(defun part1 (input)
  (length
   (apply-fold (first (cdr input))
	       (car input))))

(print (part1 *input*))

(defun apply-folds (folds coords-list)
  (if (null folds)
      coords-list
      (apply-folds (cdr folds) (apply-fold (car folds) coords-list))))

(defun flip-x (array)
  (let ((flipped (make-array (array-dimensions array) :initial-element nil)))
    (loop for x from 0 below (array-dimension array 0)
	  do (loop for y from 0 below (array-dimension array 1)
		   do (setf
		       (aref flipped x y)
		       (aref array (1- (- (array-dimension array 0) x)) y))))
    flipped))

(defun draw (coords-list)
  (let* ((max-x (apply #'max (mapcar #'car coords-list)))
	 (max-y (apply #'max (ys coords-list)))
	 (array (make-array (list (1+ max-x) (1+ max-y)) :initial-element #\.)))
    (loop for (x . y) in coords-list
	  do (setf (aref array x y) #\#))
    (format t "~a~%" (flip-x array))))

(defun part2 (input)
  (draw
   (apply-folds (cdr input) (car input))))

(part2 *input*)
