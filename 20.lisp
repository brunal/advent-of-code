
(defun make-image-array (image-list)
  ;; make sure to include a 2-pixel band of dark pixels all around the image.
  (let* ((lines (length image-list))
	 (columns (length (first image-list)))
	 (array (make-array (list (+ 4 lines) (+ 4 columns))
			    :initial-element 0)))
    (loop for line in image-list
	  for i upfrom 2
	  do (loop for pixel in line
		   for j upfrom 2
		   do (setf (aref array i j) pixel)))
    array))

(defun pixel-to-int (pixel)
  (ecase pixel
    (#\. 0)
    (#\# 1)))

(defun list-to-array (list)
  (make-array (length list) :initial-contents list))

(defun parse (lines)
  (assert (zerop (length (second lines))))
  (cons 
   (list-to-array (mapcar #'pixel-to-int (coerce (first lines) 'list)))
   (make-image-array
    (mapcar (lambda (line) (mapcar #'pixel-to-int (coerce line 'list)))
	    (rest (rest lines))))))

(defparameter *input-test* (parse (uiop:read-file-lines "20.input.test")))
(defparameter *input* (parse (uiop:read-file-lines "20.input")))

(defun bin-list-to-number (bin &optional (acc 0))
  (if (null bin)
      acc
      (bin-list-to-number (rest bin) (+ (* 2 acc) (first bin)))))

(defun decode-pixel (iea-char-array input-image i j)
  (aref iea-char-array
	(bin-list-to-number
	 (loop for ii from (1- i) upto (1+ i)
	       nconc (loop for jj from (1- j) upto (1+ j)
			   collect (aref input-image ii jj))))))

(defun decode-image (iea-char-array input-image)
  (let ((output-image
	  (make-array (mapcar (lambda (d) (+ 2 d)) (array-dimensions input-image))
		      :initial-element 0)))
    (loop for i from 1 below (1- (array-dimension input-image 0))
	  do (loop for j from 1 below (1- (array-dimension input-image 1))
		   do (setf (aref output-image (1+ i) (1+ j))
			    (decode-pixel iea-char-array input-image i j))))
    output-image))

(defun count-lit-pixels (image)
  (loop for i from 0 below (array-dimension image 0)
	sum (loop for j from 0 below (array-dimension image 1)
		  count (= 1 (aref image i j)))))

  
(defun draw-image (image)
  (loop for i from 0 below (array-dimension image 0)
	do (loop for j from 0 below (array-dimension image 1)
		 do (format t "~a " (if (zerop (aref image i j)) #\. #\#))
		    finally (terpri))))

(defun part1 (&optional (input *input*))
  (count-lit-pixels
   (decode-image (car input) 
		 (decode-image (car input) (cdr input)))))
