(defun make-image-array (image-list)
  (let ((array
	  (make-array (list (length image-list) (length (first image-list)))
		      :initial-element 0)))
    (loop for line in image-list
	  for i upfrom 0
	  do (loop for pixel in line
		   for j upfrom 0
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

(defun decode-pixel (iea-char-array get-pixel i j)
  "Decodes the pixel at (i, j) by looking at (i-1, j-1)...(i+1, j+1)."
  (aref iea-char-array
	(bin-list-to-number
	 (loop for ii from (1- i) upto (1+ i)
	       nconc (loop for jj from (1- j) upto (1+ j)
			   collect (funcall get-pixel ii jj))))))

(defun get-pixel (input-image i j default)
  "Returns the pixel at (i j) in input-image, or default if outside."
  (if (and (< -1 i (array-dimension input-image 0))
	   (< -1 j (array-dimension input-image 1)))
      (aref input-image i j)
      default))

(defun decode-image (iea-char-array input-image default)
  "Returns (list decoded new-default)."
  ;; we add a 1-pixel border to input-image size.
  (let ((get-pixel (lambda (i j) (get-pixel input-image i j default)))
	(output-image
	  (make-array (mapcar (lambda (d) (+ 2 d))
			      (array-dimensions input-image))
		      :initial-element 0)))
    (loop for i from 0 below (array-dimension output-image 0)
	  do (loop for j from 0 below (array-dimension output-image 1)
		   do (setf (aref output-image i j)
			    (decode-pixel iea-char-array
					  get-pixel
					  (1- i)
					  (1- j)))))
    (list output-image
	  ;; recompute the default value by checking the new value of something
	  ;; fully outside.
	  (decode-pixel iea-char-array get-pixel -2 -2))))

(defun count-lit-pixels (image)
  (loop for i from 0 below (array-dimension image 0)
	sum (loop for j from 0 below (array-dimension image 1)
		  count (= 1 (aref image i j)))))

(defun draw-image (image)
  (loop for i from 0 below (array-dimension image 0)
	do (loop for j from 0 below (array-dimension image 1)
		 do (format t "~a " (if (zerop (aref image i j)) #\. #\#))
		 finally (terpri))))

(defun decode-times (iea-char-array count image default)
  (if (zerop count)
      image
      (apply #'decode-times
	     iea-char-array
	     (1- count)
	     (decode-image iea-char-array
			   image
			   default))))

(defun part1 (&optional (input *input*))
  (count-lit-pixels
   (decode-times (car input) 2 (cdr input) 0)))

(defun part2 (&optional (input *input*))
  (count-lit-pixels
   (decode-times (car input) 50 (cdr input) 0)))
