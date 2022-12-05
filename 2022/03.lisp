
(defun item-priority (item)
  (if (upper-case-p item)
      (+ (- (char-int item) (char-int #\A)) 27)
      (+ (- (char-int item) (char-int #\a)) 1)))

(defparameter *input* (uiop:read-file-lines "03.input"))

(defun value-of-item-in-common (line)
  (let ((left (subseq line 0 (/ (length line) 2)))
	(right (subseq line (/ (length line) 2))))
    (item-priority
     (first (intersection (coerce left 'list)
			  (coerce right 'list))))))

(defun part1 (input)
  (reduce #'+
	  (mapcar #'value-of-item-in-common input)))

(defun value-of-item-in-common3 (s1 s2 s3)
  (item-priority
   (first (intersection (coerce s1 'list)
			(intersection (coerce s2 'list)
				      (coerce s3 'list))))))

(defun part2 (input)
  (loop for (s1 s2 s3) on input by #'cdddr
	sum (value-of-item-in-common3 s1 s2 s3)))
