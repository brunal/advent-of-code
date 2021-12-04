(ql:quickload :cl-utilities)

(defun parse-direction (direction)
  (let ((dir-and-value-str (cl-utilities:split-sequence #\Space direction)))
    (list (car dir-and-value-str) (parse-integer (cadr dir-and-value-str)))))

(defparameter +directions+ (mapcar #'parse-direction (uiop:read-file-lines "02.input")))

(defun part1 (directions)
  (loop
    for (direction value) in directions
    with depth = 0
    with horizontal = 0
    do
       (cond
	 ((string= direction "forward") (incf horizontal value))
	 ((string= direction "up") (incf depth (- value)))
	 ((string= direction "down") (incf depth value))
	 (t (error "should not happen")))
  finally (return (* depth horizontal))))

(print (part1 +directions+))

(defun part2 (directions)
  (loop
    for (direction value) in directions
    with depth = 0
    with horizontal = 0
    with aim = 0
    do
       (cond
	 ((string= direction "down") (incf aim value))
	 ((string= direction "up") (incf aim (- value)))
	 ((string= direction "forward") (progn
					  (incf horizontal value)
					  (incf depth (* aim value)))))
    finally (return (* depth horizontal))))

(print (part2 +directions+))
