(require :uiop)
(require :alexandria)

(defparameter *input* (uiop:read-file-lines "09.input"))
(defparameter *input-test*
  (uiop:split-string
   "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2" :separator (string #\newline)))

(defun n-times (n what)
  (loop for i from 0 below n
	collect what))

(defun parse-commands (lines)
  (loop for line in lines
	nconc (destructuring-bind
		  (direction quantity) (uiop:split-string line :separator " ")
		(n-times (parse-integer quantity) (intern direction)))))

(defun update-knot (head tail)
  (let* ((dx (- (car head) (car tail)))
	 (dy (- (cdr head) (cdr tail)))
	 (distance (sqrt (+ (expt dx 2) (expt dy 2)))))
    (when (>= distance 2)
       (setf (car tail) (+ (car tail) (signum dx)))
       (setf (cdr tail) (+ (cdr tail) (signum dy))))))

(defun update-locations (rope command)
  (case command
    (U (incf (car (first rope))))
    (D (decf (car (first rope))))
    (R (incf (cdr (first rope))))
    (L (decf (cdr (first rope)))))
  (loop for (knot next) on rope when next do (update-knot knot next)))
    
(defun tail-locations-table (rope commands)
  (let ((tail-locations (make-hash-table :test #'equal)))
    (loop for command in commands
	  do (progn
	       (update-locations rope command)
	       (setf (gethash (copy-tree (last rope)) tail-locations) 1)))
    tail-locations))

(defun make-rope (length)
  (loop for i from 0 below length collect (cons 0 0)))

(defun part1 (&optional (input *input*))
  (hash-table-count (tail-locations-table (make-rope 2) (parse-commands input))))

(defun part2 (&optional (input *input*))
  (hash-table-count (tail-locations-table (make-rope 10) (parse-commands input))))
