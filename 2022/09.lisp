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

(defun update-locations (command head tail)
  (case command
    (U (incf (car head)))
    (D (decf (car head)))
    (R (incf (cdr head)))
    (L (decf (cdr head))))
  ; FIXME: if head & tail not aligned, need to move diagonally.
  (when (> (car head) (1+ (car tail))) (incf (car tail)))
  (when (< (car head) (1- (car tail))) (decf (car tail)))
  (when (> (cdr head) (1+ (cdr tail))) (incf (cdr tail)))
  (when (< (cdr head) (1- (cdr tail))) (decf (cdr tail))))
    
(defun tail-locations-table (commands)
  (let ((tail-locations (make-hash-table :test #'equal))
	(head (cons 0 0))
	(tail (cons 0 0)))
    (loop for command in commands
	  do (progn
	       (update-locations command head tail)
	       ; (format t "after ~A, locations: ~A & ~A~%" command head tail)
	       (setf (gethash (copy-tree tail) tail-locations) 1)))
    tail-locations))

(defun part1 (&optional (input *input*))
  (hash-table-count (tail-locations-table (parse-commands input))))
