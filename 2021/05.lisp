(ql:quickload "cl-ppcre")

;; lines appear as x1,y1 -> x2,y2
;; parse them as ((x1 . y1) . (x2 . y2))
(defun parse-line (line)
  (multiple-value-bind (whole-match captures)
      (ppcre:scan-to-strings "^(\\d+),(\\d+) -> (\\d+),(\\d+)$" line)
    (declare (ignore whole-match))
    (unless captures (error "no match!"))
    (cons (cons (parse-integer (aref captures 0))
		(parse-integer (aref captures 1)))
	  (cons (parse-integer (aref captures 2))
		(parse-integer (aref captures 3))))))

(defun parse-input (lines) (mapcar #'parse-line lines))

(defparameter *input* (parse-input (uiop:read-file-lines "05.input")))

;; convenience accessors
;; to be clean I should define make-line-from-array.
(defun x1 (vent) (caar vent))
(defun x2 (vent) (cadr vent))
(defun y1 (vent) (cdar vent))
(defun y2 (vent) (cddr vent))

(defun discard-diagonal (lines)
  (remove-if
   (lambda (l)
     (and (not (eql (x1 l) (x2 l)))
	  (not (eql (y1 l) (y2 l)))))
   lines))

(defun max-values (lines)
  "Returns ((max x) . (max y)) encountered."
  (reduce
   (lambda (max-so-far line)
     (let ((max-x (car max-so-far))
	   (max-y (cdr max-so-far))
	   (line-max-x (max (x1 line) (x2 line)))
	   (line-max-y (max (y1 line) (y2 line))))
       (cons
	(max max-x line-max-x)
	(max max-y line-max-y))))
   lines
   :initial-value '(0 . 0)))

(defun all-vent-points (line)
  (let* ((x1 (x1 line))
	 (x2 (x2 line))
	 (y1 (y1 line))
	 (y2 (y2 line))
	 (vent-length (max (abs (- x2 x1))
			   (abs (- y2 y1))))
	 (dx (/ (- x2 x1) vent-length))
	 (dy (/ (- y2 y1) vent-length)))
    (loop
      for step from 0 upto vent-length
      collect (cons (+ x1 (* dx step))
		    (+ y1 (* dy step))))))

(defun add-vent (vents line)
  (loop for vent-point in (all-vent-points line)
	do (incf (aref vents (car vent-point) (cdr vent-point)) 1)))

(defun vents-array (lines)
  (let* ((max-values (max-values lines))
	 (vents (make-array (list (1+ (car max-values))
				  (1+ (cdr max-values)))
			    :initial-element 0)))
    (dolist (line lines)
      (add-vent vents line))
    vents))

;; part2 made me re-define all-vent-points to support diagonal lines.
;; in turn, part1 became part2 without the diagonal lines -> part1 is easy to
;; define in terms of part2.
(defun part2 (lines)
  (let ((vents-array (vents-array lines)))
    ; count cells > 1
    (loop for x from 0 below (array-dimension vents-array 0)
	  sum (loop for y from 0 below (array-dimension vents-array 1)
		    count (> (aref vents-array x y) 1)))))

(defun part1 (lines) (part2 (discard-diagonal lines)))

(print (part1 *input*))
(print (part2 *input*))
