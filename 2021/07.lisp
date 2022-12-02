(ql:quickload 'cl-utilities)

(defparameter *input*
  (mapcar #'parse-integer
	  (cl-utilities:split-sequence
	   #\,
	   (car (uiop:read-file-lines "07.input")))))


(defun total-fuel-used-linear (positions target)
  "Returns how much fuel is needed to move all of `positions` to `target`."
  (loop for p in positions
	sum (abs (- p target))))

(defun minimize-fuel-used (positions fuel-cost-function)
  (loop for i from (apply #'min positions) upto (apply #'max positions)
	minimize (funcall fuel-cost-function positions i)))

(defun part1 (positions)
  (minimize-fuel-used positions #'total-fuel-used-linear))

(print (part1 *input*))

(defun fuel-cost2 (i j)
  (let ((delta (abs (- i j))))
    (/ (* delta (1+ delta)) 2)))

(defun total-fuel-used-quadratic (positions target)
  "Same, but with cost = delta * (delta + 1) / 2"
  (loop for p in positions
	sum (fuel-cost2 p target)))

(defun part2 (positions)
  (minimize-fuel-used positions #'total-fuel-used-quadratic))

(print (part2 *input*))
