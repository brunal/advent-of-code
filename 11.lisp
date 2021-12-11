(defun parse-input (input)
  (make-array
   (list (length input) (length (first input)))
   :initial-contents
   (mapcar (lambda (line) (mapcar #'digit-char-p (coerce line 'list))) input)))

(defparameter *input* (parse-input (uiop:read-file-lines "11.input")))

(defparameter *test-input* (parse-input (uiop:read-file-lines "11.input.test")))

(defun try-increase-energy (octopuses x y)
  "If within bounds and not at zero, increase the energy of the octopus and propagate it."
  (if (and (< -1 x (array-dimension octopuses 0))
	   (< -1 y (array-dimension octopuses 1))
	   (< 0 (aref octopuses x y)))
      (progn
	(incf (aref octopuses x y))
	(maybe-flash-one octopuses x y))
      0))

(defun maybe-flash-one (octopuses x y)
  "If > 9, flash the octopus, propagate it. Return the # of octopuses flashed."
  (if (<= (aref octopuses x y) 9)
      0
      (progn
	;; reset
	(setf (aref octopuses x y) 0)
	(1+
	 ;; propagate
	 (loop for dx in '(-1 0 1)
	       for xx = (+ x dx)
	       sum (loop for dy in '(-1 0 1)
			 for yy = (+ y dy)
			 sum (try-increase-energy octopuses xx yy)))))))

(defun flash-all (octopuses)
  ;; increment all
  (loop for x from 0 below (array-dimension octopuses 0)
	do (loop for y from 0 below (array-dimension octopuses 1)
		 do (incf (aref octopuses x y))))
  ;; flash all
  (loop for x from 0 below (array-dimension octopuses 0)
	sum (loop for y from 0 below (array-dimension octopuses 1)
		  sum (maybe-flash-one octopuses x y))))

(defun part1 (input)
  (loop for step from 0 below 100
	sum (flash-all input)))

;; careful: we mutate the inputs as we flash
;; (print (part1 *input**))

(defun all-octopuses-flash-p (input)
  (= (flash-all input)
     (apply #'* (array-dimensions input))))

(defun part2 (input)
  (if (all-octopuses-flash-p input)
      1
      (1+ (part2 input))))

(print (part2 *input*))
