(require :uiop)
(require :cl-ppcre)

(defparameter *input* (uiop:read-file-lines "03.input"))
(defparameter *input-test* (uiop:split-string
			    "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
			    :separator (string #\newline)))


;; pad the board with .
;; then navigate 3 lines by 3 lines

(defun parse-bit (bit)
  (cond
    ((listp bit) bit)  ; for part 2
    ((eql bit #\.) nil)
    ((digit-char-p bit) (digit-char-p bit))
    (t bit)))

(defun input-to-padded-array (input)
  (let ((array (make-array (list (+ 2 (length input)) (+ 2 (length (first input))))
			   :initial-element nil)))
    (loop for line in input
	  for i from 0
	  do (loop for bit in (coerce line 'list)
		   for j from 0
		   do (setf (aref array (1+ i) (1+ j)) (parse-bit bit))))
    array))

(defun bits-close-to-symbols (array)
  ;; Returns an array with t around input symbols else nil.
  (let* ((X (array-dimension array 0))
	 (Y (array-dimension array 1))
	 (close-to-symbol-array (make-array (list X Y) :initial-element nil)))
    (loop for x from 1 below X
	  do (loop for y from 1 below Y
		   if (characterp (aref array x y))
		     do (progn
			  (setf (aref close-to-symbol-array (1- x) (1- y)) t)
			  (setf (aref close-to-symbol-array (1- x) y) t)
			  (setf (aref close-to-symbol-array (1- x) (1+ y)) t)
			  (setf (aref close-to-symbol-array x (1- y)) t)
			  (setf (aref close-to-symbol-array x (1+ y)) t)
			  (setf (aref close-to-symbol-array (1+ x) (1- y)) t)
			  (setf (aref close-to-symbol-array (1+ x) y) t)
			  (setf (aref close-to-symbol-array (1+ x) (1+ y)) t))))
    close-to-symbol-array))

(defun numbers-close-to-symbols (input)
  (let* ((padded-array (input-to-padded-array input))
	 (close-to-symbols (bits-close-to-symbols padded-array))
	 (X (array-dimension padded-array 0))
	 (Y (array-dimension padded-array 1)))
    ;; find digits. Keep consecutive digits if any of them has t in
    ;; close-to-symbols.
    (loop for x from 0 below X
	  nconc (loop with previously-on-digit-p = nil
		      with previously-symbol-nearby-p = nil
		      with number-consumed = 0
		      for y from 0 below Y
		      for on-digit-p = (integerp (aref padded-array x y))
		      for symbol-nearby-p = (aref close-to-symbols x y)
		      ;; did we consume a number we must yield?
		      if (and previously-on-digit-p
			      (not on-digit-p)
			      previously-symbol-nearby-p)
			collect number-consumed
		      ;; do we need to reset?
		      if (not on-digit-p)
			do (progn
			     (setf previously-on-digit-p nil)
			     (setf previously-symbol-nearby-p nil)
			     (setf number-consumed 0))
			   ;; are we consuming a number?
		      if on-digit-p
			do (progn
			     (setf previously-on-digit-p t)
			     (setf previously-symbol-nearby-p
				   (or previously-symbol-nearby-p symbol-nearby-p))
			     (setf number-consumed
				   (+ (* 10 number-consumed) (aref padded-array x y))))))))

(defun part1 (input)
  (apply #'+ (numbers-close-to-symbols input)))

;; part2:
;; * only keep \#* for symbols
;; * need to make sure it hits 2 numbers...

(defun turn-digits-into-number-list (line)
  ;; given a line of chars, turn consecutive digits into one list with the number
  ;; (\#4 \#5 \#.) -> ((45) (45) #\.) (same list instance)
  ;; note: it needs a non-digit as the last element.
  (loop with digit-count = 0
	with number-accumulated = 0
	for bit in line
	for digit-bit = (digit-char-p bit)
	if digit-bit
	  ;; accumulate
	  do (progn
	       (incf digit-count)
	       (setf number-accumulated (+ (* 10 number-accumulated) digit-bit)))
	if (and (not digit-bit) (not (zerop digit-count)))
	  ;; release
	  append (loop with number-as-list = (list number-accumulated)
		       for i from 1 upto digit-count
		       collect number-as-list)
	    into result
	if (not digit-bit)
	  ;; reset
	  do (progn
	       (setf digit-count 0)
	       (setf number-accumulated 0))
	if (not digit-bit)
	  collect bit into result
	finally
	   ;; release
	   (progn
	     (when (not (zerop digit-count))
	       (nconc result (loop with number-as-list = (list number-accumulated)
				   for i from 1 upto digit-count
				   collect number-as-list)))
	     (return result))))

(defun parse-numbers-in-input (input)
  (mapcar (lambda (line) (turn-digits-into-number-list (coerce line 'list)))
	  input))

(defun parse-bit2 (bit)
  (cond
    ((eql bit #\*) #\*)
    ((digit-char-p bit) (digit-char-p bit))
    (t nil)))

(defun maybe-gears-value (array i j)
  ;; Tries to find 2 unique number lists around array[i,j].
  (let* ((all-numbers
	   (loop for di in '(-1 0 1)
		 nconc (loop for dj in '(-1 0 1)
			     for cell = (aref array (+ i di) (+ j dj))
			     if (and cell (listp cell))
			       collect cell)))
	 (unique-numbers
	   (remove-duplicates all-numbers :test #'eq)))
    (if (= 2 (length unique-numbers))
	(apply #'* (mapcar #'first unique-numbers))
	0)))

(defun find-gears (input)
  ;; Returns a list of gears values (operands multiplied).
  (let ((parsed
	  (input-to-padded-array (parse-numbers-in-input input))))
    ;; find all #\*
    ;; collect the unique number lists around it
    ;; if got 2, multiply & collect.
    (loop for i from 1 below (array-dimension parsed 0)
	  nconc (loop for j from 1 below (array-dimension parsed 1)
		      if (eql (aref parsed i j) #\*)
			collect (maybe-gears-value parsed i j)))))

(defun part2 (input)
  (apply #'+ (find-gears input)))
  
(print (part2 *input*))
