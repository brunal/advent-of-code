(require :cl-utilities)
(require :alexandria)

(defparameter +input+ (uiop:read-file-lines "04.input"))

(defun cd6r (list)
  (cdr (cdr (cddddr list))))

(defun make-board (lines)
  (make-array
   '(5 5)
   :initial-contents (mapcar
		      (lambda (line)
			(mapcar
			 (lambda (value) (list (parse-integer value)))
			 (cl-utilities:split-sequence #\Space line :remove-empty-subseqs t)))
		      lines)))

(defun parse-boards (input)
  (loop
    for (empty-line a b c d e) on input by #'cd6r
    collect (make-board (list a b c d e))))

(defun parse-input (input)
; 1 lines with values drawn, then 5*5 boards with a newline between them
; output: list whose first element is the list of number drawn, 2nd element is a list of 2d arrays
  (cons (mapcar #'parse-integer (cl-utilities:split-sequence #\, (car input)))
	(parse-boards (cdr input))))

(defparameter +game+ (parse-input +input+))
(defparameter +rolls+ (car +game+))
(defparameter +boards+ (cdr +game+))

(defun find-in-board (board value)
  "returns (row . column) where board is value, or nil."
  (loop for row from 0 upto 4 do
    (loop for column from 0 upto 4
	  when (eql (car (aref board row column)) value)
	    do (return-from find-in-board (cons row column)))))

(defun board-value (board row column)
  (car (aref board row column)))

(defun is-marked (board row column)
  (cdr (aref board row column)))

(defun mark-board (board row column)
  (rplacd (aref board row column) t))

(defun check-if-won-vertical (board column)
  (loop
    for row from 0 upto 4
    when (not (is-marked board row column))
      do (return nil)
	 finally (return t)))

(defun check-if-won-horizontal (board row)
  (loop
    for column from 0 upto 4
    when (not (is-marked board row column))
      do (return nil)
	 finally (return t)))

(defun check-if-won (board position)
  (or (check-if-won-horizontal board (car position))
      (check-if-won-vertical board (cdr position))))
  
(defun try-mark (board roll)
  "try to check a number in the board. returns t if it won"
  (let ((position (find-in-board board roll)))
    (if position
	; mark the cell then check if we won
	(progn
	  (mark-board board (car position) (cdr position))
	  (check-if-won board position)))))

(defun sum-unmarked-numbers (board)
  (loop
    for row from 0 upto 4
    sum
    (loop
      for column from 0 upto 4
      sum (if (not (is-marked board row column))
	      (board-value board row column)
	      0))))

(defun compute-score (board roll)
  (* roll
     (+ (sum-unmarked-numbers board))))

(defun part1 (rolls boards)
    (loop
      for roll in rolls
      do (loop
	   for board in boards
	   when (try-mark board roll)
	     do (progn
		  ;(print roll)
		  ;(print board)
		  (return-from part1 (compute-score board roll))))))

; note that running part1 mutates boards. I tried alexandria:copy-arrays but it
; doesn't seem to copy the cons cells.
; (print (part1 +rolls+ +boards+))

(defun part2 (rolls boards)
  (unless rolls (error "No more rolls!"))
  (let ((new-boards (remove-if
		      (lambda (board) (try-mark board (car rolls)))
		      boards)))
    (if (eq 1 (length boards))
	(part1 (cdr rolls) new-boards)
	(part2 (cdr rolls) new-boards))))

(print (part2 +rolls+ +boards+))
