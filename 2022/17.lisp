(require :uiop)

(defun parse-input (input)
  (remove #\newline (coerce input 'list)))

(defparameter *input* (parse-input (uiop:read-file-string "17.input")))
(defparameter *input-test* (parse-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))

;; description starts from top left.
(defparameter *rocks* (list
		       ;; ####
		       (make-array '(1 4) :initial-contents '((t t t t)))
		       ;; .#.
		       ;; ###
		       ;; .#.
		       (make-array '(3 3) :initial-contents '((nil t nil) (t t t) (nil t nil)))
		       ;; ..#
		       ;; ..#
		       ;; ###
		       (make-array '(3 3) :initial-contents '((nil nil t) (nil nil t) (t t t)))
		       ;; #
		       ;; #
		       ;; #
		       ;; #
		       (make-array '(4 1) :initial-contents '((t) (t) (t) (t)))
		       ;; ##
		       ;; ##
		       (make-array '(2 2) :initial-contents '((t t) (t t)))))

(defparameter *chamber-width* 7)

(defun new-row ()
  (make-array (list *chamber-width*) :initial-element nil))

(defun rock-fits-p (chamber rock rock-offset)
  "Checks if rock fits in chamber. Returns t or nil."
  (and
   (>= rock-offset 0)
   ;; deep enough.
   (nth (1- (array-dimension rock 0)) chamber)
   ;; wide enough.
   (<= (+ rock-offset (array-dimension rock 1)) *chamber-width*)
   ;; no clash with other rocks.
   (loop for row-id below (array-dimension rock 0)
	 for chamber-row in chamber
	 always (loop for col below (array-dimension rock 1)
		      never (and (aref rock row-id col)
				 (aref chamber-row (+ col rock-offset)))))))

(assert (rock-fits-p (list (new-row)) (first *rocks*) 0))
(assert (rock-fits-p (list (new-row)) (first *rocks*) 3))
(assert (not (rock-fits-p (list (new-row)) (first *rocks*) 4)))

(defun add-rock-to-chamber (chamber rock rock-offset)
  "Stores rock in chamber at rock-offset."
  (nconc
   (loop for row-id below (array-dimension rock 0)
	 for chamber-row in chamber
	 collect
	 (make-array (list *chamber-width*)
		     :initial-contents (loop for col below *chamber-width*
					     for current-cell across chamber-row
					     collect (cond ((< col rock-offset) current-cell)
							   ((>= col (+ (array-dimension rock 1) rock-offset)) current-cell)
							   (t (or current-cell (aref rock row-id (- col rock-offset))))))))
   (nthcdr (array-dimension rock 0) chamber)))

(defun play-rock (chamber jets rock &optional (rock-offset 2))
  "Simulates `rock` moving in chamber until it gets stuck. Returns the new chamber."
  ;; on the side
  (let ((new-offset (funcall (if (eql (funcall jets) #\>) #'1+ #'1-) rock-offset)))
    (when (rock-fits-p chamber rock new-offset)
      (setf rock-offset new-offset)))
  ;; and down
  (if (rock-fits-p (rest chamber) rock rock-offset)
      (cons (first chamber)
	    (play-rock (rest chamber) jets rock rock-offset))
      (add-rock-to-chamber chamber rock rock-offset)))

(defun pad-chamber (chamber rock)
  "Makes sure there are enough empty lines atop chamber."
  (let* ((rock-height (array-dimension rock 0))
	 (empty-rows-count (loop for row in chamber while (row-empty-p row) count 1))
	 ;; we need 3 + height of rock.
	 (missing-rows-count (- (+ 3 rock-height) empty-rows-count)))
    (cond ((> 0 missing-rows-count) (nthcdr (- missing-rows-count) chamber))
	  ((zerop missing-rows-count) chamber)
	  (t (nconc (loop for i below missing-rows-count collect (new-row)) chamber)))))

(defun row-empty-p (row)
  (loop for col across row never col))
    
(defun skip-empty-rows (chamber)
  (if (or (null chamber)
	  (not (row-empty-p (first chamber))))
      chamber
      (skip-empty-rows (rest chamber))))

(defun infinite-list (list)
  (let ((new (copy-list list)))
    (setf (cdr (last new)) new)))

(defun make-iterator (list)
  "Given a list, returns a function that returns the next element."
  (let ((current list))
    (lambda () (destructuring-bind (first . rest) current
		 (setf current rest)
		 first))))

(defun draw-chamber (chamber)
  (loop for row in chamber do (print (coerce
				      (loop for col across row collect (if col #\# #\.))
				      'string)))
  (terpri))

(defun simulate-fall (jets rock-count)
  "Let rock-count rocks fall."
  (loop with jets-iterator = (make-iterator (infinite-list jets))
	with chamber = nil
	for rock-id from 1 upto rock-count
	for rock in (infinite-list *rocks*)
	do (setf chamber (play-rock (pad-chamber chamber rock) jets-iterator rock))
	finally (return chamber)))

(defun part1 (&optional (jets *input*) (rocks-count 2022))
  (length (skip-empty-rows (simulate-fall jets rocks-count))))

(assert (= 3068 (part1 *input-test*)))
