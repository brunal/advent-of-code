(require :uiop)

(defparameter *input* (uiop:read-file-lines "14.input"))
(defparameter *input-test* (uiop:split-string "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9" :separator (string #\newline)))

(defun parse-line (line)
  (loop for word in (uiop:split-string line :separator " ")
	unless (string= word "->")
	  collect (apply #'complex
			 (mapcar #'parse-integer (uiop:split-string word :separator ",")))))

(defun line-between (start end)
  (loop with delta = (- end start)
	with distance = (abs delta)
	with increment-float = (/ delta distance)
	with increment-int = (complex (truncate (realpart increment-float))
				      (truncate (imagpart increment-float)))
	for i from 0 upto distance
	collect (+ start (* increment-int i))))

(defun list-cells (steps)
  (loop for (start end) on steps
	while end
	nconc (line-between start end)))

(defun build-cave (&optional (input *input*) &aux (cave (make-hash-table :test #'eql)))
  (loop for line in input
	for steps = (parse-line line)
	do (loop for cell in (list-cells steps)
		 do (setf (gethash cell cave) 'R)))
  cave)

(defparameter *sand-start* #c(500 0))

(defun deepest-cave-point (cave)
  (loop for key being the hash-keys in cave
	maximize (imagpart key)))

(defun let-sand-fall (cave sand-location max-depth)
  "Returns the end coords of the sand, or 'END."
  (if (= (imagpart sand-location) max-depth)
      'END
      (progn
	(loop for delta in '(#c(0 1) #c(-1 1) #c(1 1))
	      for candidate = (+ sand-location delta)
	      if (null (gethash candidate cave))
		do (return-from let-sand-fall (let-sand-fall cave candidate max-depth)))
	;; all spots are taken.
	sand-location)))

(defun sand-until (cave goal &aux (max-depth (deepest-cave-point cave)))
  "Let sand fall until a grain stops at goal."
  (loop for i from 0 upto (* max-depth max-depth)
	for location = (let-sand-fall cave *sand-start* max-depth)
	do (if (eql location goal)
	       (return i)
	       (setf (gethash location cave) 'S))))

(defun part1 (&optional (input *input*))
  (let ((cave (build-cave input)))
    (values
     (sand-until cave 'END)
     cave)))

(defun add-floor-to-cave (cave &aux (max-depth (deepest-cave-point cave)))
  (let* ((new-floor-depth (+ 2 max-depth))
	 (new-floor-start (complex (- *sand-start* new-floor-depth) new-floor-depth))
	 (new-floor-end (complex (+ *sand-start* new-floor-depth) new-floor-depth)))
    (loop for new-floor-tile in (line-between new-floor-start new-floor-end)
	  do (setf (gethash new-floor-tile cave) 'R))))

(defun part2 (&optional (input *input*))
  (let ((cave (build-cave input)))
    (add-floor-to-cave cave)
    ;; sand-until stop right before the sand stops at goal, so we need to 1+ it.
    (values
     (1+ (sand-until cave *sand-start*))
     cave)))


;; for fun
;; usage:
;; (multiple-value-bind (ret cav) (part2) (draw-cave cave))
(defun draw-cave (cave)
  (loop for key being the hash-keys in cave
	for x = (realpart key)
	for y = (imagpart key)
	minimize x into min-x
	maximize x into max-x
	maximize y into max-y
	finally (loop for y from 0 upto max-y
		      do (print (coerce
				 (loop for x from min-x upto max-x
				       collect (case (gethash (complex x y) cave)
						 (S #\.)
						 (R #\#)
						 ((nil) #\space)))
				 'string)))))
