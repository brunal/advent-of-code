(require :uiop)

(defparameter *wall* #\#)

(defun parse-input (input)
  "Returns (list array start end)."
  (let ((array
	  (make-array (list (length input) (length (first input)))
		      :initial-contents (loop for row in input
					      collect (loop for col in (coerce row 'list)
							    collect (case col
								      (#\. nil)
								      (#\# *wall*)
								      (t (list col))))))))
    (list array #c(0 1) (complex (1- (array-dimension array 0))
				 (- (array-dimension array 1) 2)))))

(defparameter *input* (parse-input (uiop:read-file-lines "24.input")))
(defparameter *input-test* (parse-input (uiop:split-string "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#" :separator (string #\newline))))

(defun draw (map &optional current)
  (loop for i below (array-dimension map 0)
	do (write-line (coerce 
			(loop for j below (array-dimension map 1)
			      collect (cond
					((and current (= current (complex i j))) #\E)
					((null (aref map i j)) #\.)
					((characterp (aref map i j)) (aref map i j))
					((> (length (aref map i j)) 1) (digit-char (length (aref map i j))))
					(t (first (aref map i j)))))
			'string))))

(defun neighbours (map location)
  (loop for delta in '(-1 1 #c(0 -1) #c(0 1) 0)
	for pos = (+ location delta)
	if (and (array-in-bounds-p map (realpart pos) (imagpart pos))
		(not (aref map (realpart pos) (imagpart pos))))
	  collect pos))

(defparameter *blizzard-deltas*
  '((#\> . #c(0 1))
    (#\< . #c(0 -1))
    (#\v . 1)
    (#\^ . -1)))
  
(defun reset-dim (x delta dims)
  (flet ((reset-val (x d max)
	   (cond ((zerop d) x)
		 ((= 1 d) 1)
		 (t (- max 2)))))
    (complex (reset-val (realpart x) (realpart delta) (first dims))
	     (reset-val (imagpart x) (imagpart delta) (second dims)))))

(defun move-blizzard (map from blizzard)
  "Move `blizzard` from `from` onto `map`."
  (let* ((delta (cdr (assoc blizzard *blizzard-deltas*)))
	 (to (+ from delta)))
    (when (eql *wall* (aref map (realpart to) (imagpart to)))
      (setf to (reset-dim to delta (array-dimensions map))))
    (push blizzard (aref map (realpart to) (imagpart to)))))

(defun move-blizzards (map)
  "Moves all the blizzards once, returns the new map."
  (let* ((dims (array-dimensions map))
	 (rows (first dims))
	 (cols (second dims))
	 (updated (make-array dims :initial-element nil)))
    ;; start by copying the borders.
    (loop for i below rows
	  do (loop for j below cols
		   if (eql *wall* (aref map i j))
		     do (setf (aref updated i j) *wall*)))
    ;; then move the blizzards.
    (loop for i below rows
	  do (loop for j below cols
		   for current = (complex i j)
		   if (listp (aref map i j))
		     do (loop for blizzard in (aref map i j)
			      do (move-blizzard updated current blizzard))))
    updated))

(defun play-one-round (map possible-locations)
  "Returns (values new-map new-possible-locations)."
  (let ((new-map (move-blizzards map)))
    (values new-map
	    (remove-duplicates (loop for location in possible-locations
				     nconc (neighbours new-map location))
			       :test #'eql))))

(defun navigate (map from to)
  "Returns (values map round-count) to reach `to` from `from` the fastest."
  (loop with locations = (list from)
	for round-id from 1
	do (progn
	     (multiple-value-setq (map locations) (play-one-round map locations))
	     (when (member to locations :test #'eql)
	       (return (values map round-id))))))

(defun part1 (&optional (input *input*))
  (destructuring-bind (map start end) input
    (nth-value 1 (navigate map start end))))

(assert (= 18 (part1 *input-test*)))

(defun part2 (&optional (input *input*))
  (destructuring-bind (map start end) input
    (multiple-value-bind (map count1) (navigate map start end)
      (multiple-value-bind (map count2) (navigate map end start)
	(multiple-value-bind (map count3) (navigate map start end)
	  (declare (ignore map))
	  (+ count1 count2 count3))))))

(assert (= 54 (part2 *input-test*)))
