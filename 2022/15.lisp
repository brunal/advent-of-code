(require :uiop)
(require :cl-ppcre)

(defparameter *input* (uiop:read-file-lines "15.input"))
(defparameter *input-test* (uiop:split-string "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3" :separator (string #\newline)))

(defparameter *line-pattern* "^Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)$")

(defstruct sensor
  (sensor 0 :read-only t)
  (beacon 0 :read-only t)
  (distance 0 :read-only t))

(defun xs (sensor) (realpart (sensor-sensor sensor)))
(defun ys (sensor) (imagpart (sensor-sensor sensor)))

(defun norm (c) (+ (abs (realpart c)) (abs (imagpart c))))

(defun init-sensor (xs ys xb yb)
  (let ((s (complex xs ys))
	(b (complex xb yb)))
  (make-sensor :sensor s
	       :beacon b
	       :distance (norm (- s b)))))

(defun parse-input (input)
  (loop for line in input
	collect (multiple-value-bind (match groups)
		    (cl-ppcre:scan-to-strings *line-pattern* line)
		  (declare (ignore match))
		  (apply #'init-sensor (mapcar #'parse-integer (coerce groups 'list))))))

(defun sensor-intersection (sensor y)
  "Returns (xmin xmax) covered by the sensor at y."
  (let* ((dy (abs (- y (ys sensor))))
	 (dx (- (sensor-distance sensor) dy)))
    (unless (< dx 0)
      (list (- (xs sensor) dx) (+ (xs sensor) dx)))))

(defun count-positions (intervals exclude)
  "Returns the number of points inside any of intervals, outside exclude."
  (let ((positions (make-hash-table :test #'eql)))
    (loop for (x y) in intervals
	  do (loop for i from x upto y
		   do (setf (gethash i positions) t)))
    (loop for e in exclude
	  do (remhash e positions))
    (hash-table-count positions)))

(defun merge-interval (i1 i2)
  "Tries to merge 2 intervals. Either returns the union, or (values smallest largest)."
  (cond
    ((< (1+ (second i1)) (first i2)) (values i1 i2))
    ((< (1+ (second i2)) (first i1)) (values i2 i1))
    (t (list (min (first i1) (first i2)) (max (second i1) (second i2))))))
  
(defun merge-intervals (intervals new)
  "Returns a list of intervals representing the union. intervals have start sorted and are disjoint."
  ;; initialization
  (if (null intervals)
      (list new)
      (multiple-value-bind (i1 i2) (merge-interval (first intervals) new)
	(if (null i2)
	    ;; we merge!
	    (merge-intervals (rest intervals) i1)
	    ;; we did not...
	    (cons i1 (merge-intervals (rest intervals) i2))))))

(defun intervals-minus (intervals minus)
  "Returns a new list of sorted intervals, not containing minus."
  (if (null intervals)
      nil
      (destructuring-bind (start end) (first intervals)
	(cond
	  ((< minus start) intervals)
	  ((> minus end) (cons (first intervals) (intervals-minus (rest intervals) minus)))
	  ((eql minus start) (cons (list (1+ start) end) (rest intervals)))
	  ((eql minus end) (cons (list start (1- end)) (rest intervals)))
	  (t (cons (list start (1- minus))
		   (cons (list (1+ minus) end)
			 (rest intervals))))))))
  
(defun intervals-size (intervals)
  (loop for (start end) in intervals sum (1+ (- end start))))

(defun find-blocked-intervals (sensors y)
  (reduce #'intervals-minus
	  ;; remove beacons & sensors
	  nil
	  ;; blocking them is only needed for part1!
	  ;; move this logic to part1.
	;;   (loop for s in sensors 
	;; 	if (= (ys s) y) collect (xs s)
	;; 	  if (= (imagpart (sensor-beacon s)) y) collect (realpart (sensor-beacon s)))
	  :initial-value (reduce #'merge-intervals
				 (remove nil (loop for s in sensors
						   collect (sensor-intersection s y)))
				 :initial-value nil)))

(defun part1 (&optional (input *input*) (y 2000000))
  (intervals-size
   (find-blocked-intervals (parse-input input) y)))

;; (assert (eql (part1 *input-test* 10) 26))

(defun clip-intervals (intervals min max)
  (if (null intervals)
      nil
      (destructuring-bind (start end) (first intervals)
	(cond
	  ((< end min) (clip-intervals (rest intervals) min max))
	  ((< start min) (clip-intervals
			  (cons (list min end) (rest intervals))
			  min
			  max))
	  ((> start max) nil)
	  ((> end max) (list (list start max)))
	  (t (cons (first intervals) (clip-intervals (rest intervals) min max)))))))

(defun part2 (&optional (input *input*) (max 4000000))
  (loop with sensors = (parse-input input)
	for y from 0 upto max
	for blocked-intervals = (clip-intervals
				 (find-blocked-intervals sensors y) 0 max)
	if (> (length blocked-intervals) 1)
	  ;; do (format t "on row y=~A, intervals ~A show a free spot.~%" y blocked-intervals) 
	  return (+ y (* (1+ (second (first blocked-intervals))) 4000000))))
	
(assert (eql (part2 *input-test* 20) 56000011))

;; (defun points-surrounding (sensor)
;;   "Returns the 4 points that bound sensor box."
;;   (let ((d (1+ (sensor-distance sensor)))
;; 	(s (sensor-sensor sensor)))
;;     (list (+ s d)
;; 	  (+ s (complex 0 d))
;; 	  (- s d)
;; 	  (- s (complex 0 d)))))
;; 
;; (defun clip-axis (axis min max)
;;   "Given 2 pairs of points representing a line, clips it inside min max."
;;   (destructuring-bind (start end) axis
;;     (let ((x0 (realpart start))
;; 	  (y0 (imagpart start))
;; 	  (xend (realpart end))
;; 	  (yend (imagpart end)))
;;       (flet ((y (x) (+ y0 (/ (* (- yend y0) (- x x0)) (- xend x0))))
;; 	     (x (y) (+ x0 (/ (* (- xend x0) (- y y0)) (- yend y0)))))
;; 	;; don't reuse {x,y}{0,end} as we modify {start,end}.
;; 	;; TODO: refactor...
;; 	;; clip x0
;; 	(when (< (realpart start) min)
;; 	  (setf start (complex min (y min))))
;; 	(when (> (realpart start) max)
;; 	  (setf start (complex max (y max))))
;; 	;; clip xend
;; 	(when (< (realpart end) min)
;; 	  (setf end (complex min (y min))))
;; 	(when (> (realpart end) max)
;; 	  (setf end (complex max (y max))))
;; 	;; clip y0
;; 	(when (< (imagpart start) min)
;; 	  (setf start (complex (x min) min)))
;; 	(when (> (imagpart start) max)
;; 	  (setf start (complex (x max) max)))
;; 	;; clip yend
;; 	(when (< (imagpart start) min)
;; 	  (setf start (complex (x min) min)))
;; 	(when (> (imagpart start) max)
;; 	  (setf start (complex (x max) max)))
;;     (list start end)))))

;; (defun axes-surrounding (sensor min max)
;;   (destructuring-bind (p1 p2 p3 p4) (points-surrounding sensor)
;;     (mapcar (lambda (axis) (clip-axis axis min max))
;; 	    `((,p4 ,p1) (,p1 ,p2) (,p2 ,p3) (,p3 ,p4)))))

;; (defun find-free-spot (sensors min max)
;;   (let ((axes (loop for s in sensor
;; 		    collect (axes-surrounding s min max))))
;;     ;; build the list of candidates: find 4 axes from different sensors that
;;     ;; have an intersection.
;;   )
