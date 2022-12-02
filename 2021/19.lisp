(ql:quickload 'cl-utilities)

(defun parse (input)
  (unless (null input)
    (assert (string= (subseq (first input) 0 4) "--- "))
    (loop for (line . rest) on (rest input)
	  while (> (length line) 0)
	  collect (mapcar #'parse-integer (cl-utilities:split-sequence #\, line)) into coords
	  finally (return (cons coords (parse rest))))))

(defparameter *input* (parse (uiop:read-file-lines "19.input")))
(defparameter *input-test* (parse (uiop:read-file-lines "19.input.test")))

(defun vec-op (op &rest vectors)
  (apply #'map 'list op vectors))

(defun dot-product (a b)
  (apply #'+ (vec-op #'* a b)))

(defun cross-product (a b)
  (destructuring-bind (ax ay az) a
    (destructuring-bind (bx by bz) b
      (list
       (- (* ay bz) (* az by))
       (- (* az bx) (* ax bz))
       (- (* ax by) (* ay bx))))))

;; angle is 1 2 or 3
;; cos(0) = 1, cos(1) = 0, cos(2) = -1, cos(3) = 0
;; sin(0) = 0, sin(1) = 1, sin(2) = 0, sin(3) = -1
;; => sin(n) = cos(n-1)
(defun my-cos (angle)
  (ecase angle
    (0 1)
    (1 0)
    (2 -1)
    (3 0)))

(defun my-sin (angle)
  (my-cos (mod (1- angle) 4)))

(defun rotate-vector (vector rotation-vector angle)
  (vec-op #'+
	  (mapcar (lambda (v) (* v (my-cos angle))) vector)
	  (mapcar (lambda (v) (* v (my-sin angle)))
		  (cross-product vector rotation-vector))
	  (mapcar (lambda (v) (* v
				 (dot-product vector rotation-vector)
				 (- 1 (my-cos angle))))
		  rotation-vector)))

(defparameter *unit-vectors*
  '((1 0 0) (0 1 0) (0 0 1) (-1 0 0) (0 -1 0) (0 0 -1)))

(defparameter *orientations*
  (loop for x in *unit-vectors*
	nconc (loop for y in *unit-vectors*
		    for z = (cross-product x y)
		    unless (every #'zerop z)
		      collect (list x y z))))

(defun matmul (orientation vector)
  (loop for axis in orientation
	collect (dot-product axis vector)))

(defun rotate (measure orientation)
  "Returns what measure would be if orientation were (1 0 0) (0 1 0) (0 0 1)."
  ;; orientation can be seen as a matrix be which each measurement should be multiplied.
  (mapcar (lambda (m) (matmul orientation m)) measure))

(defun translate (m delta)
  (loop for mm in m collect (vec-op #'+ mm delta)))

(defun find-matching-delta (m1 m2)
  "Tries to match measurements in m1 with m2 (without rotating)."
  ;; build a hash table of (delta vector) -> count. Stop when we found a delta that
  ;; happens 12+ times. Return that delta.
  (loop with deltas = (make-hash-table :test #'equal)
	for mm1 in m1 do
	  (loop for mm2 in m2
		for delta = (vec-op #'- mm1 mm2)
		;; did we find at least 12 combinations of points with this delta?
		if (>= (incf (gethash delta deltas 0)) 12)
		  do (return-from find-matching-delta delta))))

(defun try-merge-measurement (measurement merged-measurements)
  "Returns the transformation to match the measurement."
  (loop for o in *orientations*
	for rotated-m = (rotate measurement o)
	do (loop for (mm-delta . mm) in merged-measurements
		 for delta = (find-matching-delta mm rotated-m)
		 if delta do
		   (progn
		     (format t "orientation ~a~%delta ~a~%used to be~%~a~%now becomes~%~a~%~%"
			     o delta measurement (translate rotated-m delta))
		     (return-from try-merge-measurement
		       (cons delta (translate rotated-m delta)))))))

;; used for debugging
(defun mergeability-matrix (measurements)
  ;; the matrix returned here should:
  ;; * have t in (i i) (measures are compatible with themselves)
  ;; * be its own transpose (compatible(a,b) == compatible(b,a))
  ;; * have at least 2 t in each column/row (compatible with itself and another one)
  (let ((merge-matrix (make-array (list (length measurements)
					(length measurements)))))
    (loop for m1 in measurements
	  for i upfrom 0
	  do (loop for m2 in measurements
		   for j upfrom 0
		   do (loop for o in *orientations*
			    for m2-rot = (rotate m2 o)
			    for delta = (find-matching-delta m1 m2-rot)
			    if delta
			      do (progn
				   (setf (aref merge-matrix i j) t)
				   (return)))))
    merge-matrix))

(defun merge-measurements (measurements &optional merged-measurements)
  "Returns merged measurements: a list of
   (translation . rotated-and-translated-measurement) with a bonus sentinel."
  (cond
    ;; init.
    ((null merged-measurements)
     (format t "start from ~a...~%" (first (first measurements)))
     (merge-measurements
      (append (rest measurements) (list (1- (length measurements))))
      (list (cons '(0 0 0) (first measurements)))))
    ;; end.
    ((null measurements) merged-measurements)
    ;; sanity check after looping through all measurements.
    ((numberp (first measurements))
     (if (= (length (rest measurements)) (first measurements))
	 (error "full loop, still ~a measurements left." (first measurements))
	 (progn
	   (format t "full loop, went from ~a to ~a measures to merge.~%"
		   (first measurements) (length (rest measurements)))
	   (merge-measurements
	    ;; do not re-add the sending if we're down to 0.
	    (if (null (rest measurements))
		'()
		(append (rest measurements) (list (length (rest measurements)))))
	    merged-measurements))))
    ;; merging.
    (t
     (format t "try to merge ~a...~%" (first (first measurements)))
     (let* ((m (first measurements))
	    (delta-and-new-m (try-merge-measurement m merged-measurements)))
       (if delta-and-new-m
	   ;; update map and recurse
	   (merge-measurements
	    (rest measurements)
	    (cons delta-and-new-m merged-measurements))
	   ;; queue it for future processing
	   (merge-measurements
	    (append (rest measurements) (list m))
	    merged-measurements))))))

(defun build-global-map (deltas-and-measurements)
  (remove-duplicates
   (apply #'append
	  (mapcar #'cdr deltas-and-measurements))
   :test #'equal))

(defun part1 (&optional (input *input*))
  (length (build-global-map (merge-measurements input))))

(defun manhattan (v1 v2)
  (apply #'+
	 (map 'list (lambda (i1 i2) (abs (- i1 i2))) v1 v2)))

(defun part2 (&optional (input *input*))
  (let ((scanners (mapcar #'car (merge-measurements input))))
    (loop for s1 in scanners
	  maximize (loop for s2 in scanners
			 maximize (manhattan s1 s2)))))
