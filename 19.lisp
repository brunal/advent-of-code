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

(defun vec-op (op v1 v2)
  (map 'list op v1 v2))

(defun dot-product (a b)
  (apply #'+ (vec-op #'* a b)))

(defun cross-product (a b)
  (destructuring-bind (ax ay az) a
    (destructuring-bind (bx by bz) b
      (list
       (- (* ay bz) (* az by))
       (- (* ax bz) (* az bx))
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
  (map 'list #'+
       (mapcar (lambda (v) (* v (my-cos angle))) vector)
       (mapcar (lambda (v) (* v (my-sin angle)))
	       (cross-product vector rotation-vector))
       (mapcar (lambda (v) (* v (dot-product vector rotation-vector) (- 1 (my-cos angle))))
	       rotation-vector)))

(defparameter *unit-vectors*
  '((1 0 0) (0 1 0) (0 0 1) (-1 0 0) (0 -1 0) (0 0 -1)))

(defun is-null-vector (v)
  (every #'zerop v))

(defparameter *orientations*
  ;; the first x can be in a 6 directions. For each of those, there is 4 possible y.
  (loop for x in *unit-vectors*
	nconc (loop for y in *unit-vectors*
		    for z = (cross-product x y)
		    unless (is-null-vector z)
		      collect (list x y z))))

(defun matmul (orientation vector)
  (loop for axis in orientation
	collect (dot-product axis vector)))

(defun rotate-scanner-measure (measure orientation)
  "Returns what measure would be if orientation were (1 0 0) (0 1 0) (0 0 1)."
  ;; orientation can be seen as a matrix be which each measurement should be multiplied.
  (mapcar (lambda (m) (matmul orientation m)) measure))

(defun translate (m delta)
  (loop for mm in m collect (vec-op #'+ mm delta)))

(defun match-and-translate (m1 m2)
  "Tries to match measurements in m1 with m2 (without rotating)."
  ;; build a hash table of (delta vector) -> count. Stop when we found a delta that
  ;; happens 12+ times.
  ;; return m2 translated to match m1.
  (loop with deltas = (make-hash-table :test #'equal)
	for mm1 in m1 do
	  (loop for mm2 in m2
		for delta = (vec-op #'- mm1 mm2)
		;; did we find at least 12 combinations of points with this delta?
		if (>= (incf (gethash delta deltas 0)) 12)
		  ;; yes. translate all of m2 with it.
		  do (progn
		       (format t "delta: ~a~%" delta)
		       (return-from match-and-translate
			 (translate m2 delta))))))

(defun match-and-translate2 (m1 m2)
  "Tries to match measurements in m1 with m2 (without rotating)."
  ;; for each of m1, for each of m2. Suppose they're the same. How many of m2 are then in m1?
  (loop for mm1 in m1
	do (loop for mm2 in m2
		 for delta = (vec-op #'- mm1 mm2)
		 if (>=
		     (length (intersection (translate m2 delta) m1 :test #'equal))
		     12)
		   do (progn
			(format t "delta: ~a~%" delta)
			(return-from match-and-translate2
			  (translate m2 delta))))))

(defun try-merge-measurement (measurement merged-measurements)
  "Returns measurement rotated & translated in case of success)."
  (loop for o in *orientations*
	for rotated-m = (rotate-scanner-measure measurement o)
	do (loop for mm in merged-measurements
		 for translated-m = (match-and-translate mm rotated-m)
		 if translated-m do
		   (progn
		     (format t "orientation ~a~%now starts with ~a~%~%"
			     o (first translated-m))
		     (return-from try-merge-measurement translated-m)))))

(defun rot (v) (append (rest v) (list (first v))))

(defun merge-measurements (measurements &optional merged-measurements global-map)
  "Merges all measurements into global-map. merged-measurements is a list of
   rotated-and-translated-measurement that were already merged into global-map."
  (cond
    ;; init
    ((null merged-measurements)
     (format t "start from ~a...~%" (first (first measurements)))
     (merge-measurements (rest measurements)
			 (list (first measurements))
			 (first measurements)))
    ;; end
    ((null measurements) global-map)
    ;; merging
    (t
     (format t "try to merge ~a...~%"
	     (first (first measurements)))
     (let ((new-m
	     (try-merge-measurement (first measurements) merged-measurements)))
       (if new-m
	   ;; update map and recurse
	   (merge-measurements
	    (rest measurements)
	    (cons new-m merged-measurements)
	    (remove-duplicates (append global-map new-m)))
	   ;; queue it for future processing
	   (merge-measurements
	    (append (rest measurements) (list (first measurements)))
	    merged-measurements
	    global-map))))))

(defun part1 (&optional (input *input*))
  (length (merge-measurements input)))

(defparameter *modified-fifth*
  (translate
   (rotate-scanner-measure (fifth *input-test*) '((0 -1 0) (0 0 -1) (1 0 0)))
   '(-20 -1133 1061)))

;; works
(print
 (match-and-translate
  (third *input-test*)
  (rotate-scanner-measure (fifth *input-test*) '((0 1 0) (1 0 0) (0 0 -1)))))

;; does not T_T
(loop for o in *orientations*
      for translated = (match-and-translate (third *input-test*)
					    (rotate-scanner-measure *modified-fifth* o))
      when translated
	do (format t "~a~%" translated))
