(require :uiop)

(defparameter *input* (uiop:read-file-lines "08.input"))
(defparameter *input-test* (uiop:split-string "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)" :separator (string #\newline)))

(defun parse-input (input)
  (cons (loop for char in (coerce (first input) 'list)
	      collect (intern (string char)))
	(loop with ht = (make-hash-table :test #'equal)
	      for line in (rest (rest input))
	      do (setf (gethash (subseq line 0 3) ht)
		       (list (subseq line 7 10)
			     (subseq line 12 15)))
	      finally (return ht))))

(defun make-loop-list (list)
  (loop for l on list
	if (null (cdr l))
	  return (progn
		   (rplacd l list)
		   list)))

(defun step-count-until-predicate-satisfied (steps map start-loc predicate)
  (loop with location = start-loc
	for step-count from 0
	for next-step in (make-loop-list (copy-list steps))
	for (L R) = (gethash location map)
	if (funcall predicate location)
	  return step-count
	do (setf location
		 (if (eq next-step 'L) L R))))
  
(defun part1 (input)
  (let* ((data (parse-input input))
	 (steps (car data))
	 (map (cdr data)))
    (step-count-until-predicate-satisfied steps map "AAA"
					  (lambda (l) (string= l "ZZZ")))))

;; This solution is correct but takes too long.
(defun part2-first-try (input)
  (let* ((data (parse-input input))
	 (steps (car data))
	 (map (cdr data))
	 ;; find all nodes names ending with A 
	 (start-locations (loop for k being the hash-keys of map
				if (char= #\A (char k 2)) collect k)))
    (loop with locations = start-locations
	  for step-count from 0
	  for next-step in (make-loop-list steps)
	  if (every (lambda (loc) (char= #\Z (char loc 2))) locations)
	    return step-count
	  if (zerop (mod step-count 100000000))
	    do (format t "steps ~A, we're at ~A~%" step-count locations)
	  do (setf locations
		   (loop for location in locations
			 for (L R) = (gethash location map)
			 collect (if (eq next-step 'L) L R))))))

;; Just solve each start-location independently & take the LCM of each
;; solution. This makes LOTS of assumptions about the input data, but
;; hey it works!
(defun part2 (input)
  (let* ((data (parse-input input))
	 (steps (car data))
	 (map (cdr data))
	 (start-locations (loop for k being the hash-keys of map
				if (char= #\A (char k 2)) collect k)))
    (apply #'lcm
	   (loop for start-loc in start-locations
		 collect (step-count-until-predicate-satisfied
			  steps map start-loc (lambda (l) (char= #\Z (char l 2))))))))
