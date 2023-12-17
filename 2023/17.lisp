(require :uiop)
(require :priority-queue)

(defun parse-input (input)
  (make-array (list (length input) (length (first input)))
	      :initial-contents
	      (loop for line in input
		    collect (loop for char in (coerce line 'list)
				  collect (parse-integer (string char))))))

(defparameter *input* (parse-input (uiop:read-file-lines "17.input")))
(defparameter *input-test* (parse-input (uiop:split-string "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533" :separator (string #\newline))))

(defun turn-left (direction)
  (complex (imagpart direction) (- (realpart direction))))

(defun turn-right (direction)
  (complex (- (imagpart direction)) (realpart direction)))

;; state = (location direction steps-straight)

(defun neighbours (map state min-straight-steps max-straight-steps)
  (destructuring-bind (location direction steps-straight) state
    (remove-if-not (lambda (st)
		     (and (< -1 (realpart (first st)) (array-dimension map 0))
			  (< -1 (imagpart (first st)) (array-dimension map 1))))
		   (flet ((walk (dir steps) (list (+ location dir) dir steps)))
		     (cond
		       ((= steps-straight max-straight-steps)
			(list (walk (turn-left direction) 1)
			      (walk (turn-right direction) 1)))
		       ((< steps-straight min-straight-steps)
			(list (walk direction (1+ steps-straight))))
		       (t (list (walk (turn-left direction) 1)
				(walk direction (1+ steps-straight))
				(walk (turn-right direction) 1))))))))

(defun find-shortest-path (map min-straight-steps max-straight-steps)
  (let ((distances (make-hash-table :test #'equal))
	(pq (priority-queue:make-pqueue #'<)))
    ;; initialize with the 2 possible start directions.
    (loop for start-dir in '(1 #c(0 1))
	  for start-state = (list 0 start-dir 0)
	  do (setf (gethash start-state distances) 0)
	  do (priority-queue:pqueue-push start-state 0 pq))
    (loop for state = (priority-queue:pqueue-pop pq)
	  do (loop for next-state in (neighbours map state min-straight-steps max-straight-steps)
		   for dist = (+ (gethash state distances most-positive-fixnum)
				 (aref map
				       (realpart (first next-state))
				       (imagpart (first next-state))))
		   if (< dist (gethash next-state distances most-positive-fixnum))
		     do (progn
			  (setf (gethash next-state distances) dist)
			  (priority-queue:pqueue-push next-state dist pq)))
	  if (priority-queue:pqueue-empty-p pq)
	    do (return))
    (report-result map distances min-straight-steps)))

(defun report-result (map distances min-straight-steps)
  (loop with end-location = (apply #'complex (mapcar #'1- (array-dimensions map)))
	for state being the hash-keys in distances using (hash-value dist)
		 if (and (equal (first state) end-location)
			 (>= (third state) min-straight-steps))
		   minimize dist))

(defun part1 (input)
  (find-shortest-path input 1 3))

(assert (= 102 (part1 *input-test*)))
(print (part1 *input*))

(defun part2 (input)
  (find-shortest-path input 4 10))

(assert (= 94 (part2 *input-test*)))
(print (part2 *input*))
