(require :uiop)

(defparameter *input* (parse-input (uiop:read-file-lines "13.input")))
(defparameter *input-test* (parse-input (uiop:split-string "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#" :separator (string #\newline))))

(defun split-list-on (list predicate)
  (loop with acc = nil
	for elem in list
	if (funcall predicate elem)
	  collect (reverse acc) into result
	and do (setf acc nil)
	else
	  do (push elem acc)
	finally
	   (return (append result (list (reverse acc))))))
	
(defun parse-input (input)
  (loop for block in (split-list-on input #'uiop:emptyp)
	collect (loop for line in block collect (coerce line 'list))))

(defun list-to-int (list)
  (reduce (lambda (x y) (+ (* 2 x) (if (char= y #\#) 1 0)))
	  list
	  :initial-value 0))

(defun block-ints (block)
  "Returns ((int signature for rows) (int signature for cols))."
  (list
   (loop for row in block collect (list-to-int row))
   (loop for col-idx from 0 below (length (first block))
	 collect (list-to-int
		  (loop for row in block collect (nth col-idx row))))))

(defun list-prefix-equal (l1 l2)
  (reduce (lambda (x y) (and x y))
	  (mapcar #'eq l1 l2)))

(defun find-symmetry (list)
  (loop with past = nil
	for idx from 1
	for (cur . rest) on list
	do (setf past (cons cur past))
	if (and rest (list-prefix-equal past rest))
	  return idx))

(defun block-symmetry (block)
  "Returns (row-sym col-sym). 1 is nil."
  (let* ((signatures (block-ints block))
	 (symmetries (mapcar #'find-symmetry signatures)))
    (assert (= 1 (count-if-not #'null symmetries)))
    symmetries))

(defun symmetry-value (row-sym col-sym)
  (+ (* (or row-sym 0) 100) (or col-sym 0)))
  
(defun part1 (input)
  (loop for block in input
	for symmetry = (block-symmetry block)
	sum (apply #'symmetry-value symmetry)))

(assert (= 405 (part1 *input-test*)))

(print (part1 *input*))

;; part2 :rather than find-symmetry, compute the difference. If a
;; single element is not zerop and a single bit of difference, and
;; symmetry is different from the natural symmetry, success.

(defun list-prefix-nearly-equal (l1 l2)
  "Checks if only 1 item differs between l1 and l2, by a single bit."
  (let ((diffs (loop for ll1 in l1
		     for ll2 in l2
		     if (/= ll1 ll2)
		       collect (list ll1 ll2))))
    (and (= 1 (length diffs))
	 ;; single bit diff = xor of the numbers is a power of 2.
	 (let ((xor (apply #'logxor (first diffs))))
	   (zerop (logand xor (1- xor)))))))
  
(defun find-nearly-symmetry (list sym-idx)
  (loop with past = nil
	for idx from 1
	for (cur . rest) on list
	do (push cur past)
	if (and rest
		(list-prefix-nearly-equal past rest)
		(/= idx (or sym-idx 0)))
	  return idx))

(defun block-nearly-symmetry (block)
  (let* ((signatures (block-ints block))
	 (symmetries (mapcar #'find-symmetry signatures))
	 (nearly-symmetries (mapcar #'find-nearly-symmetry signatures symmetries)))
    (assert (= 1 (count-if-not #'null nearly-symmetries)))
    nearly-symmetries))
  
(defun part2 (input)
  (loop for i from 0
	for block in input
	for symmetry2 = (block-nearly-symmetry block)
	sum (apply #'symmetry-value symmetry2)))

(assert (= 400 (part2 *input-test*)))

(print (part2 *input*))
