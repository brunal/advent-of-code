(require :uiop)
(require :fare-memoization)

(defparameter *input* (uiop:read-file-lines "12.input"))
(defparameter *input-test* (uiop:split-string "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1" :separator (string #\newline)))

(defun parse-input (input)
  (loop for line in input
	for (springs counts) = (uiop:split-string line :separator " ")
	collect (list
		 (loop for char in (coerce springs 'list) collect (intern (string char)))
		 (mapcar #'parse-integer (uiop:split-string counts :separator ",")))))

(defparameter *broken* '\#)
(defparameter *ok* '\.)
(defparameter *unknown* '?)

(defun can-consume-damaged-springs (springs count)
  (cond
    ((null count) nil)
    ((< (length springs) count) nil)
    (t (loop for s in springs
	     for c from 0 upto count
	     if (and (< c count) (eq s *ok*))
	       return nil
	     if (and (= c count))
	       return (not (eq s *broken*))
	     finally ; no more springs
		     (return t)))))

(fare-memoization:memoize 'can-consume-damaged-springs)

(defun count-possibilities (springs counts)
  (let ((s (first springs))
	(c (first counts)))
    (cond
      ;; are we done?
      ((and (null springs) (null counts)) 1)
      ;; maybe skip ok
      ((eq s *ok*) (count-possibilities (rest springs) counts))
      ;; maybe eat (first counts) + 1 *ok*
      ((and (eq s *broken*)
	    (can-consume-damaged-springs springs c))
       (count-possibilities (nthcdr (1+ c) springs) (rest counts)))
      ;; handle unknown
      ((eq s *unknown*) (+ (count-possibilities (cons *ok* (rest springs)) counts)
			   (count-possibilities (cons *broken* (rest springs)) counts)))
      (t 0))))

(fare-memoization:memoize 'count-possibilities)

(defun part1 (input)
  (loop for line in (parse-input input)
	for (springs counts) = line
	sum (count-possibilities springs counts)))

(assert (= 21 (part1 *input-test*)))
(print (part1 *input*))

(defun unfold-springs (springs)
  (rest (loop for i from 1 upto 5
	      append (cons *unknown* springs))))

(defun unfold-counts (counts)
  (loop for i from 1 upto 5 append counts))

(defun part2 (input)
  (loop for line in (parse-input input)
	for (springs counts) = line
	sum (count-possibilities (unfold-springs springs) (unfold-counts counts))))

(assert (= 525152 (part2 *input-test*)))
