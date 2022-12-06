(require 'uiop)

(defun parse-input (file) (coerce (first (uiop:read-file-lines file)) 'list))

(defparameter *input* (parse-input "06.input"))

(defun all-different (list)
  (let ((hash (make-hash-table :test #'eq)))
    (loop for item in list
	  never (gethash item hash)
	  do (setf (gethash item hash) t))))

(defun find-n-different (unique-count input &optional (i 0))
  (if (all-different (subseq input 0 unique-count))
      (+ i unique-count)
      (find-n-different unique-count (cdr input) (1+ i))))

(defun part1 (input)
  (find-n-different 4 input))

(print (part1 (coerce "bvwbjplbgvbhsrlpgdmjqwftvncz" 'list))) ; 5
(print (part1 (coerce "nppdvjthqldpwncqszvftbrmjlhg" 'list))) ; 6

(print (part1 *input*))

(defun part2 (input)
  (find-n-different 14 input))

(print (part2 (coerce "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 'list))) ; 19
(print (part2 (coerce "nppdvjthqldpwncqszvftbrmjlhg" 'list))) ; 23

(print (part2 *input*))
