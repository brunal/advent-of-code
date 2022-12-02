
(defun accumulate (acc line)
  (if (= 0 (length line))
      (cons '() acc)
      (cons (cons (parse-integer line) (car acc)) (cdr acc))))

(defun parse-input (lines)
  (reduce #'accumulate lines :initial-value '(())))

(defparameter *input* (parse-input (uiop:read-file-lines "01.input")))

(defun calories (input)
 (mapcar (lambda (bag) (apply #'+ bag)) input))

(defun part1 (input)
  (apply #'max (calories input)))

(print (part1 *input*))

(defun part2 (input)
  (apply #'+ (subseq (sort (calories input) #'>) 0 3)))

(print (part2 *input*))
  
	
