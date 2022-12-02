(defun parse-input (lines)
  (mapcar (lambda (l) (coerce l 'list)) lines))

(defparameter *input* (parse-input (uiop:read-file-lines "10.input")))
(defparameter *test-input* (parse-input (uiop:read-file-lines "10.input.test")))

(defun try-pop-symbol (stack symbol)
  "Either returns the modified stack, or 'invalid."
  (if (eql (car stack) symbol)
      (cdr stack)
      symbol))

(defun push-symbol (stack symbol)
  (cons symbol stack))

(defun eat-symbol (stack symbol)
  (if (member symbol '(#\( #\[ #\< #\{) :test #'eql)
      (push-symbol stack symbol)
      (try-pop-symbol
       stack
       (ccase symbol
	 (#\) #\()
	 (#\] #\[)
	 (#\} #\{)
	 (#\> #\<)))))

(defun process-line (line &optional (stack ()))
  "Returns one of ('corrupted . wrong-symbol), ('incomplete . remaining-stack), ('valid)."
  (cond
    (line (let ((stack-or-expected-symbol (eat-symbol stack (car line))))
	    (if (listp stack-or-expected-symbol)
		;; recurse
		(process-line (cdr line) stack-or-expected-symbol)
		;; report the invalid char
		(cons 'corrupted (car line)))))
    (stack (cons 'incomplete stack))
    (t '(valid))))

(defun line-value-part1 (line)
  (let ((result (process-line line)))
    (ccase (car result)
      (corrupted (ccase (cdr result)
		   (#\) 3)
		   (#\] 57)
		   (#\} 1197)
		   (#\> 25137)))
      (incomplete 0)
      (valid 0))))

(defun part1 (input)
  (apply #'+ (mapcar #'line-value-part1 input)))

(print (part1 *input*))

(defun symbol-score-part2 (symbol)
  (ccase symbol
    (#\( 1)
    (#\[ 2)
    (#\{ 3)
    (#\< 4)))

(defun compute-score-part2 (stack &optional (total-score 0))
  "Compute the score of what remains on the stack."
  (if (eq nil stack)
      total-score
      (compute-score-part2
       (cdr stack)
       (+ (* 5 total-score)
	  (symbol-score-part2 (car stack))))))

(defun line-value-part2 (line)
  (let ((result (process-line line)))
    (ccase (car result)
      (corrupted nil)
      (incomplete (compute-score-part2 (cdr result)))
      (valid nil))))

(defun median (values)
  (nth
   (/ (1- (length values)) 2)
   (sort values #'<)))

(defun part2 (input)
  (median (remove nil (mapcar #'line-value-part2 input))))

(print (part2 *input*))
