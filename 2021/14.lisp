(ql:quickload 'cl-utilities)
(ql:quickload 'alexandria)

(defun print-hash-table (table)
  (flet ((print-hash-entry (k v) (format t "~a -> ~a~%" k v)))
    (maphash #'print-hash-entry table)))

(defun parse-rules (lines)
  (let ((table (make-hash-table :test #'equal)))
    (loop for line in lines
	  for (polymer-pair arrow new-polymer-str) = (cl-utilities:split-sequence #\Space line)
	  for (polymer-left polymer-right) = (coerce polymer-pair 'list)
	  for (new-polymer) = (coerce new-polymer-str 'list)
	  do (setf
	      (gethash (cons polymer-left polymer-right) table)
	      new-polymer))
    table))

(defun parse-input (input)
  (cons (parse-rules (cddr input)) (coerce (first input) 'list)))

(defparameter *input* (parse-input (uiop:read-file-lines "14.input")))
(defparameter *input-test* (parse-input (uiop:read-file-lines "14.input.test")))

(defun maybe-expand (rules left right)
  (let ((insert (gethash (cons left right) rules)))
    (if insert (list left insert) (list left))))

(defun expand-once (rules template)
  (apply #'nconc
	 (maplist
	  (lambda (tpl) (maybe-expand rules (first tpl) (second tpl)))
	  template)))

(defun expand-times (rules template times)
  (if (zerop times)
      template
      (expand-times rules (expand-once rules template) (1- times))))

(defun least-and-most-frequent (template)
  (let ((counts (make-hash-table :test #'eql))
	(most -1)
	(least most-positive-fixnum))
    (dolist (letter template) (incf (gethash letter counts 0)))
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (if (> v most) (setf most v))
       (if (< v least) (setf least v)))
     counts)
    (values least most)))

(defun part1 (input &optional (times 10))
  (multiple-value-bind (least most)
      (least-and-most-frequent
       (expand-times (car input) (cdr input) times))
    (- most least)))

(print (part1 *input*))

;; part2: we expand too much!
;; instead of keeping the template, store it as a hashtable of pairs
;; when expanding, increment/insert the pair in the hashmap.
;; we also need to keep the count of each letter: we can't recompute it
;; from the expanded template-table since the first/last letter only appear
;; once and not twice like the others.
(defun parse-template (template)
  "Turns the list of chars into hashmap of (char . next char) -> count cells, hashmap of letter count"
  (let ((template-table (make-hash-table :test #'equal))
	(letters-count (make-hash-table :test #'eql)))
    (loop for (left . (right . rest)) on template
	  do (progn
	       (if right
		   (incf (gethash (cons left right) template-table 0)))
	       (incf (gethash left letters-count 0))))
    (values template-table letters-count)))

(defun expand-once2 (rules template-table letters-count)
  (let ((old-template-table (alexandria:copy-hash-table template-table)))
    (maphash
     (lambda (expanding-pair new-letter)
       (let ((expanding-pair-count (gethash expanding-pair old-template-table)))
	 (when expanding-pair-count
	   ;; decrement the pairs getting separated
	   (decf
	    (gethash expanding-pair template-table)
	    expanding-pair-count)
	   (if (zerop (gethash expanding-pair template-table))
	       (remhash expanding-pair template-table))
	   ;; add the newly formed pairs
	   (incf
	    (gethash (cons (car expanding-pair) new-letter) template-table 0)
	    expanding-pair-count)
	   (incf
	    (gethash (cons new-letter (cdr expanding-pair)) template-table 0)
	    expanding-pair-count)
	   ;; update the count of the newly inserted letter
	   (incf (gethash new-letter letters-count 0) expanding-pair-count))))
     rules)))

(defun expand-times2 (rules template-table letters-count times)
  (dotimes (ti times)
    (expand-once2 rules template-table letters-count)))

(defun least-and-most-frequent2 (letters-count)
  (loop for count being each hash-value of letters-count
	maximizing count into max
	minimizing count into min
	finally (return (values min max))))

(defun part2 (input &optional (times 40))
  (multiple-value-bind (template-table letters-count)
      (parse-template (cdr input))
    (expand-times2 (car input) template-table letters-count times)
    (multiple-value-bind (least most)
	(least-and-most-frequent2 letters-count)
      (- most least))))

(print (part2 *input*))
