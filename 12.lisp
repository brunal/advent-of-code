(ql:quickload 'cl-utilities)

(defun print-graph (graph)
  (flet ((print-hash-entry (k v) (format t "~a -> ~a~%" k v)))
    (maphash #'print-hash-entry graph)))

(defun add-directed-edge (graph from to)
  (unless (string= to "start") ; forbide going back to the start.
    (setf
     (gethash from graph)
     (cons to (gethash from graph)))))

(defun add-edges (graph e1 e2)
  (add-directed-edge graph e1 e2)
  (add-directed-edge graph e2 e1))

(defun parse-input (lines)
  (let ((graph (make-hash-table :test #'equal)))
    (loop for line in lines
	  for (e1 e2) = (cl-utilities:split-sequence #\- line)
	  do (add-edges graph e1 e2))
    graph))

(defun visitable-neighbours (graph location visited-list)
  (set-difference
   (gethash location graph)
   visited-list
   :test #'string=))

(defun mark-visited (location visited-list)
  (if (string= location (string-upcase location))
      visited-list
      (cons location visited-list)))

(defun count-distinct-paths
    (graph neighbours-func mark-visited-func &optional (start "start") (visited-list nil))
  (if (string= start "end")
      1
      (loop for neighbour in (funcall neighbours-func graph start visited-list)
	    sum (count-distinct-paths graph neighbours-func mark-visited-func
				      neighbour
				      (funcall mark-visited-func neighbour visited-list)))))

(defun part1 (graph)
  (count-distinct-paths graph #'visitable-neighbours #'mark-visited))

(defparameter *test-input1* (parse-input (uiop:read-file-lines "12.input.test1")))
(assert (eql (part1 *test-input1*) 10))
(defparameter *test-input2* (parse-input (uiop:read-file-lines "12.input.test2")))
(assert (eql (part1 *test-input2*) 19))
(defparameter *test-input3* (parse-input (uiop:read-file-lines "12.input.test3")))
(assert (eql (part1 *test-input3*) 226))

(defparameter *input* (parse-input (uiop:read-file-lines "12.input")))
(print (count-distinct-paths *input*))

(defun visitable-neighbours2 (graph location visited-list)
  (if (car visited-list)
      (set-difference
       (gethash location graph)
       (cdr visited-list)
       :test #'string=)
      ;; 2nd visit allowed
      (gethash location graph)))

(defun mark-visited2 (location visited-list)
  (cond
    ;; unlimited visits?
    ((string= location (string-upcase location)) visited-list)
    ;; 2nd visit of a small cave?
    ((member location (cdr visited-list) :test #'string=) (cons t (cdr visited-list)))
    ;; 1st visit of a small cave
    (t (cons (car visited-list)
	     (cons location
		   (cdr visited-list))))))

;; Part2: one small cave can be visited twice.
(defun part2 (graph)
  (count-distinct-paths graph #'visitable-neighbours2 #'mark-visited2))

(assert (eql (part2 *test-input1*) 36))
(assert (eql (part2 *test-input2*) 103))
(assert (eql (part2 *test-input3*) 3509))

(print (part2 *input*))
