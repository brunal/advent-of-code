(require :uiop)

(defparameter *input*
  (uiop:split-string (first (uiop:read-file-lines "15.input"))
		     :separator ","))
(defparameter *input-test*
  (uiop:split-string "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" :separator ","))

(defun hash-string (string)
  (reduce (lambda (acc char) (rem (* 17 (+ acc (char-code char))) 256))
	  (coerce string 'list)
	  :initial-value 0))

(defun part1 (input)
  (apply #'+ (mapcar #'hash-string input)))

(assert (= 1320 (part1 *input-test*)))
(print (part1 *input*))


(defun remove-lens (boxes label)
  (let* ((box-number (hash-string label))
	 (box (aref boxes box-number)))
    (setf
     (aref boxes box-number)
     (delete-if (lambda (l) (string= (first l) label)) box))))

(defun add-lens (boxes label focal-length)
  (let* ((box-number (hash-string label))
	 (box (aref boxes box-number)))
    (loop for lens in box
	  if (string= (first lens) label)
	    do (progn
		 (rplacd lens focal-length)
		 (return))
	  finally
	     (progn
	       (let ((lens (cons label focal-length)))
		 (setf (aref boxes box-number)
		       (push lens box)))))))

(defun run-instruction (boxes instruction)
  (if (char= #\- (char instruction (1- (length instruction))))
      (remove-lens boxes (subseq instruction 0 (1- (length instruction))))
      (add-lens boxes
		(subseq instruction 0 (- (length instruction) 2))
		(parse-integer (subseq instruction (1- (length instruction)))))))

(defun part2 (input)
  (let ((boxes (make-array '(256) :initial-element nil)))
    (loop for instruction in input
	  do (run-instruction boxes instruction))
    (loop for i from 1
	  for box across boxes
	  sum (loop for j from 1
		    for (label . focal-length) in (reverse box)
		    sum (* i j focal-length)))))

(assert (= 145 (part2 *input-test*)))
(print (part2 *input*))
