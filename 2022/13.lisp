(require :uiop)

(defparameter *input* (uiop:read-file-lines "13.input"))
(defparameter *input-test* (uiop:split-string "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]" :separator (string #\newline)))
  
(defun read-packet (line)
  (read
   (make-string-input-stream
    (substitute #\) #\]
		(substitute #\( #\[
			    (substitute #\space #\, line))))))

(defun read-packets (input)
  (loop for (left right empty) on input by #'cdddr 
	collect (mapcar #'read-packet (list left right))))
  
(defparameter *packets* (read-packets *input*))
(defparameter *packets-test* (read-packets *input-test*))

(defun cmp-packets (left right &aux (ll (listp left)) (rl (listp right)))
  "Compares 2 atoms. Returns one of 'S 'G 'U (smaller greater undecided)."
  (cond
    ((and (not ll) (not rl)) (cond ((< left right) 'S)
				   ((> left right) 'G)
				   (t 'U)))
    ((and ll rl) (cond ((and (null left) (null right)) 'U)
		       ((null left) 'S)
		       ((null right) 'G)
		       (t (case (cmp-packets (first left) (first right))
			    (S 'S)
			    (G 'G)
			    (U (cmp-packets (rest left) (rest right)))))))
    (rl (cmp-packets (list left) right))
    (ll (cmp-packets left (list right)))))
  
(defun part1 (&optional (packets *packets*))
  (loop for (left right) in packets
	for i from 1 upto (length packets)
	if (eq 'S (cmp-packets left right))
	  sum i))
  
(defun packet-smaller-p (p1 p2)
  (ecase (cmp-packets p1 p2)
    (S t)
    (G nil)))

(defun part2  (&optional (packets *packets*))
  (let* ((special-packets '(((2)) ((6))))
	 (sorted (sort (apply #'append special-packets (copy-tree packets))
		       #'packet-smaller-p)))
    (apply #'* (loop for special in special-packets
		     collect (1+ (position special sorted))))))
