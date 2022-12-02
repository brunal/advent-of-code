(ql:quickload 'cl-ppcre)
(ql:quickload 'cl-utilities)

(setf *print-circle* t)
(defparameter *debug* nil)

(defun snailfish-parse (input)
  "Turns a string into a snailfish."
  (read-from-string
   (cl-ppcre:regex-replace-all
    ","
    (cl-ppcre:regex-replace-all
     "\\]"
     (cl-ppcre:regex-replace-all "\\[" input "(")
     ")")
    " . ")))

(defun try-split (s)
  "Returns (modified-p . new-s)."
  (if (numberp s)
      (if (>= s 10)
	  ;; split the value
	  (list* t (floor (/ s 2)) (ceiling (/ s 2)))
	  (cons nil s))
      (destructuring-bind (modified . new-car-s) (try-split (car s))
	(if modified
	    (list* t new-car-s (cdr s))
	    (destructuring-bind (modified . new-cdr-s) (try-split (cdr s))
	      (list* modified (car s) new-cdr-s))))))

(defun add-leftmost (s value)
  (cond
    ((null value) s)
    ((numberp s) (+ s value))
    ( t (cons (add-leftmost (car s) value) (cdr s)))))

(defun add-rightmost (s value)
  (cond
    ((null value) s)
    ((numberp s) (+ s value))
    (t (cons (car s) (add-rightmost (cdr s) value)))))

(defun try-explode (s &optional (depth 0))
  "Returns (modified-p new-s left-explode-value right-explode-value)."
  (cond
    ((numberp s) (list nil s nil nil))
    ((>= depth 4) (list t 0 (car s) (cdr s)))
    (t (destructuring-bind
	   (modified left-reduce left-explode-value right-explode-value)
	   (try-explode (car s) (1+ depth))
	 (if modified
	     (list t
		   (cons left-reduce
			 (add-leftmost (cdr s) right-explode-value))
		   left-explode-value
		   nil)
	     (destructuring-bind
		 (modified right-reduce left-explode-value right-explode-value)
		 (try-explode (cdr s) (1+ depth))
	       (if modified
		   (list t
			 (cons (add-rightmost (car s) left-explode-value)
			       right-reduce)
			 nil
			 right-explode-value)
		   (list nil s nil nil))))))))

(defun loop-reduce (s)
  (when *debug*
    (format t "~a~%" (snailfish-string s)))
  (destructuring-bind (modified new-s ef er) (try-explode s)
    (declare (ignore ef er))
    (if modified
	(loop-reduce new-s)
	(destructuring-bind (modified . new-s) (try-split s)
	  (if modified
	      (loop-reduce new-s)
	      new-s)))))

(defun snailfish-add (s1 s2)
  (loop-reduce (cons s1 s2)))

(defun snailfish-print-to-stream (sn st)
  (if (numberp sn)
      (format st "~a" sn)
      (progn
	(format st "[")
	(snailfish-print-to-stream (car sn) st)
	(format st ",")
	(snailfish-print-to-stream (cdr sn) st)
	(format st "]"))))

(defun snailfish-string (s)
  "Turns a snailfish into a string."
  (let ((stream (make-string-output-stream)))
    (snailfish-print-to-stream s stream)
    (get-output-stream-string stream)))

;; since we destructively modify the expressions, always return fresh ones.
(defparameter *input*
  (mapcar #'snailfish-parse (uiop:read-file-lines "18.input")))

(defparameter *input-test*
  (mapcar #'snailfish-parse (uiop:read-file-lines "18.input.test")))

(defparameter *input-test2*
  (mapcar #'snailfish-parse (uiop:read-file-lines "18.input.test2")))

(defparameter *snailfish-test*
  (snailfish-parse "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"))

(defun snailfish-magnitude (s)
  (if (numberp s)
      s
      (+ (* 3 (snailfish-magnitude (car s)))
	 (* 2 (snailfish-magnitude (cdr s))))))

(assert (= 3488
	   (snailfish-magnitude
	    (snailfish-parse
	     "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))

(defun part1 (&optional (input *input*))
  (snailfish-magnitude (reduce #'snailfish-add input)))

(defun part2 (&optional (input *input*))
  (loop for left in input
	maximize (loop for right in input
		       maximize (snailfish-magnitude
				 (snailfish-add left right)))))

;; below is test code due to me failing super hard with my first implementation.

(defun test-sum (text result)
  (let ((successive-snailfishes
	  (mapcar #'snailfish-parse
		  (cl-utilities:split-sequence #\linefeed text))))
    (loop with value-so-far = (first successive-snailfishes)
	  for right-value in (rest successive-snailfishes)
	  do (progn
	       (format t "  ~a~%+ ~a~%"
		       (snailfish-string value-so-far)
		       (snailfish-string right-value))
	       (let ((new-value (snailfish-add value-so-far right-value)))
		 (format t "= ~a~%~%" (snailfish-string new-value))
		 (setf value-so-far new-value)))
	  finally (assert (string= (snailfish-string value-so-far) result)))))

(test-sum
 "[[[[4,3],4],4],[7,[[8,4],9]]]
[1,1]"
 "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

(test-sum
 "[1,1]
[2,2]
[3,3]
[4,4]"
 "[[[[1,1],[2,2]],[3,3]],[4,4]]")

(test-sum
 "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]"
 "[[[[3,0],[5,3]],[4,4]],[5,5]]")

(test-sum
 "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"
 "[[[[5,0],[7,4]],[5,5]],[6,6]]")

;; this fails
(test-sum
 "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
 "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
;; mine:
;;[[[[6,7],[7,7]],[[7,7],[7,0]]],[[[7,7],[7,8]],[[8,8],[8,9]]]]

(defun assert-operation (left right expected-result)
  (assert (string= (snailfish-string
		    (snailfish-add
		     (snailfish-parse left)
		     (snailfish-parse right)))
		   expected-result)))

(defun test-add (lines)
  (apply #'assert-operation
	 (mapcar
	  (lambda (line) (subseq line 2))
	  (rest (cl-utilities:split-sequence #\linefeed lines)))))

(test-add "
  [[[[4,3],4],4],[7,[[8,4],9]]]
+ [1,1]
= [[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

(let ((*debug* t))
  (test-add "
  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
+ [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
;;mine:
;;[[[[4,0],[5,4]],[[7,7],[6,0]]],[[[6,6],[5,6]],[[6,0],[7,7]]]]

(test-add "
  [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
+ [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
= [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]")

(test-add "
  [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
+ [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
= [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]")

(test-add "
  [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
+ [7,[5,[[3,8],[1,4]]]]
= [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]")

(test-add "
  [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
+ [[2,[2,2]],[8,[8,1]]]
= [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]")

(test-add "
  [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
+ [2,9]
= [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]")

(test-add "
  [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
+ [1,[[[9,3],9],[[9,0],[0,7]]]]
= [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]")

(test-add "
  [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
+ [[[5,[7,4]],7],1]
= [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]")

(test-add "
  [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
+ [[[[4,2],2],6],[8,7]]
= [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
