(ql:quickload 'cl-ppcre)
(ql:quickload 'cl-utilities)

(setf *print-circle* t)
(defparameter *debug* nil)

(defun snailfish-parse (input)
  "Turns a string into a snailfish."
  (with-input-from-string
      (stream (cl-ppcre:regex-replace-all
	       ","
	       (cl-ppcre:regex-replace-all
		"\\]"
		(cl-ppcre:regex-replace-all "\\[" input "(")
		")")
	       " "))
    (let ((s (parse-snailfish-structure (read stream))))
      (weave-leaves (get-all-leaves s))
      s)))

(defun test ()
  (loop-reduce (snailfish-parse "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")))
;; reboot :-| end

(defun snailfish-print-to-stream (sn st)
  (ecase (getf sn :type)
    (pair
     (format st "[")
     (snailfish-print-to-stream (getf sn :left) st)
     (format st ",")
     (snailfish-print-to-stream (getf sn :right) st)
     (format st "]"))
    (leaf (format st "~a" (getf sn :value)))))

(defun snailfish-string (s)
  "Turns a snailfish into a string."
  (let ((stream (make-string-output-stream)))
    (snailfish-print-to-stream s stream)
    (get-output-stream-string stream)))

(defun parse-snailfish-structure (s &optional (depth 0))
  "Recursively builds a snailfish from int-or-myself recursive cons cells."
  ;; note: having all properties on both types means that when we modify either (explode or split)
  ;; we actually modify the plist rather that than get another list that we would have to assign back.
  (if (numberp s)
      (list :type 'leaf
	    :depth depth
	    :value s
	    :prev nil
	    :next nil)
      (list :type 'pair
	    :depth depth
	    :left (parse-snailfish-structure (first s) (1+ depth))
	    :right (parse-snailfish-structure (second s) (1+ depth)))))

(defun get-all-leaves (s)
  "Returns a list of all the leaves in the snailfish."
  (if (eq 'pair (getf s :type))
      (nconc (get-all-leaves (getf s :left))
	     (get-all-leaves (getf s :right)))
      (list s)))

(defun weave-leaves (leaves)
  "Sets the :prev and the :next on all leaves."
  (loop for (prev current next) on (cons nil leaves)
	while current
	do (progn
	     (setf (getf current :prev) prev)
	     (setf (getf current :next) next))))

;; since we destructively modify the expressions, always return fresh ones.
(defun input () (mapcar #'snailfish-parse (uiop:read-file-lines "18.input")))
(defun input-test () (mapcar #'snailfish-parse (uiop:read-file-lines "18.input.test")))
(defun input-test2 () (mapcar #'snailfish-parse (uiop:read-file-lines "18.input.test2")))
(defun snailfish-test ()
  (snailfish-parse "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"))

;; note: (setf (getf plist ...) ...) does not always modify in-place the plist (e.g. if
;; the requested property is missing or has null value.
;; set-property guarantees the modification is done in-place.
(defun set-property (s prop value)
  "Sets the property to value in s in-place."
  (cond
    ((null s) (error "s is nil"))
    ((eq prop (first s)) (rplaca (cdr s) value))
    ((null (cddr s)) (rplacd (cdr s) (list prop value)))
    (t (set-property (cddr s) prop value))))

(defun explode-pair (s)
  (let* ((left (getf s :left))
	 (right (getf s :right))
	 (prev (getf left :prev))
	 (next (getf right :next)))
    (if prev
	(incf (getf prev :value) (getf left :value)))
    (if next
	(incf (getf next :value) (getf right :value)))
    ;; turn s into a leaf of value 0.
    (set-property s :left nil)
    (set-property s :right nil)
    (set-property s :type 'leaf)
    (set-property s :value 0)
    (set-property s :prev prev)
    (set-property s :next next)
    (if prev (set-property prev :next s))
    (if next (set-property next :prev s))))

(defun split-value (s)
  (let* ((value (getf s :value))
	 (prev (getf s :prev))
	 (next (getf s :next))
	 (left (list :type 'leaf
		     :value (floor (/ value 2))
		     :depth (1+ (getf s :depth))))
	 (right (list :type 'leaf
		      :value (ceiling (/ value 2))
		      :depth (1+ (getf s :depth)))))
    ;; turn s into a pair.
    (set-property s :value nil)
    (set-property s :prev nil)
    (set-property s :next nil)
    (set-property s :type 'pair)
    (set-property s :left left)
    (set-property s :right right)
    (if prev (set-property prev :next left))
    (set-property left :next right)
    (set-property right :next next)
    (if next (set-property next :prev right))
    (set-property right :prev left)
    (set-property left :prev prev)))
     
(defun explode-snailfish-once (s)
  (ecase (getf s :type)
    (pair
     ;; explode the pair if it is 4 levels deep
     (if (>= (getf s :depth) 4)
	 (progn (explode-pair s) t)
	 (or (explode-snailfish-once (getf s :left))
	     (explode-snailfish-once (getf s :right)))))
    (leaf nil)))

(defun split-snailfish-once (s)
  (ecase (getf s :type)
    (pair (or (split-snailfish-once (getf s :left))
	      (split-snailfish-once (getf s :right))))
    (leaf (when (>= (getf s :value) 10)
	    (split-value s)
	    t))))

(defun reduce-snailfish-once (s)
  "Performs up to 1 reduction on the input expression. Returns whether a reduction was performed."
  (or (explode-snailfish-once s)
      (split-snailfish-once s)))
 
(defun reduce-snailfish (s)
  (when *debug*
    (format t "~a~%" (snailfish-string s)))
  (when (reduce-snailfish-once s)
    (reduce-snailfish s)))

(defun incf-depth (s)
  (incf (getf s :depth))
  (when (eq (getf s :type) 'pair)
    (incf-depth (getf s :left))
    (incf-depth (getf s :right))))

(defun snailfish-add (s1 s2)
  (incf-depth s1)
  (incf-depth s2)
  (let ((s (list :type 'pair
		 :depth 0
		 :left s1
		 :right s2))
	(s1-rightmost (first (last (get-all-leaves s1))))
	(s2-leftmost (first (get-all-leaves s2))))
    (assert (null (getf s1-rightmost :next)))
    (assert (null (getf s2-leftmost :prev)))
    (set-property s1-rightmost :next s2-leftmost)
    (set-property s2-leftmost :prev s1-rightmost)
    (reduce-snailfish s)
    s))

(defun snailfish-magnitude (s)
  (ecase (getf s :type)
    (leaf (getf s :value))
    (pair (+
	   (* 3 (snailfish-magnitude (getf s :left)))
	   (* 2 (snailfish-magnitude (getf s :right)))))))

(assert (= 3488
	   (snailfish-magnitude
	    (snailfish-parse
	     "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))

(defun part1 (input)
  (snailfish-magnitude (reduce #'snailfish-add input)))

;; test things because I can't find the bug :-|
(defun test-sum (text result)
  (terpri)
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
