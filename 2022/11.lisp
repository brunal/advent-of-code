(require :uiop)
(require :cl-ppcre)

(defparameter *input* (uiop:read-file-string "11.input"))

(defstruct monkey
  (id 0 :read-only t)
  (operation 0 :read-only t)
  (test 0 :read-only t)
  (positive-target 0 :read-only t)
  (negative-target 0 :read-only t)
  items
  (items-inspected-count 0))

(defparameter *monkey-pattern*
"Monkey (.+):
  Starting items: (.+)
  Operation: new = (.+)
  Test: divisible by (.+)
    If true: throw to monkey (.+)
    If false: throw to monkey (.+)
")

(defun parse-operation (op)
  "Parses something like 'old * 11'."
  (destructuring-bind (left op right) (uiop:split-string op :separator " ")
    (let ((parsed-op (if (string= op "*") #'* #'+))
	  (args (mapcar
		 (lambda (arg) (if (string= arg "old") 'old (parse-integer arg)))
		 (list left right))))
      (lambda (old)
	(apply parsed-op (mapcar (lambda (arg) (if (eq arg 'old) old arg)) args))))))

(defun parse-items (items)
  "Parses a comma-separated list of values."
  (mapcar #'parse-integer
	  (remove "" (uiop:split-string items :separator ", ") :test #'string=)))

(defun parse-input (input &aux (monkeys nil))
  "Returns an array of monkeys."
  (cl-ppcre:do-register-groups (id items op test test-true test-false)
      (*monkey-pattern* input)
    (push (make-monkey :id (parse-integer id)
		       :operation (parse-operation op)
		       :test (parse-integer test)
		       :positive-target (parse-integer test-true)
		       :negative-target (parse-integer test-false)
		       :items (parse-items items))
	  monkeys))
  (coerce (reverse monkeys) 'vector))

(defun play-round (monkeys worry-reducer)
  (loop for m across monkeys
	do (incf (monkey-items-inspected-count m) (length (monkey-items m)))
	do (loop for item in (monkey-items m)
		 for new-worry = (funcall worry-reducer (funcall (monkey-operation m) item))
		 for target-accessor = (if (= 0 (rem new-worry (monkey-test m)))
						  #'monkey-positive-target
						  #'monkey-negative-target)
		 do (push new-worry
			  (monkey-items (aref monkeys (funcall target-accessor m)))))
	do (setf (monkey-items m) nil)))

(defun play-rounds (rounds monkeys worry-reducer)
  (dotimes (i rounds)
    (play-round monkeys worry-reducer)))

(defun monkey-business (monkeys)
  (let ((activity (sort (loop for m across monkeys
			      collect (monkey-items-inspected-count m))
			#'>)))
    (* (first activity) (second activity))))

(defun part1 ()
  (let ((monkeys (parse-input *input*)))
    (play-rounds 20
		 monkeys
		 (lambda (worry) (floor worry 3)))
    (monkey-business monkeys)))

(defun part2 ()
  (let* ((monkeys (parse-input *input*))
	 (common-divisor (reduce #'* (loop for m across monkeys collect (monkey-test m)))))
    (play-rounds 10000
		 monkeys
		 (lambda (worry) (rem worry common-divisor)))
    (monkey-business monkeys)))
