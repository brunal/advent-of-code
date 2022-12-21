(require :uiop)
(require :trivia)

(defun parse-input (input &aux (table (make-hash-table :test #'equal)))
  (loop for line in input
	for (monkey _ . rest) = (uiop:split-string line :separator ": ")
	do (setf (gethash monkey table)
		 (if (= 1 (length rest))
		     (parse-integer (first rest))
		     (list (intern (second rest)) (first rest) (third rest)))))
  table)

(defparameter *input* (parse-input (uiop:read-file-lines "21.input")))
(defparameter *input-test* (parse-input (uiop:split-string "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32" :separator (string #\newline))))

(defun symbol-to-op (symbol)
  (ecase symbol
    (+ #'+)
    (- #'-)
    (* #'*)
    (/ #'/))) 

(defun part1-resolve (table key &aux (value (gethash key table)))
  (if (integerp value)
      value
      (funcall (symbol-to-op (first value))
	       (part1-resolve table (second value))
	       (part1-resolve table (third value)))))

(defun part1 (&optional (input *input*))
  (part1-resolve input "root"))

(defun as-tree (table root)
  "Turns the table into a tree, overriding root/humn values. Each node is either an int or (op left right)."
  (let ((val (gethash root table)))
    (cond
      ((string= root "humn") '?)
      ((integerp val) val)
      (t (list (if (string= root "root") '= (first val))
	       (as-tree table (second val))
	       (as-tree table (third val)))))))

(defun simplify (node)
  (if (or (integerp node) (symbolp node))
      node
      (destructuring-bind (op left-node right-node) node
	(let ((left (simplify left-node))
	      (right (simplify right-node)))
	  (if (and (integerp left) (integerp right))
	      (funcall (symbol-to-op op) left right)
	      (list op left right))))))

(defun const-on-right (node)
  (destructuring-bind (op left-node right-node) node
    (if (integerp left-node)
	(list op right-node left-node)
	node)))

(defun rewrite-once (node)
  "Reduces the depth of `node` by one."
  (destructuring-bind (equals eq result) node
    (assert (eq '= equals))
    (assert (integerp result))
    (trivia:match eq
      ((list '+ op1 (and (type integer) op2)) (list '= op1 (- result op2)))
      ((list '+ (and (type integer) op1) op2) (list '= op2 (- result op1)))
      ((list '* op1 (and (type integer) op2)) (list '= op1 (/ result op2)))
      ((list '* (and (type integer) op1) op2) (list '= op2 (/ result op1)))
      ((list '- op1 (and (type integer) op2)) (list '= op1 (+ result op2)))
      ((list '- (and (type integer) op1) op2) (list '= op2 (- op1 result)))
      ((list '/ op1 (and (type integer) op2)) (list '= op1 (* result op2)))
      ((list '/ (and (type integer) op1) op2) (list '= op2 (/ op1 result))))))

(defun solve (node)
  "Returns the value of '? that satisfies the equation."
  ;; one leaf is an int, the other an equation.
  (let ((rewritten (rewrite-once node)))
    (if rewritten
	(solve rewritten)
	;; we're done, return the result
	(third node))))

(defun part2 (&optional (input *input*))
  ;; turn table into a tree + fix root & humn.
  (solve (const-on-right (simplify (as-tree input "root")))))

(assert (= (part2 *input-test*) 301))
