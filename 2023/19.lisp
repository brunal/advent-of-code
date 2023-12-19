(require :uiop)

(defun upcase-symbol (string)
  (intern (coerce (mapcar #'char-upcase (coerce string 'list)) 'string)))

(defun parse-test (test)
  (list (upcase-symbol (subseq test 0 1))
	(if (string= (subseq test 1 2) "<") #'< #'>)
	(parse-integer (subseq test 2))))

(defun as-symbol-or-string (label)
  (if (= 1 (length label)) (upcase-symbol label) label))

(defun parse-cond (c)
  ;; a<2006:qkq
  (let ((cond-pieces (uiop:split-string c :separator ":")))
    (if (> (length cond-pieces) 1)
	(cons (parse-test (first cond-pieces)) (as-symbol-or-string (second cond-pieces)))
	(as-symbol-or-string (first cond-pieces)))))

(defun parse-workflow (workflow)
  ;; px{a<2006:qkq,m>2090:A,rfg}
  (destructuring-bind (name conds) (uiop:split-string workflow :separator "{")
    (list name
	  (mapcar #'parse-cond
		  (uiop:split-string (subseq conds 0 (1- (length conds))) :separator ",")))))

(defun parse-rating (rating)
  ;; Returns an alist (symbol . value).
  (loop for rat in (uiop:split-string (subseq rating 1 (1- (length rating)))
				      :separator ",")
	for (cat val) = (uiop:split-string rat :separator "=")
	collect (cons (upcase-symbol cat) (parse-integer val))))

(defun parse-input (input)
  (loop for (line . rest) on (uiop:split-string input :separator (string #\newline))
	if (uiop:emptyp line)
	  return (list
		  (mapcar #'parse-workflow workflows)
		  (mapcar #'parse-rating rest))
	else
	  collect line into workflows))

(defparameter *input* (parse-input (uiop:read-file-string "19.input")))
(defparameter *input-test* (parse-input "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"))

(defun process-rating (workflows rating &optional (workflow "in"))
  (if (symbolp workflow)
      workflow
      (loop with conds = (gethash workflow workflows)
	    for cond in conds
	    if (and (listp cond)
		    (funcall (second (car cond))
			     (cdr (assoc (first (car cond)) rating))
			     (third (car cond))))
	      return (process-rating workflows rating (cdr cond))
	    else if (not (listp cond))
		   return (process-rating workflows rating cond))))

(defun workflows-hashtable (workflows)
  (loop with workflows-ht = (make-hash-table :test #'equal
					     :size (length workflows))
	for (name conds) in workflows
	do (setf (gethash name workflows-ht) conds)
	   finally (return workflows-ht)))

(defun process-ratings (input)
  (loop with (workflows ratings) = input
	with workflows-ht = (workflows-hashtable workflows)
	for rating in ratings
	collect (process-rating workflows-ht rating)))

(defun part1 (input)
  (loop for rating in (second input)
	for result in (process-ratings input)
	if (eq 'A result)
	  sum (loop for (label . value) in rating
		    sum value)))

(assert (= 19114 (part1 *input-test*)))

;; part2
;; 1 state = (label . list of (category min max)
;; go through one step = apply workflow, get multiple states

(defun initial-state ()
  (cons "in" (loop for category in '(X M A S) collect `(,category 1 4000))))

(defun state-value (state)
  (if (eq (first state) 'A)
      (apply #'*
	     (loop for (cat min max) in (rest state)
		   collect (1+ (- max min))))
      0))

(defun make-rating-if-valid-split-cat (start cat splits end)
  (loop for split in splits
	collect (if (apply #'<= split)
		    (append start (list* (cons cat split) end))
		    nil)))

(defun split-rating (condition rating)
  "Returns the accepted & rejected ratings based on CONDITION = (category cmp value)."
  (loop with (cond-cat cmp value) = condition
	for (r . rest) on rating
	for (cat min max) = r
	if (eq cat cond-cat)
	  return (make-rating-if-valid-split-cat
		  rs
		  cat
		  (mapcar #'interval-validp
			  (if (eq cmp #'<)
			      (list (list min (1- value)) (list value max))
			      (list (list (1+ value) max) (list min value))))
		  rest)
	else
	  collect r into rs))

(defun apply-condition (condition rating)
  ;; Returns (values new-label accepted-rating rejected-rating).
  (if (listp condition)
      ;; conditional jump. Split the rating based on the condition
      ;; condition = ((cat #'<-or-#'> value) . label)
      ;; rating = list of (cat min max)
      (destructuring-bind (accepted rejected) (split-rating (car condition) rating)
	(values (cdr condition) accepted rejected))
      ;; inconditional jump.
      (values condition rating nil)))

(defun apply-workflow (workflow rating)
  (when rating
    ;; apply (first workflow) to get a new state + recurse on (rest workflow)
    (multiple-value-bind (new-label accepted rejected) (apply-condition (first workflow) rating)
      (if accepted
	  (cons (cons new-label accepted)
		(apply-workflow (rest workflow) rejected))
	  (apply-workflow (rest workflow) rejected)))))

(defun process-state (state workflows)
  (if (symbolp (first state))
      (state-value state)
      (loop for st in (apply-workflow (gethash (first state) workflows)
				      (rest state))
	    sum (process-state st workflows))))

(defun part2 (input)
  (process-state (initial-state) (workflows-hashtable (first input))))

(assert (= 167409079868000 (part2 *input-test*)))
