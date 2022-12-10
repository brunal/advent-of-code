(require :uiop)

(defparameter *input* (uiop:read-file-lines "10.input"))
(defparameter *input-test0* (uiop:split-string "noop
addx 3
addx -5" :separator (string #\newline)))

(defparameter *input-test1* (uiop:split-string "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop" :separator (string #\newline)))

;; Cons cell: value, cycle
(defun init-machine ()
  (cons 1 0))

(defun get-x (machine) (car machine))
(defun get-cycle (machine) (cdr machine))

(defun process-instruction (machine instruction)
  (let ((tokens (uiop:split-string instruction :separator " ")))
    (if (string= "noop" (first tokens))
	(cons (get-x machine) (1+ (get-cycle machine)))
	(cons (+ (get-x machine) (parse-integer (second tokens))) (+ 2 (get-cycle machine))))))
  
(defun process-instructions (machine instructions)
  (if (null instructions)
      ;; note: we don't actually record the end state! that helps with part2.
      nil
      (cons machine
	    (process-instructions
	     (process-instruction machine (first instructions))
	     (rest instructions)))))

(defun get-list-of-machine-states (instructions)
  (process-instructions (init-machine) instructions))

(defun n-times (value times)
  (loop for i from 0 below times collect value))

(defun machines-to-list-of-values (machines)
  (nconc
   (loop for (m mm) on machines
	 when mm
	   append (n-times (get-x m) (- (get-cycle mm) (get-cycle m))))
   (list (get-x (first (last machines))))))

(defun get-signal-strengths (list-of-values)
  (loop for cycle in '(20 60 100 140 180 220)
	when (> (length list-of-values) cycle)
	  sum (* cycle (nth (1- cycle) list-of-values))))

(defun part1 (&optional (input *input*))
  (get-signal-strengths
   (machines-to-list-of-values
    (get-list-of-machine-states input))))

(print (part1))

(defun draw (chars)
  (unless (null chars)
    (print (coerce (subseq chars 0 40) 'string))
    (draw (subseq chars 40))))

(defun part2 (&optional (input *input*))
  (let ((values (machines-to-list-of-values (get-list-of-machine-states input))))
    (draw
     (loop for i from 0 below (length values)
	   for v in values
	   collect (if (<= (abs (- v (mod i 40))) 1) #\# #\.)))))

(part2)
