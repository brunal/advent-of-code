(require 'cl-utilities)

(defun parse-command (line)
  (let ((words (cl-utilities:split-sequence #\Space line)))
    (mapcar
     #'parse-integer
     (list (second words) (fourth words) (sixth words)))))

(defun parse-commands (lines)
  ; Discard the setup plan: we've harcoded it below.
  (loop for line in lines
	if (and (> (length line) 4)
		(string= (subseq line 0 4) "move"))
	  collect (parse-command line)))

(defparameter *input* (uiop:read-file-lines "05.input"))
(defparameter *commands* (parse-commands *input*))

(defun get-init-crates ()
  (let ((crates (make-hash-table)))
    (setf (gethash 1 crates) '(P D Q R V B H F))
    (setf (gethash 2 crates) '(V W Q Z D L))
    (setf (gethash 3 crates) '(C P R G Q Z L H))
    (setf (gethash 4 crates) '(B V J F H D R))
    (setf (gethash 5 crates) '(C L W Z))
    (setf (gethash 6 crates) '(M V G T N P R J))
    (setf (gethash 7 crates) '(S B M V L R J))
    (setf (gethash 8 crates) '(J P D))
    (setf (gethash 9 crates) '(V W N C D))
    crates))

(defun apply-command-part-1 (crates count from to)
  (dotimes (i count)
    (setf (gethash to crates) (cons (car (gethash from crates)) (gethash to crates)))
    (setf (gethash from crates) (cdr (gethash from crates)))))

(defun apply-commands (apply-one-command crates commands)
  (mapcar
   (lambda (command) (apply apply-one-command crates command))
   commands))

(defun top-crates-to-string (crates)
  (apply #'concatenate 'string
	 (loop for i from 1 upto (hash-table-count crates)
	       collect (string (car (gethash i crates))))))

(defun part1 (commands)
  (let ((crates (get-init-crates)))
    (apply-commands #'apply-command-part-1 crates commands)
    (top-crates-to-string crates)))

(print (part1 *commands*))

(defun apply-command-part-2 (crates count from to)
  (setf (gethash to crates)
	(append (subseq (gethash from crates) 0 count)
		(gethash to crates)))
  (setf (gethash from crates) (subseq (gethash from crates) count)))

(defun part2 (commands)
  (let ((crates (get-init-crates)))
    (apply-commands #'apply-command-part-2 crates commands)
    (top-crates-to-string crates)))

(print (part2 *commands*))
