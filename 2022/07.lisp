(require :uiop)

(defparameter *input* (uiop:read-file-lines "07.input"))

(defun make-node ()
  (make-hash-table :test #'equal))

(defun enter-or-create-dir (node dir-name)
  (when (null (gethash dir-name node))
    (setf (gethash dir-name node) (make-node)))
  (gethash dir-name node))

(defun register-size (node file-name file-size)
  (setf (gethash file-name node) file-size))

(defun command-tokens (command)
  (uiop:split-string command :separator " "))

(defun into-dir (dir-name input current-node parent-nodes)
  (if (string= dir-name "..")
      (process-command input
		       (first parent-nodes)
		       (rest parent-nodes))
					; we should never have to create it: we ls first.
      (process-command input
		       (enter-or-create-dir current-node dir-name)
		       (cons current-node parent-nodes))))

(defun list-contents (input current-node parent-nodes)
  (unless (null input)
    (let ((tokens (command-tokens (first input))))
      (cond
	((string= "$" (first tokens)) (process-command input current-node parent-nodes))
	((string= "dir" (first tokens)) (progn
					  (enter-or-create-dir current-node (second tokens))
					  (list-contents (rest input) current-node parent-nodes)))
	(t (progn
	     (register-size current-node (second tokens) (parse-integer (first tokens)))
	     (list-contents (rest input) current-node parent-nodes)))))))

(defun process-command (input current-node parent-nodes)
  (unless (null input)
    (let ((tokens (command-tokens (first input))))
      (cond
	((string= "cd" (second tokens)) (into-dir (third tokens) (rest input) current-node parent-nodes))
	((string= "ls" (second tokens)) (list-contents (rest input) current-node parent-nodes))
	(t (error "~a is not handled" tokens))))))


(defun process-all-commands (input)
  (let ((root (make-node)))
    (process-command input root nil)
    root))

(defparameter *hierarchy* (process-all-commands *input*))

(defun node-to-listing (node &optional (indent 0))
  (let ((indent-fmt (make-string (* 2 indent) :initial-element #\Space)))
    (loop for key being the hash-keys of node
	    using (hash-value value)
	  do (if (integerp value)
		 (format t "~A- ~A (size: ~A)~%" indent-fmt key value)
		 (progn
		   (format t "~A- ~A:~%" indent-fmt key)
		   (node-to-listing value (1+ indent)))))))

(defun total-size-of (node)
  (loop for value being the hash-values of node
	sum (if (integerp value)
		value
		(total-size-of value))))

(defun augment-node-with-dir-size (node)
  "Turns every node into (size . node)."
  (cons (total-size-of node)
	(progn 
	  (maphash 
	   (lambda (key val)
	     (setf (gethash key node)
		   (if (integerp val)
		       val
		       (augment-node-with-dir-size val))))
	   node)
	  node)))

(defparameter *augmented-hierarchy* (augment-node-with-dir-size *hierarchy*))

(defun augmented-node-to-listing (node &optional (indent 0))
  (let ((indent-fmt (make-string (* 2 indent) :initial-element #\Space)))
    (loop for key being the hash-keys of (cdr node)
	    using (hash-value value)
	  do (if (integerp value)
		 (format t "~A- ~A (size: ~A)~%" indent-fmt key value)
		 (progn
		   (format t "~A- ~A (dir size: ~A):~%" indent-fmt key (car value))
		   (augmented-node-to-listing value (1+ indent)))))))

(defun sum-dir-sizes-below (augmented-node max)
  (let ((base (if (< (car augmented-node) max) (car augmented-node) 0)))
    (+ base
       (loop for value being the hash-values of (cdr augmented-node)
	     unless (integerp value)
	       sum (sum-dir-sizes-below value max)))))


(defun part1 ()
  (sum-dir-sizes-below *augmented-hierarchy* 100000))

(print (part1))

(defparameter *total-disk-size* 70000000)
(defparameter *disk-size-needed* 30000000)

(defun take-best (goal candidates)
  "Returns the smallest candidate larger than the goal."
  (let ((actual-candidates
	  (remove-if (lambda (c) (< c goal))
		     (remove nil candidates))))
    (if (null actual-candidates) nil (apply #'min actual-candidates))))

(defun smallest-dir-larger-than (node size)
  (take-best
   size
   (cons (car node)
	 (loop for value being the hash-values of (cdr node)
	       unless (integerp value)
		 collect (smallest-dir-larger-than value size)))))

(defun size-of-dir-to-delete (augmented-node &optional (total-size *total-disk-size*) (size-needed *disk-size-needed*))
  "Returns the size of the smallest dir that, if deleted, frees up enough space."
  (let* ((space-remaining (- total-size (car augmented-node)))
	 (space-missing (- size-needed space-remaining)))
    (smallest-dir-larger-than augmented-node space-missing)))

(defun part2 ()
  (size-of-dir-to-delete *augmented-hierarchy*))

(print (part2))
