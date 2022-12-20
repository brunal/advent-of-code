(require :uiop)

(defun parse-input (input) (mapcar #'parse-integer input))

(defparameter *input* (parse-input (uiop:read-file-lines "20.input")))
(defparameter *input-test* (parse-input (uiop:split-string "1 2 -3 3 -2 0 4" :separator (string #\space))))

(defstruct node
  value
  original-index
  (original-next nil)
  (next nil)
  (previous nil))

(defun bind (prev next)
  "Bind prev before next."
  (setf (node-next prev) next)
  (setf (node-previous next) prev)
  nil)

(defun lift-list (list)
  "Turns a list of int into a node."
  (let* ((lifted
	   (loop with previous = nil
		 for l on list
		 for (head next) = l
		 for index from 0
		 for new-node = (make-node
				 :value head
				 :original-index index)
		 collect new-node
		 if previous
		   do (bind previous new-node)
		   and do (setf (node-original-next previous) new-node)
		 do (setf previous new-node)))
	 (first (first lifted))
	 (last (first (last lifted))))
    ;; weave last & first node
    (bind last first)
    (setf (node-original-next last) first)))

(defun remove-node (node)
  (bind (node-previous node) (node-next node)))

(defun insert-after (node after-this-node)
  "Put node after after-this-node."
  (let ((next (node-next after-this-node)))
    (bind after-this-node node)
    (bind node next)))

(defun insert-before (node before-this-node)
  "Put node before before-this-node."
  (let ((previous (node-previous before-this-node)))
    (bind previous node)
    (bind node before-this-node)))

(defun flatten (node &optional sentinel)
  "Returns the equivalent list starting at node."
  (if (eq sentinel node)
      nil
      (cons
       (node-value node)
       (flatten (node-next node)
		(or sentinel node)))))

(assert (equal *input-test* (flatten (lift-list *input-test*))))

(defun move (node list-size &optional sentinel)
  "Moves all nodes by their value, starting at node."
  (cond
    ((eq sentinel node) nil)
    ((zerop (mod (node-value node) (1- list-size))) (move (node-original-next node) list-size (or sentinel node)))
    ;; move node by its value
    (t
     (remove-node node)
     (if (< 0 (node-value node))
	 ;; move forward
	 (insert-after node
		       (loop with n = node
			     for i below (mod (node-value node) (1- list-size))
			     do (setf n (node-next n))
			     finally (return n)))
	 ;; move backward
	 (insert-before node
			(loop with n = node
			      for i from 0 above (- (mod (node-value node) (1- list-size)) (1- list-size))
			      do (setf n (node-previous n))
			      finally (return n))))
     (move (node-original-next node) list-size (or sentinel node)))))

(setq *print-circle* t)

(defun score (list)
  (let ((zero (position 0 list))
	(size (length list)))
    (+ (nth (mod (+ zero 1000) size) list)
       (nth (mod (+ zero 2000) size) list)
       (nth (mod (+ zero 3000) size) list))))

(defun part1 (&optional (input *input*))
  (let ((node (lift-list input))
	(size (length input)))
    (move node size)
    (score (flatten node))))

(assert (= (part1 *input-test*) 3))
(assert (= (part1) 19559))

(defparameter *decryption-key* 811589153)

(defun part2 (&optional (input *input*))
  (let* ((input2 (loop for i in input collect (* i *decryption-key*)))
	 (size (length input))
	 (node (lift-list input2)))
    (loop for i below 10 do (move node size))
    (score (flatten node))))

(assert (= (part2 *input-test*) 1623178306))
