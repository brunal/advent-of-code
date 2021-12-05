(defun prepare-input (input)
  "Turns the input list of lines into a list of char vector."
  (mapcar (lambda (line) (coerce (coerce line 'list) 'vector))
  input))

(defparameter *input*
  (prepare-input (uiop:read-file-lines "03.input")))

(defparameter *input-test*
  (prepare-input (uiop:read-file-lines "03.input.test")))


(defun get-nth-bit (input n)
  "Returns the n-th bit of each (vector) element of input."
  (mapcar (lambda (vec) (aref vec n)) input))

(defun most-common-bit (bits)
  (loop
    with zeroes = 0
    with ones = 0
    for bit in bits
    do
       (ccase bit
	 (#\0 (incf zeroes 1))
	 (#\1 (incf ones 1)))
    finally (return (cond
		      ((> zeroes ones) 0)
		      ((< zeroes ones) 1)
		      (t (error "as many zeroes as ones"))))))

(defun opposite-bits (bits)
  (mapcar (lambda (bit) (- 1 bit)) bits))

(defun to-integer (bits)
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bits))

(defun make-result (bits)
  (let* ((num (to-integer bits))
	 (opposite-num (to-integer (opposite-bits bits))))
    (* num opposite-num)))

(defun part1 (input)
  (make-result
  (loop
    for i from 0 below (length (car input))
    collect (most-common-bit (get-nth-bit input i)))))

(print (part1 *input*))


(defun part2-helper (input index choice-function)
  (if (eql 1 (length input))
      (car input)
      ; filter more!
      (let ((bit (funcall choice-function (get-nth-bit input index))))
        (part2-helper
	 (remove-if-not (lambda (line) (eq bit (aref line index))) input)
	 (1+ index)
	 choice-function))))

(defun most-common-bit-or-1 (bits)
  (loop
    with zeroes = 0
    with ones = 0
    for bit in bits
    do
       (ccase bit
	 (#\0 (incf zeroes 1))
	 (#\1 (incf ones 1)))
       finally (return (if (> zeroes ones) #\0 #\1))))

(defun least-common-bit-or-0 (bits)
  (case (most-common-bit-or-1 bits)
    (#\0 #\1) (#\1 #\0)))

(defun char-vec-to-integer (char-vec)
  (to-integer (mapcar #'digit-char-p (coerce char-vec 'list))))
  
(defun part2 (input)
  (* (char-vec-to-integer (part2-helper input 0 #'most-common-bit-or-1))
    (char-vec-to-integer (part2-helper input 0 #'least-common-bit-or-0))))

(print (part2 *input*))
