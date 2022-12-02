(ql:quickload 'cl-utilities)
(ql:quickload 'cl-ppcre)

(defun parse-instruction (line)
  "Returns a list of (instruction-symbol arg1 arg2-or-nil)."
  (let ((pieces (cl-utilities:split-sequence #\Space line)))
    (list (read-from-string (first pieces))
	  (read-from-string (second pieces))
	  (cond
	    ((null (third pieces)) nil)
	    ((every #'digit-char-p (coerce (third pieces) 'list)) (parse-integer (third pieces)))
	    (t (read-from-string (third pieces)))))))

(defun parse (program)
  (mapcar #'parse-instruction program))

(defparameter *lines* (uiop:read-file-lines "24.input"))
(defparameter *input* (parse *lines*))

(defun int-to-bits (int &optional acc)
  (if (= 0 int)
      acc
      (int-to-bits
       (truncate (/ int 2))
       (cons (mod int 2) acc))))

(defun make-instruction (instruction symbol-for-next-input-bit arg1 &optional arg2)
  (ecase instruction
    (inp (values `(setf ,arg1 ,symbol-for-next-input-bit) t))
    (add `(incf ,arg1 ,arg2))
    (mul `(setf ,arg1 (* ,arg1 ,arg2)))
    (div `(setf ,arg1 (truncate (/ ,arg1 ,arg2))))
    (mod `(setf ,arg1 (mod ,arg1 ,arg2)))
    (eql `(setf ,arg1 (if (eql ,arg1 ,arg2) 1 0)))))

(defun make-program (instructions)
  ;; input bits are from high to low.
  (let* ((bits-eaten 0)
	 (generated-instructions
	   (loop for (i a1 a2) in instructions
		 for symbol-for-next-input-bit = (intern (format nil "i~a" bits-eaten))
		 for (code-str input-bit-eaten) = (multiple-value-list
						   (make-instruction
						    i
						    symbol-for-next-input-bit
						    a1
						    a2))
		 collect code-str
		 if input-bit-eaten
		   do (incf bits-eaten)))
	 (arg-list (loop for i from 0 below bits-eaten
			 collect (intern (format nil "i~a" i)))))
    `(lambda (,@arg-list)
       (declare (type fixnum ,@arg-list))
       (declare (optimize (speed 3) (safety 0)))
       (let ((w 0) (x 0) (y 0) (z 0))
	 (declare (type fixnum w x y z))
	 ,@generated-instructions
	 (list w x y z)))))

(defparameter *program-code* (make-program *input*))
(defparameter *program-fun* (eval *program-code*))

(defparameter *start-model-value* (make-list 14 :initial-element 1))
;; decrements the first bit first.
(defun next-test-value (current-value)
  (when (null current-value) (error "up to max!"))
  (if (= (first current-value) 9)
      (cons 1 (next-test-value (rest current-value)))
      (cons (1+ (first current-value)) (rest current-value))))

(defun part1-compile (&optional (current-test-value *start-model-value*))
  (if (zerop (fourth (apply #'funcall *program-fun* (reverse current-test-value))))
      current-test-value
      (part1 (next-test-value current-test-value))))

;; tests
(defun build-program-from-string (string)
  (eval (make-program (parse (cl-utilities:split-sequence #\linefeed string)))))

(defparameter *test-negates-into-x*
  (build-program-from-string "inp x
mul x -1"))
(assert (equal (funcall *test-negates-into-x* 1) '(0 -1 0 0)))
(assert (equal (funcall *test-negates-into-x* -5) '(0 5 0 0)))

(defparameter *test-three-times-larger*
  (build-program-from-string "inp z
inp x
mul z 3
eql z x"))
(assert (= (fourth (funcall *test-three-times-larger* 2 6)) 1))
(assert (= (fourth (funcall *test-three-times-larger* 3 6)) 0))

(defparameter *test-to-binary*
  (build-program-from-string "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"))
(assert (equal (funcall *test-to-binary* 3) '(0 0 1 1)))
(assert (equal (funcall *test-to-binary* 9) '(1 0 0 1)))
(assert (equal (funcall *test-to-binary* 16) '(0 0 0 0)))

;; my wonderful approach above did not work.
;; looking at the code, there's repeated bits that do the following transformation:
(defun block-transform (param1 param2 param3)
  (lambda (input-code-digit z)
    (let* ((x (if (= input-code-digit (+ param2 (floor (mod z 26))))
		  0 1))
	   (y (* x (+ input-code-digit param3)))
	   (z (+ y
		 (floor (/ (* (1+ (* x 25))
			      z)
			   param1)))))
      z)))

(defun parse-block (lines)
  "Returns a blocks' 3 parameters.block-transform"
  (let ((line-idx -1) param1 param2 param3)
    (flet ((next-line () (nth (incf line-idx) lines)))
      (flet ((lineq (val) (assert (equal val (next-line)))))
	(lineq "inp w")
	(lineq "mul x 0")
	(lineq "add x z")
	(lineq "mod x 26")
	;; div z ...
	(setf param1
	      (parse-integer (third (cl-utilities:split-sequence #\Space (next-line)))))
	;; add x ...
	(setf param2
	      (parse-integer (third (cl-utilities:split-sequence #\Space (next-line)))))
	(lineq "eql x w")
	(lineq "eql x 0")
	(lineq "mul y 0")
	(lineq "add y 25")
	(lineq "mul y x")
	(lineq "add y 1")
	(lineq "mul z y")
	(lineq "mul y 0")
	(lineq "add y w")
	;; add y ...
	(setf param3
	      (parse-integer (third (cl-utilities:split-sequence #\Space (next-line)))))
	(lineq "mul y x")
	(lineq "add z y"))
      (list param1 param2 param3))))

(defun get-program-parameters (lines)
  (loop for l on lines by (lambda (l) (nthcdr 18 l))
	collect (parse-block l)))
  
(defun parse-program (lines)
  (mapcar (lambda (params) (apply #'block-transform params))
	  (get-program-parameters lines)))
  
(defun decode (lines input-bits)
  (loop with z = 0
	for transform in (parse-program *lines*)
	for bit in input-bits
	do (setf z (funcall transform bit z))
	finally (return z)))

(defun part1 (&optional (current-test-value *start-model-value*))
  (if (zerop (decode (reverse current-test-value) *lines*))
      current-test-value
      (part1 (next-test-value current-test-value))))

;; in the end I solved it with pen & paper.
