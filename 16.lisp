(defparameter *input* (first (uiop:read-file-lines "16.input")))
(defparameter *example1* "D2FE28")
(defparameter *example2* "38006F45291200")
(defparameter *example3* "EE00D40C823060")

(defun word-to-bit-stream (hexa-word)
  (loop for letter in (coerce hexa-word 'list)
	for value = (parse-integer (string letter) :radix 16)
	nconc (loop for bit-id from 3 downto 0
		    collect (ldb (byte 1 bit-id) value))))

(defun bit-stream-to-int (bit-stream &optional bit-count (value 0))
  (cond
    ((null bit-stream) (values nil value))
    ((and bit-count (zerop bit-count)) (values bit-stream value))
    (t (bit-stream-to-int
	(rest bit-stream)
	(if (null bit-count) nil (1- bit-count))
	(+ (* 2 value) (first bit-stream))))))

(defun bit-stream-eater (bit-list)
  (let* ((current-position bit-list)
	 (eater
	   (lambda (bit-count)
	     (when (null current-position)
	       (error "nothing left to read"))
	     (multiple-value-bind (new-position value)
		 (bit-stream-to-int current-position bit-count)
	       (setf current-position new-position)
	       value))))
    eater))

(defun eat-literal-value (next-bits-to-int &optional (total-value 0) (bits-eaten-count 0))
  "Returns (values literal #-of-bits-eaten)."
  (let* ((zero-if-last (funcall next-bits-to-int 1))
	 (value (funcall next-bits-to-int 4))
	 (new-value (+ value (* 16 total-value)))
	 (new-bits-eaten (+ bits-eaten-count 5)))
    (if (zerop zero-if-last)
	(values new-value new-bits-eaten)
	(eat-literal-value next-bits-to-int new-value new-bits-eaten))))

(defun eat-subpackets-by-count (next-bits-to-int subpacket-count)
  (loop for i from 0 below subpacket-count
	collect (eat-packet next-bits-to-int)))

(defun eat-subpackets-by-size (next-bits-to-int size)
  (unless (zerop size)
    (let ((subpacket (eat-packet next-bits-to-int)))
      (cons
       subpacket
       (eat-subpackets-by-size
	next-bits-to-int
	(- size (getf subpacket :size)))))))

(defun eat-packet (next-bits-to-int)
  (let* ((packet-version (funcall next-bits-to-int 3))
	 (packet-type-id (funcall next-bits-to-int 3)))
    (multiple-value-bind
	  (packet bec)
	(if (= 4 packet-type-id)
	    (eat-literal-value next-bits-to-int)
	    (progn
	      (let* ((by-size-p (zerop (funcall next-bits-to-int 1)))
		     (next-word-size (if by-size-p 15 11))
		     (subpacket-func (if by-size-p #'eat-subpackets-by-size #'eat-subpackets-by-count))
		     (subpackets (funcall subpacket-func
					  next-bits-to-int
					  (funcall next-bits-to-int next-word-size))))
		(values
		 subpackets
		 (+ (loop for p in subpackets sum (getf p :size))
		    next-word-size 1)))))
      (list
       :version packet-version
       :type-id packet-type-id
       :size (+ bec 3 3)
       :payload packet))))

(defun word-to-packet (hexa-word)
  (eat-packet
   (bit-stream-eater
    (word-to-bit-stream hexa-word))))

(defun sum-packet-versions (packet)
  (+ (getf packet :version)
     (let ((payload (getf packet :payload)))
       (if (numberp payload)
	   0
	   (apply #'+ (mapcar #'sum-packet-versions payload))))))

(defun part1 (&optional (hexa *input*))
  (sum-packet-versions (word-to-packet hexa)))
(assert (= (part1 "8A004A801A8002F478") 16))
(assert (= (part1 "620080001611562C8802118E34") 12))
(assert (= (part1 "C0015000016115A2E0802F182340") 23))
(assert (= (part1 "A0016C880162017C3686B18A3D4780") 31))

(print (part1))

(defun binop-to-int-bool (binop)
  (lambda (&rest numbers) (if (apply binop numbers) 1 0)))

(defun operator (value)
  (ccase value
    (0 #'+)
    (1 #'*)
    (2 #'min)
    (3 #'max)
    (4 nil)
    (5 (binop-to-int-bool #'>))
    (6 (binop-to-int-bool #'<))
    (7 (binop-to-int-bool #'=))))


(defun eval-packet (packet)
  (let ((op (operator (getf packet :type-id))))
    (if (null op)
	(getf packet :payload)
	(apply op (mapcar #'eval-packet (getf packet :payload))))))

(defun part2 (&optional (hexa *input*))
  (eval-packet (word-to-packet hexa)))

(print (part2))
