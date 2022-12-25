(require :uiop)
(require :alexandria)

(defparameter *input* (uiop:read-file-lines "25.input"))
(defparameter *input-test* (uiop:split-string "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122" :separator (string #\newline)))

(defparameter *sums* nil "List of (d1 . d2 . result-list-lowest-first)")

(defmacro def-sum (d1 d2 &rest sum)
  `(progn
    (push (list ,d1 ,d2 ,@sum) *sums*)
    (push (list ,d2 ,d1 ,@sum) *sums*)))

(def-sum #\2 #\= #\0)
(def-sum #\2 #\- #\1)
(def-sum #\2 #\0 #\2)
(def-sum #\2 #\1 #\= #\1)
(def-sum #\2 #\2 #\- #\1)

(def-sum #\1 #\= #\-)
(def-sum #\1 #\- #\0)
(def-sum #\1 #\0 #\1)
(def-sum #\1 #\1 #\2)

(def-sum #\0 #\= #\=)
(def-sum #\0 #\- #\-)
(def-sum #\0 #\0 #\0)

(def-sum #\- #\= #\2 #\-)
(def-sum #\- #\- #\=)

(def-sum #\= #\= #\1 #\-)

(defun get-sum (d1 d2)
  (loop for (dd1 . rest) in *sums*
	for (dd2 . sum) = rest
	if (and (char= d1 dd1) (char= d2 dd2))
	  do (return sum)))

(defun sum-snafu-lists (n1 n2)
  (cond ((null n1) n2)
	((null n2) n1)
	(t (let ((digits-sum (get-sum (first n1) (first n2))))
	     (cons (first digits-sum)
		   (sum-snafu-lists (sum-snafu-lists (rest digits-sum) (rest n1))
				    (rest n2)))))))

(defun sum-snafu (n1 n2)
  (coerce 
   (reverse
    (sum-snafu-lists (reverse (coerce n1 'list))
		     (reverse (coerce n2 'list))))
   'string))

(assert (string= (sum-snafu "12" "2=") "1=0"))
(assert (string= (sum-snafu "20" "20") "1-0"))

(defun part1 (&optional (input *input*))
  (reduce #'sum-snafu input))

(assert (string= (part1 *input-test*) "2=-1=0"))
