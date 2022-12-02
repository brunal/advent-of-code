(ql:quickload 'cl-utilities)

(defun parse-input (line)
  (flet ((parse-display-list (display-list)
	   (cl-utilities:split-sequence #\Space display-list :remove-empty-subseqs t)))
    (destructuring-bind (left right) (cl-utilities:split-sequence #\| line)
      (cons (parse-display-list left) (parse-display-list right)))))

;; *input* is a list of problems
(defparameter *input* (mapcar #'parse-input (uiop:read-file-lines "08.input")))

(defparameter *one-test-input*
  (parse-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

(defparameter *test-input* (mapcar #'parse-input (uiop:read-file-lines "08.input.test")))


(defun part1 (input)
  ;; count the 1 4 7 8. they have respectively 2 4 3 7 segments.
  (loop for line in input
	sum (loop for output-value in (cdr line)
		  count (member (length output-value) '(2 4 3 7)))))

(print (part1 *input*))

;; each display is a string with letters a-g representing which segments of the display are on.
;;  aa 
;; b  c
;; b  c
;;  dd
;; e  f
;; e  f
;;  gg

;; Yes, I could just process it once instead of over and over.
(defun find-displays-by-segment-count (displays segment-count)
  (loop for display in displays
	if (= (length display) segment-count) collect display))

(defun superset (d dd)
  "Returns whether chars of d are a superset of chars of dd"
  (cond
    ((not dd) t)
    ((not d) nil)
    ;; match
    ((char= (car d) (car dd)) (superset (cdr d) (cdr dd)))
    ;; skip to next one
    ((char< (car d) (car dd)) (superset (cdr d) dd))
    ((char> (car d) (car dd)) nil)))


(defun superset-string (d dd)
  (superset
   (sort (coerce d 'list) #'char<)
   (sort (coerce dd 'list) #'char<)))

(defun segments-shared-count (d dd)
  "Counts the # of segments in common between d and dd."
  (length (intersection (coerce d 'list) (coerce dd 'list))))

;; 5-segments displays are 2, 3, 5.
;; * Only 3 includes segments from 1. 
;; * Only 5 shares 3 segments with 4
;; * Last is 2
(defun categorize-5-segments (d-5-segments d1 d4)
  "Returns (d2 d3 d5)."
  (let* (
	 ;; d3 is a superset of d1
	 (d3 (find-if (lambda (d) (superset-string d d1)) d-5-segments))
	 (d-5-segments-without-d3 (remove d3 d-5-segments :test #'string=))
	 ;; d5 shares 3 segments with d4
	 (d5 (find-if (lambda (d) (= (segments-shared-count d d4) 3)) d-5-segments-without-d3))
	 ;; d2 remains
	 (d2 (car (remove d5 d-5-segments-without-d3 :test #'string=))))
    (list d2 d3 d5)))

;; 6-segments displays are 0, 6, 9.
;; * 4 (or 2) is included in 9
;; * 7 is included in 0
;; * Last is 6
(defun categorize-6-segments (d-6-segments d4 d7)
  "Returns (d0 d6 d9)."
  (let* (
	 ;; d9 is a superset of d4
	 (d9 (find-if (lambda (d) (superset-string d d4)) d-6-segments))
	 (d-6-segments-without-d9 (remove d9 d-6-segments :test #'string=))
	 ;; d0 is a superset of d7
	 (d0 (find-if (lambda (d) (superset-string d d7)) d-6-segments-without-d9))
	 ;; d6 remains
	 (d6 (car (remove d0 d-6-segments-without-d9 :test #'string=))))
    (list d0 d6 d9)))

(defun guess-combination (displays)
  "Returns the 10 input displays sorted: first one is 0, then 1, etc."
  (let ((d1 (first (find-displays-by-segment-count displays 2)))
	(d4 (first (find-displays-by-segment-count displays 4)))
	(d7 (first (find-displays-by-segment-count displays 3)))
	(d8 (first (find-displays-by-segment-count displays 7)))
	(d-5-segments (find-displays-by-segment-count displays 5))
	(d-6-segments (find-displays-by-segment-count displays 6)))
    (destructuring-bind (d2 d3 d5) (categorize-5-segments d-5-segments d1 d4)
      (destructuring-bind (d0 d6 d9) (categorize-6-segments d-6-segments d4 d7)
	(mapcar (lambda (d) (sort d #'char<))
		(list d0 d1 d2 d3 d4 d5 d6 d7 d8 d9))))))
  
(defun to-number (list)
  (labels ((f (list)
	     (if list
		 (+ (car list)
		    (* 10 (f (cdr list))))
		 0)))
    (f (nreverse list))))
(assert (= 12345 (to-number '(1 2 3 4 5))))

(defun get-number (combination displays)
  "Retuns the list of digits represented on displays."
  (to-number
   (mapcar (lambda (display) (position (sort display #'char<) combination :test #'string=))
	   displays)))

(defun guess-output-value (line)
  "Returns the number displayed on the right-hand side"
  (get-number
   (guess-combination (car line))
   (cdr line)))

(defun guess-print (line)
  (with-output-to-string (output)
    (format output "~a --> ~a~%" (cdr line) (guess-output-value line))
    output))

(defparameter *test-output*
  '(8394 9781 1197 9361 4873 8418 4548 1625 8717 4315))
(loop for ti in *test-input*
      for to in *test-output*
      do (assert (equalp (guess-output-value ti) to)
		 (ti to)
		 "~ainstead of ~a. Decoded digits were~%~a.~%"
		 (guess-print ti) to (guess-combination (car ti))))

(defun part2 (inputs)
  (apply #'+ (mapcar #'guess-output-value inputs)))


(print (part2 *input*))
