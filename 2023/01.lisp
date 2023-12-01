(defparameter *raw-input* (uiop:read-file-lines "01.input"))
(defparameter *input*
  (mapcar (lambda (string) (coerce string 'list)) *raw-input*))

(defun extract-digits (line)
  (remove nil
	  (mapcar #'digit-char-p
		  line)))

(defun digits-value (digits)
  (+ (* 10 (first digits)) (car (last digits))))

(defun part1 (input)
  (apply #'+
	 (mapcar #'digits-value
		 (mapcar #'extract-digits
			 input))))

(print (part1 *input*))

(defparameter *digit-to-word*
  '((1 . "one")
    (2 . "two")
    (3 . "three")
    (4 . "four")
    (5 . "five")
    (6 . "six")
    (7 . "seven")
    (8 . "eight")
    (9 . "nine")))

(defparameter *word-list-to-digit-char*
  (loop for (digit . word) in *digit-to-word*
	collect (cons (coerce word 'list)
		      (code-char (+ digit (char-code #\0))))))

(defun maybe-replace-prefix (list prefix replacement)
  ;; If `list` starts with `prefix`, then replace the 1st item of
  ;; `list` with `replacement` (not replacing all = correct handling
  ;; of overlapping words).
  (when (and (<= (length prefix) (length list))
	     (equal (subseq list 0 (length prefix)) prefix))
    (cons replacement (rest list))))

(defun maybe-replace-any-prefix (list)
  (or
   (loop for (prefix . replacement) in *word-list-to-digit-char*
	 for result = (maybe-replace-prefix list prefix replacement)
	 if result
	   return result)
   list))

(defun replace-digit-words (list &optional acc)
  ;; replaces subsequences of `list` that match any item in *word-list-to-digit-char*
  ;; with its replacements.
  (if (null list)
      (reverse acc)
      (let ((updated (maybe-replace-any-prefix list)))
	(replace-digit-words
	 (rest updated)
	 (cons (first updated) acc)))))

(defun part2 (input)
  (part1 (mapcar #'replace-digit-words input)))

(print (part2 *input*))

(defparameter *raw-example-input*
  '("two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"))
(defparameter *example-input*
  (mapcar (lambda (string) (coerce string 'list)) *raw-example-input*))
(assert (= (part2 *example-input*) 281))
