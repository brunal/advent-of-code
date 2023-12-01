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
  (mapcar
   (lambda (digit-and-word)
     (cons (coerce (cdr digit-and-word) 'list)
	   (code-char (+ (char-code #\0) (car digit-and-word)))))
   *digit-to-word*))

;; NOTE: subsequences can overlap and should still be all replaced, i.e.
;; "twone" -> "21", and not "2ne".
;; We fix this by keeping the last char of `prefix` around...
(defun maybe-replace-prefix (list prefix replacement)
  (if (and (<= (length prefix) (length list))
	   (equal (subseq list 0 (length prefix)) prefix))
      (cons replacement
	    ;; Here is the hack.
	    (cons (first (last prefix))
		  (subseq list (length prefix))))
      nil))

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

(defun digits-or-words-only (line)
    ;; replace words with their digit in our input, then call digits-only on it
  (let* ((replaced (replace-digit-words (coerce line 'list)))
	 (digits (digits-only replaced)))
    (format t "~A -> ~A -> ~A~%" line (coerce replaced 'string) digits)
    digits))

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
