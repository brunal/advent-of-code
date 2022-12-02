(ql:quickload 'cl-utilities)
(ql:quickload 'alexandria)

(defun parse-input (lines)
  (mapcar
   (lambda (line)
     (mapcar
      #'read-from-string
      (cl-utilities:split-sequence #\SPACE line)))
   lines))

(defparameter *input* (parse-input (uiop:read-file-lines "02.input")))

(defun inner-value (shape)
  (case shape
    (X 1)
    (Y 2)
    (Z 3)))

(defun score (round)
  (+ (inner-value (cadr round))
     (alexandria:switch (round :test equal)
       ;; A X Rock
       ;; B Y Paper
       ;; C Z Scissors
       ;; 0 loss, 3 draw, 6 win
       ('(A X) 3)
       ('(A Y) 6)
       ('(A Z) 0)
       ('(B X) 0)
       ('(B Y) 3)
       ('(B Z) 6)
       ('(C X) 6)
       ('(C Y) 0)
       ('(C Z) 3))))

(defun part1 (input)
  (reduce #'+ (mapcar #'score input)))

(print (part1 *input*))

(defun choose-shape (round)
  (alexandria:switch (round :test equal)
    ('(A X) 'Z)
    ('(A Y) 'X)
    ('(A Z) 'Y)
    ('(B X) 'X)
    ('(B Y) 'Y)
    ('(B Z) 'Z)
    ('(C X) 'Y)
    ('(C Y) 'Z)
    ('(C Z) 'X)))
  
(defun part2-score (round)
  (let ((shape (choose-shape round)))
    (+ (inner-value shape)
       (case (cadr round)
	 (X 0)
	 (Y 3)
	 (Z 6)))))

(defun part2 (input)
  (reduce #'+ (mapcar #'part2-score input)))

(print (part2 *input*))
