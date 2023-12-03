(require :uiop)
(require :cl-ppcre)

(defparameter *max-ball-count*
  '(("red" . 12)
    ("green" . 13)
    ("blue" . 14)))

(defparameter *input* (uiop:read-file-lines "02.input"))
(defparameter *input-test* (uiop:split-string
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  :separator (string #\newline)))

(defun game-possible? (line)
  ;; Look for "$n $col", check if any of them is above the limit
  (let ((ok t))
    (cl-ppcre:do-register-groups (count-str color)
	(" (\\d+) (red|green|blue)" line)
      (let ((count (parse-integer count-str)))
	(when (> count (cdr (assoc color *max-ball-count* :test #'string=)))
	    (setf ok nil)
	    (return))))
  ok))

(defun part1 (input)
  (loop for line in input
	for i from 1
	if (game-possible? line)
	  sum i))

(print (part1 *input*))

(defun minimum-balls-needed (line)
  ;; Returns an alist of (color . min-needed)
  (let ((mins (loop for (color . max) in *max-ball-count* collect (cons color 0))))
    (cl-ppcre:do-register-groups (count-str color)
	(" (\\d+) (red|green|blue)" line)
      (let ((count (parse-integer count-str))
	    (pair (assoc color mins :test #'string=)))
	(setf (cdr pair) (max (cdr pair) count))))
    mins))

(defun cubes-power (cubes-alist)
  (apply #'* (loop for (color . count) in cubes-alist collect count)))

(defun part2 (input)
  (loop for game in input
	sum (cubes-power (minimum-balls-needed game))))
  
(print (part2 *input*))
