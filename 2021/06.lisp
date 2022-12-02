(ql:quickload "cl-utilities")

(defun parse-input (input)
  (mapcar #'parse-integer
	  (cl-utilities:split-sequence #\, input)))

(defparameter *input*
  (parse-input (first (uiop:read-file-lines "06.input"))))

(defparameter *test-input*
  (parse-input "3,4,3,1,2"))

;; The first functions here work for part1. They represent the fishes naively, as a tree.
(defun grow-one-fish (fish-value)
  (if (eql 0 fish-value) '(6 8) (1- fish-value)))

(defun grow-all-fishes-once (population)
  (mapcar
   (lambda (pop)
     (if (consp pop)
	 (grow-all-fishes-once pop)
	 (grow-one-fish pop)))
   population))

(defun grow-all-fishes-over-time (population days)
  (dotimes (d days)
    (setf population (grow-all-fishes-once population)))
  population)

(defun count-fishes (population)
  (cond
    ((consp population) (+ (count-fishes (car population)) (count-fishes (cdr population))))
    (population 1)
    (t 0)))

(defun grow-and-count (input days)
  (count-fishes
   (grow-all-fishes-over-time input days)))

(defun part1 (input)
  (grow-and-count input 80))

(print (part1 *input*))

;; For part2, we need to adapt our representation. Instead of a naive "represent all fish",
;; keep a list of count of fish from each day.
;; For legacy, I'm keeping the part1 solution above.
;; Functions are similarly named, sorry.

(defun make-fishes (list-of-fishes &optional (value 0))
  (if (< value 9)
      (cons
       ;; count fishes that are `value`.
       (loop for fish in list-of-fishes
	     count (eql fish value))
       (make-fishes-helper list-of-fishes (1+ value)))
      nil))

(defun add-lists (l1 l2)
  "Sums 2 lists element-wise. l1 must be longer than l2."
  (if l1
      (cons (+ (car l1) (or (car l2) 0))
	    (add-lists (cdr l1) (cdr l2)))
      nil))

(defun grow-fishes (fish-bank)
  "1 day of growth."
  (add-lists
   (list 0 0 0 0 0 0 (car fish-bank) 0 (car fish-bank))
   (cdr fish-bank)))

(defun grow-fishes-over-time (fish-bank days)
  (dotimes (d days)
    (setf fish-bank (grow-fishes fish-bank)))
  fish-bank)

(defun count-fishes (fish-bank)
  (apply #'+ fish-bank))

(defun part2 (input)
  (count-fishes (grow-fishes-over-time (make-fishes input) 256)))

(print (part2 *input*))
