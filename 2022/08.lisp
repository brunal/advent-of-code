(require :uiop)

(defun parse-input (lines)
  (mapcar
   (lambda (row)
     (mapcar #'digit-char-p (coerce row 'list)))
   lines))
  
(defparameter *input* (parse-input (uiop:read-file-lines "08.input")))

(defparameter *test-input* (parse-input
			    '("30373"
			      "25512"
			      "65332"
			      "33549"
			      "35390")))

(defun to-tree-array (input)
  (let ((lines (length input))
	(cols (length (car input))))
    (make-array (list lines cols) :initial-contents input)))
  
(defparameter *tree-array* (to-tree-array *input*))
(defparameter *test-tree-array* (to-tree-array *test-input*))

(defun make-visibility-array (&optional (trees *tree-array*))
  (let ((visible (make-array (array-dimensions trees) :initial-element nil)))
     (count-from-bottom trees
			(count-from-top trees
					(count-from-right trees
							  (count-from-left trees visible))))))
(defun count-visible (visible)
  (loop for x from 0 below (array-dimension visible 0)
	sum (loop for y from 0 below (array-dimension visible 1)
		  counting (aref visible x y))))

;; TODO: refactor...

(defun count-from-left (trees visibility)
  (loop for x from 0 below (array-dimension trees 0)
	do (loop for y from 0 below (array-dimension trees 1)
		 with max-so-far = -1
		 if (> (aref trees x y) max-so-far)
		   do (progn
			(setf (aref visibility x y) t)
			(setf max-so-far (aref trees x y)))))
  visibility)

(defun count-from-right (trees visibility)
  (loop for x from 0 below (array-dimension trees 0)
	do (loop for y from (1- (array-dimension trees 1)) downto 0
		 with max-so-far = -1
		 if (> (aref trees x y) max-so-far)
		   do (progn
			(setf (aref visibility x y) t)
			(setf max-so-far (aref trees x y)))))
  visibility)

(defun count-from-top (trees visibility)
  (loop for y from 0 below (array-dimension trees 1)
	do (loop for x from 0 below (array-dimension trees 0)
		 with max-so-far = -1
		 if (> (aref trees x y) max-so-far)
		   do (progn
			(setf (aref visibility x y) t)
			(setf max-so-far (aref trees x y)))))
  visibility)


(defun count-from-bottom (trees visibility)
  (loop for y from 0 below (array-dimension trees 1)
	do (loop for x from (1- (array-dimension trees 0)) downto 0
		 with max-so-far = -1
		 if (> (aref trees x y) max-so-far)
		   do (progn
			(setf (aref visibility x y) t)
			(setf max-so-far (aref trees x y)))))
  visibility)


(defun part1 (&optional (tree-array *tree-array*))
  (print (count-visible (make-visibility-array tree-array))))

(defun count-until-higher-than (trees x y candidates)
  (cond
    ((null candidates) 0)
    ((>= (apply #'aref trees (car candidates)) (aref trees x y)) 1)
    (t (1+ (count-until-higher-than trees x y (cdr candidates))))))

(defun scenic-score (trees x y)
  (* (count-until-higher-than trees x y (loop for x2 from (1- x) downto 0 collect (list x2 y)))
     (count-until-higher-than trees x y (loop for x2 from (1+ x) below (array-dimension trees 0) collect (list x2 y)))
     (count-until-higher-than trees x y (loop for y2 from (1- y) downto 0 collect (list x y2)))
     (count-until-higher-than trees x y (loop for y2 from (1+ y) below (array-dimension trees 1) collect (list x y2)))))

(defun best-scenic-score (trees)
  (loop for x from 0 below (array-dimension trees 0)
	maximize (loop for y from 0 below (array-dimension trees 1)
		       maximize (scenic-score trees x y))))

(defun part2 (&optional (tree-array *tree-array*))
  (print (best-scenic-score tree-array)))
