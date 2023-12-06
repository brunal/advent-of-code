(require :uiop)

(defparameter *input* (uiop:read-file-lines "05.input"))
(defparameter *input-test* (uiop:read-file-lines "05.input.test"))

(defun parse-input (input)
  ;;Turns the input into a list of 
  ;; * a list of # (seeds)
  ;; * maps: each is a list of (start dest length)
  (let ((seeds (mapcar #'parse-integer
		       (cdr (uiop:split-string (first input) :separator (string #\Space))))))
    (cons seeds (parse-map-blocks (rest (rest input))))))

(defun parse-map-blocks (input)
  (if (null input)
      nil
      (multiple-value-bind (block rest)
	  (parse-map-block input)
	(cons
	 (sort block #'< :key #'first)
	 (parse-map-blocks rest)))))

(defun parse-map-block (input)
  ;; Starting with a "x-to-y map:" line, parse the map & returns the
  ;; input after the map parsed.
   (loop for lines on (cdr input)
	 for (line . rest) = lines
	 for line-split = (uiop:split-string line :separator (string #\Space))
	 if (null line-split)
	   return (values map rest)
	 collect (mapcar #'parse-integer line-split) into map
	 ;; handle EOF
	 finally (return (values map nil))))


(defun lookup-map (value map)
  (loop for (dest-range-start source-range-start range-length) in map
	if (<= source-range-start value (+ source-range-start range-length))
	  return (+ dest-range-start (- value source-range-start))
	finally (return value)))

(defun lookup-maps (value maps)
  (if (null maps)
      value
      (lookup-maps
       (lookup-map value (first maps))
       (rest maps))))

(defun part1 (input)
  (loop with (seeds . maps) = (parse-input input)
	for seed in seeds
	minimize (lookup-maps seed maps)))

(print (part1 *input*))

(defun part2-fail (input)
  ;; This would take way too long to compute.
  (loop with (seeds . maps) = (parse-input input)
	for (seed-start range . _) on seeds by #'cddr
	minimize (loop for seed from seed-start below (+ seed-start range)
		       minimize (lookup-maps seed maps))))

;; Instead of manipulating numbers, manipulate ranges.

(defun intersect-p (x1 y1 x2 y2)
  (let ((xx (max x1 x2))
	(yy (min y1 y2)))
    (when (< xx yy)
      (list xx yy))))
	
(defun apply-map-to-range (range map)
  ;; range: (start end)
  ;; map: list of (start end offset) sorted by start
  ;; returns a list of (start end) range, so that every point of the
  ;; input is in the output. Either natural or translated by a map.
  ;; Logic:
  ;; * advance to the first line in `map` that intersects range
  ;; * yield natural [start .... beginning of intersection]
  ;; * yield intersection, translated
  ;; * recurse with the end of the range & of the map
  (cond
    ((>= (first range) (second range)) nil)
    ((null map) (list range))
    (t (loop with (range-start range-end) = range
	     for (map-start map-end offset) in map
	     for remaining-map-lines on map
	     for intersection = (intersect-p range-start range-end
					     map-start map-end)
	     if intersection
	       ;; natural from range-start until intersection-start
	       ;; +offset from intersection-start until intersection-end
	       ;; then resume at (min range-end intersection-end)
	       return (let* ((intersection-start (first intersection))
			     (intersection-end (second intersection))
			     (offset-range (list (+ offset intersection-start) (+ offset intersection-end)))
			     (recursion (apply-map-to-range (list (min range-end intersection-end) range-end)
							    remaining-map-lines)))
			(if (< range-start intersection-start)
			    (list* (list range-start intersection-start)
				   offset-range
				   recursion)
			    (list* offset-range recursion)))
	     finally (return (list range))))))

(defun apply-map-to-ranges (ranges map)
  (apply #'append
	 (mapcar (lambda (range) (apply-map-to-range range map))
		 ranges)))

(defun part2 (input)
  (let* ((parsed (parse-input input))
	 ;; ranges = list of [start, end[
	 (start-ranges (sort (loop for (start length . _) on (first parsed) by #'cddr
				   collect (list start (+ start length)))
			     #'< :key #'first))
	 ;; maps = list of list of [start, end, offset[
	 (maps (mapcar
		(lambda (map)
		  (sort (mapcar
			 (lambda (list)
			   (destructuring-bind (dest-start source-start length) list
			     (list source-start (+ source-start length) (- dest-start source-start))))
			 map)
			#'< :key #'first))
		(rest parsed))))
    (apply #'min
	   (mapcar #'first 
		   (reduce #'apply-map-to-ranges maps :initial-value start-ranges)))))

(print (part2 *input*))
