(ql:quickload 'cl-ppcre)

(defparameter *line-regex*
  "^(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)$")

(defun parse-line (line)
  (let ((bits (coerce
	       (nth-value 1 (cl-ppcre:scan-to-strings *line-regex* line))
	       'list)))
    (cons (if (string= (first bits) "on") t nil)
	  (mapcar
	   #'parse-integer
	   (rest bits)))))
(assert (equal (parse-line "on x=-20..26,y=-36..17,z=-47..7")
	       '(t . (-20 26 -36 17 -47 7))))

(defun parse (input)
  (mapcar #'parse-line input))

(defparameter *input* (parse (uiop:read-file-lines "22.input")))
(defparameter *input-test* (parse (uiop:read-file-lines "22.input.test")))

;; first part, not very interesting.

(defun within-bounds? (command)
  (every (lambda (i) (<= -50 i 50))
	 (rest command)))

(defun offset (i)
  (+ 50 i))

(defun build-box-dumb(input)
  ;; we're interested in the [-50, 50]^3 box, which has 101 elements on each side.
  (let ((box (make-array '(101 101 101) :initial-element nil)))
    (loop for command in input
	  for (action x1 x2 y1 y2 z1 z2) = command
	  if (within-bounds? command)
	    do (loop for x from x1 upto x2
		     do (loop for y from y1 upto y2
			      do (loop for z from z1 upto z2
				       do (setf (aref box (offset x) (offset y) (offset z)) action)))))
    box))

(defun count-box-dumb-on (box)
  (loop for x from -50 upto 50
	sum (loop for y from -50 upto 50
		  sum (loop for z from -50 upto 50
			    sum (if (aref box (offset x) (offset y) (offset z)) 1 0)))))

(defun part1 ()
  (count-box-dumb-on (build-box-dumb *input*)))

;; Part2: we remember the coordinates of lit boxes.
;; This is a first tentative that will end up failing.
;; When we process a new box, for each lit box:
;; * split lit box into boxes without new-box, and intersection with new-box
;; * if new box is off, just remember the split lit box
;; * if new box is on, remember the split lit box and the new-box
;; In the end, compute the size of each remembered box.
;; We represent segments with complex numbers.
;; The approach below ended up not working: heap exhaustion on *input*.
(defun segment-intersection (s1 s2)
  "Returns the segment at the intersection of s1 and s2, or nil."
  (let ((left (max (realpart s1) (realpart s2)))
	(right (min (imagpart s1) (imagpart s2))))
    (if (<= left right)
	(complex left right)
	nil)))

(defun segment-diff (s1 s2)
  "Returns (is-on . sub-segment) where union(sub-segments on) = s1 - s2. s2 is inside s1."
  (remove-if
   (lambda (state-and-segment)
     ;; FIXME make sure this is the correct bound.
     (= (realpart (cdr state-and-segment)) (imagpart (cdr state-and-segment))))
   (list
    (cons t (complex (realpart s1) (1- (realpart s2))))
    (cons nil s2)
    (cons t (complex (1- (imagpart s2)) (imagpart s1))))))

(defun actual-cube-diff (cube sub-cube)
  "Returns cubes that are in cube but not sub-cube. Sub-cube is 100% inside cube."
  ;; we return up to 26 cubes, when 6 cubes should always suffice.
  (destructuring-bind (cx cy cz) cube
    (destructuring-bind (sx sy sz) sub-cube
      (loop for (state-x . sub-seg-x) in (segment-diff cx sx)
	    nconc (loop for (state-y . sub-seg-y) in (segment-diff cy sy)
			nconc (loop for (state-z . sub-seg-z) in (segment-diff cz sz)
				    if (or state-x state-y state-z)
				      collect (list sub-seg-x sub-seg-y sub-seg-z)))))))

(defun cube-diff (lit-zone command)
  "Removes command from lit-zone. Returns (up to 26) sub-cubes of lit-zone."
  (destructuring-bind (lx ly lz) lit-zone
    (destructuring-bind (cx cy cz) command
      (let ((ix (segment-intersection lx cx))
	    (iy (segment-intersection ly cy))
	    (iz (segment-intersection lz cz)))
	(if (some #'null (list ix iy iz))
	    ;; no intersection, nothing to remove
	    (list lit-zone)
	    ;; gotta remove!
	    (actual-cube-diff lit-zone (list ix iy iz)))))))

;; box-merging was intended as an optimization. in practice it makes things worse.
;; the goal was to merge the 26 blocks into down to 6.
(defun try-merge-2 (b1 b2)
  ;; 2 boxes are compatible when they have same bounds on 2 dimensions and adjacent ones on the 3rd.
  (flet ((same-or-join (i j)
	   (cond
	     ((= i j) i)
	     ((= (imagpart i) (1- (realpart j))) (complex (realpart i) (imagpart j)))
	     ((= (imagpart j) (1- (realpart i))) (complex (realpart j) (imagpart i)))
	     (t (return-from try-merge-2 nil)))))
    (if (< 2 (loop for bb1 in b1
		   for bb2 in b2
		   count (= bb1 bb2)))
	nil
	(map 'list #'same-or-join b1 b2))))

(defun merge-boxes-reduce (boxes new-box)
  ;; Computes whether a box in 'boxes' can be merged with new-box.
  ;; If yes, repeat.
  ;; YES THE FLOW IS UGLY I KNOW.
  (loop for b in boxes
	for merged-box = (try-merge-2 b new-box)
	if merged-box
	  do (return-from merge-boxes-reduce
	       (merge-boxes-reduce (append (rest boxes) boxes-not-merged) merged-box))
	collect b into boxes-not-merged)
  (cons new-box boxes))

(defun merge-boxes (boxes)
  "Tries to merge boxes."
  (reduce #'merge-boxes-reduce boxes :initial-value nil))
(assert (equal (merge-boxes '((#C(0 2) #C(5 8) #C(10 10)) (#C(3 8) #C(5 8) #C(10 10))))
	       '((#C(0 8) #C(5 8) #C(10 10)))))

(defun add-command (lit-zones command)
  (format t "Adding command ~a to our ~a zones.~%" command (length lit-zones))
  ;; merge-boxes used to be here:
  (let ((new-lit-zones (loop for lit-zone in lit-zones
			     nconc (cube-diff lit-zone (rest command)))))
    
    (if (first command)
	(cons (rest command) new-lit-zones)
	new-lit-zones)))

(defun as-complex (input)
  (mapcar (lambda (line) (list (first line)
			       (complex (second line) (third line))
			       (complex (fourth line) (fifth line))
			       (complex (sixth line) (seventh line))))
	  input))

(defun segment-size (segment)
  (1+ (- (imagpart segment) (realpart segment))))

(defun box-size (box)
  (apply #'* (mapcar #'segment-size box)))

(defun part2-first-try (&optional (input *input*))
  (apply #'+
	 (mapcar #'box-size
		 (reduce #'add-command (as-complex input) :initial-value nil))))

;; Initial part2 approach did not work...
;; Instead, when we merge:
;; * reset all points in the next box (by generating intersection boxes of flipped value)
;; * add the next box if it's on.
;; At the end, sum the total value of each box.
;; This ends up much terser and simpler than the previous approach! The only bits I reuse are
;; segment-intersection and box-size.
(defun generate-intersection-boxes (boxes-and-value new-box)
  (loop for (box . value) in boxes-and-value
	for intersections = (map 'list #'segment-intersection new-box box)
	unless (some #'null intersections)
	  collect (cons intersections (- value))))

(defun add-command2 (boxes-and-value new-box)
  (let ((intersection-boxes (generate-intersection-boxes boxes-and-value (rest new-box))))
    (if (first new-box)
	(cons (cons (rest new-box) 1) (nconc intersection-boxes boxes-and-value))
	(nconc intersection-boxes boxes-and-value))))

(defun part2 (&optional (input *input*))
  (loop for (box . value) in (reduce #'add-command2 (as-complex input) :initial-value nil)
	sum (* value (box-size box))))
(assert (= (part2 *input-test*)
	   2758514936282235))
