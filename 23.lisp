;; world parsing & printing fuctions.

;; the world is a list
;; each item is either ('spot . contents)
;; or (('entrance . room-owner) . contents-list)
(defun make-world (rooms-contents)
  `((spot)
    (spot)
    ((entrance . A) . ,(first rooms-contents))
    (spot)
    ((entrance . B) . ,(second rooms-contents))
    (spot)
    ((entrance . C) . ,(third rooms-contents))
    (spot)
    ((entrance . D) . ,(fourth rooms-contents))
    (spot)
    (spot)))

(defun print-world (world &optional (s t))
  (format s "#############~%#")
  (loop for tile in world
	do (format s "~a"
		   (if (entrance-p tile)
		       #\.
		       (or (tile-contents tile) #\.))))
  (format s "#~%")
  ;; rows of entrances
  (loop for row-count from 0 below (length (tile-contents (third world)))
	do (loop for tile in world
		   initially (format s " ")
		 do (format s "~a"
			    (if (spot-p tile)
				#\#
				(or (nth row-count (tile-contents tile)) #\.)))
		 finally (format s "~%")))
  (format s " ###########~%"))

(defun draw-world (world)
  ;; inverse function of parse-world.
  (with-output-to-string (s)
    (print-world world s)))

(defun parse (input)
  (make-world
   (loop for index in '(3 5 7 9)
	 collect (loop for line in input
		       if (some #'alphanumericp (coerce line 'list))
			 collect (intern (string (char line index)))))))

;; inputs & goal
(defparameter *input* (parse (uiop:read-file-lines "23.input")))
(defparameter *input-test* (parse (uiop:read-file-lines "23.input.test")))
(defparameter *goal*
  (make-world '((A A) (B B) (C C) (D D))))

;; world manipulation.
(defun tile-type (tile)
  "Returns the type of a tile. Either 'spot or '(entrance . <letter>)."
  (car tile))

(defun tile-contents (tile)
  "Returns the contents of a tile. Either <letter> or '(<letter> <letter>) or nil(s)."
  (cdr tile))

(defun spot-p (tile)
  (eq (tile-type tile) 'spot))

(defun empty-spot-p (tile)
  (and (spot-p tile)
       (null (tile-contents tile))))

(defun make-spot (&optional amphipod)
  (cons 'spot amphipod))

(defun entrance-p (tile)
  "Given a tile, returns its owner if it's an entrance, or nil."
  (when (consp (tile-type tile))
    (assert (eq (car (tile-type tile)) 'entrance))
    (cdr (tile-type tile))))

(defun make-room (owner &optional contents)
  (cons (cons 'entrance owner) contents))

;; move functions

(defun move-cost (amphipod)
  (ecase amphipod
    (A 1)
    (B 10)
    (C 100)
    (D 1000)))

(defun kick-out-next (contents &optional (depth 1))
  "Returns (values depth victim new-situation)."
  (unless contents
    (error "Noone to kick out!"))
  (if (first contents)
      (values depth
	      (first contents)
	      (append (loop for d below depth collect nil)
		      (rest contents)))
      (kick-out-next (rest contents) (1+ depth))))
(assert (equal '(2 a (nil nil b c))
	       (multiple-value-list (kick-out-next '(nil a b c)))))

(defun try-kick-out-of-room (owner contents)
  "Returns (cost amphipod new-room)."
  (unless (every (lambda (c) (or (null c) (eq owner c))) contents)
    ;; kick out the first non-nil
    (multiple-value-bind (depth out new-contents) (kick-out-next contents)
      (list 
       (* depth (move-cost out))
       out
       (make-room owner new-contents)))))

(defun next-out (tile)
  "If an amphipod can move out of tile, returns (cost amphipod new-tile)."
  (if (spot-p tile)
      (list 0 (tile-contents tile) (make-spot))
      (try-kick-out-of-room (entrance-p tile)
			    (tile-contents tile))))

(defun get-into-room (contents in &optional (depth 1))
  "Returns nil if not possible, or (cost . new-contents)"
  (if (null contents)
      nil
      (let ((rec (get-into-room (rest contents) in (1+ depth))))
	(if rec
	    (cons (car rec)
		  (cons (first contents) (cdr rec)))
	    (if (null (first contents))
		;; insert here
		(cons (* depth (move-cost in))
		      (cons in (rest contents)))
		nil)))))

(defun enter-room (tile amphipod)
  "Returns (cost . new-room) when amphipod enters tile if it can, or nil"
  (assert (eq amphipod (entrance-p tile)))
  (if (some (lambda (a) (and a (not (eq a amphipod)))) (tile-contents tile))
      ;; intruders in the room
      nil
      (destructuring-bind (cost . new-contents)
	  (get-into-room (tile-contents tile) amphipod)
	(cons cost (make-room amphipod new-contents)))))

(defun try-enter (current-tile-type tile amphipod)
  "Try to move amphipod into tile, returning (cost . new-tile), or nil."
  (cond
    ;; here we forbide room->room, which is actually allowed.
    ((eq current-tile-type (tile-type tile)) nil)
    ((empty-spot-p tile) (cons 0 (make-spot amphipod)))
    ((eq amphipod (entrance-p tile)) (enter-room tile amphipod))
    (t nil)))

(defun try-move (amphipod current-tile-type next-tiles)
  "Returns a list of (cost . new-tiles) where amphipod can move.
  If it can move into its designated room, only return this."
  ;; careful: if we're in a spot, we cannot go to another spot!
  (loop for (tile . rest) on next-tiles
	for steps upfrom 1
	;; is it blocked?
	until (and (spot-p tile) (tile-contents tile))
	for (cost . new-tile) = (try-enter current-tile-type tile amphipod)
	if new-tile
	  collect (let ((total-cost (+ cost (* steps (move-cost amphipod))))
			(new-world (append past-tiles (cons new-tile rest))))
		    (if (entrance-p new-tile)
			;; amphipod got into its designated room
			(return-from try-move (list (cons total-cost new-world)))
			(cons total-cost new-world)))
	collect tile into past-tiles))

(defun possible-moves (world)
  "Returns a list of (cost . new-world) after moving 1 amphipod."
  (loop for (tile . rest) on world
	for (cost-out amphipod-out tile-out) = (next-out tile)
	if amphipod-out
	  nconc (append
		 ;; if we move to the left
		 ;; Don't forget to reverse the resulting worlds.
		 (loop for (cost-left . reversed-world-left) in (try-move amphipod-out (tile-type tile) (reverse past))
		       collect (cons
				(+ cost-left cost-out)
				(append (reverse (cons tile-out reversed-world-left))
					rest)))
		 ;; if we move to the right
		 (loop for (cost-right . world-right) in (try-move amphipod-out (tile-type tile) rest)
		       collect (cons
				(+ cost-right cost-out)
				(append past
					(cons tile-out world-right)))))
	collect tile into past))

(defun paths (from to cost visited)
  (if (equal from to)
      (cons cost (reverse visited))
      (reduce (lambda (c-p-1 c-p-2)
		(if (< (car c-p-1) (car c-p-2) ) c-p-1 c-p-2))
	      (loop for (c . next) in (possible-moves from)
		    unless (member next visited :test #'equal)
		      collect (paths next to (+ c cost) (cons next visited)))
	      :initial-value (list most-positive-fixnum))))

(defun dijkstra-short-path (start end)
  (paths start end 0 (list start)))

(defun part1 (&optional (input *input*) (goal *goal*))
  (destructuring-bind (cost . steps) (dijkstra-short-path input goal)
    (format t "cost: ~a.~%" cost)
    (loop for s in steps
	  do (print-world s)
	  do (terpri))
    cost))

(defparameter *input2* (parse (uiop:read-file-lines "23.input.2")))
(defparameter *input-test2* (parse (uiop:read-file-lines "23.input.test.2")))
(defparameter *goal2*
  (make-world '((A A A A) (B B B B) (C C C C) (D D D D))))
(defun test2 ()
  (assert (= 44169 (part1 *input-test2* *goal2*))))

(defun part2 () (part1 *input2* *goal2*))
