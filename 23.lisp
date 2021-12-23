;; world parsing & printing fuctions.

;; the world is a list
;; each item is either ('spot . contents)
;; or (('entrance . room-owner) . (contents1 contents2))
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
  (format s "#~%#")
  ;; first row of entrances
  (loop for tile in world
	do (format s "~a"
		   (if (spot-p tile)
		       #\#
		       (or (first (tile-contents tile)) #\.))))
  (format s "#~%  ")
  ;; second row of entrances (skip 1 spot on each side)
  (loop for (tile . rest) on (rest world)
	while rest
	do (format s "~a"
		   (if (spot-p tile)
		       #\#
		       (or (second (tile-contents tile)) #\.))))
  (format s "~%  #########~%"))

(defun draw-world (world)
  ;; inverse function of parse-world.
  (with-output-to-string (s)
    (print-world world s)))

(defun parse (input)
  (let ((relevant-lines (subseq input 2 4)))
    (make-world
     (loop for index in '(3 5 7 9)
	   collect (loop for line in relevant-lines
			 collect (intern (string (char line index))))))))

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

(defun next-out (tile)
  "If an amphipod can move out of tile, returns (cost amphipod new-tile)."
  (if (spot-p tile)
      (list 0 (tile-contents tile) (make-spot))
      (let* ((room-owner (entrance-p tile))
	     (room-contents (tile-contents tile))
	     (first-a (first room-contents))
	     (second-a (second room-contents)))
	(if first-a
	    (unless (and (eq room-owner first-a)
			 (eq room-owner second-a))
	      ;; move out
	      (list (move-cost first-a)
		    first-a
		    (make-room room-owner (list nil second-a))))
	    (if second-a
		(unless (eq room-owner second-a)
		  ;; move out
		  (list (* 2 (move-cost second-a))
			second-a
			(make-room room-owner))))))))

(defun enter-room (tile amphipod)
  "Returns (cost . new-room) when amphipod enters tile if it can, or nil"
  (assert (eq amphipod (entrance-p tile)))
  (cond
    ;; intruders in the room
    ((some (lambda (a) (and a (not (eq a amphipod)))) (tile-contents tile))
     nil)
    ;; noone in it
    ((null (second (tile-contents tile)))
     (cons (* 2 (move-cost amphipod))
	   (make-room amphipod (list nil amphipod))))
    ;; first spot is still free
    ((null (first (tile-contents tile)))
     (cons (move-cost amphipod)
	   (make-room amphipod (cons amphipod (rest (tile-contents tile))))))))

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

(defun solve-world (world)
  "Returns a list of [(cost . goal)]."
  (loop for cost-and-new-world in (possible-moves world)
	for (cost . new-world) = cost-and-new-world
	nconc (if (equal world *goal*)
		    (progn
		      (format t "found a solution at cost ~a~%" cost)
		      (list cost-and-new-world))
		    ;; recurse
		    (loop for (cost2 . new-world2) in (solve-world new-world)
			  if new-world2
			    collect (cons (+ cost cost2) new-world2)))))

(defun cheap-solve (initial-world)
  (loop for (cost . goal-world) in (solve-world initial-world)
	minimize cost))
