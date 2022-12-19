(require :uiop)
(require :cl-ppcre)

(defparameter *pattern* "Blueprint (.+): Each ore robot costs (.+) ore. Each clay robot costs (.+) ore. Each obsidian robot costs (.+) ore and (.+) clay. Each geode robot costs (.+) ore and (.+) obsidian.")

(defun parse-input (input)
  (loop for line in input
	collect (destructuring-bind
		    (id ore-ore clay-ore obs-ore obs-clay geode-ore geode-obs)
		    (mapcar #'parse-integer (coerce (nth-value 1 (cl-ppcre:scan-to-strings *pattern* line)) 'list))
		  (cons id
			(list
			 (list ore-ore 0 0 0)
			 (list clay-ore 0 0 0)
			 (list obs-ore obs-clay 0 0)
			 (list geode-ore 0 geode-obs 0))))))
	
(defparameter *input* (parse-input (uiop:read-file-lines "19.input")))
(defparameter *input-test*
  (parse-input '("Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
		 "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")))

(defun maximize-geode-robots (costs time)
  (loop with resources-and-robots = '(((0 0 0 0) . (1 0 0 0)))
	for time-step below time
	for next-step-resources-and-robots = nil
	do (loop for (resources . robots) in resources-and-robots
		 do (loop for cost in costs
			  for robot-built in '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))
			  if (loop for r in resources for c in cost always (>= r c))
			    do (push (cons
				      (mapcar #'+ resources robots (mapcar #'- cost))
				      (mapcar #'+ robots robot-built))
				     next-step-resources-and-robots)
			  finally
			     ;; build nothing
			     (push (cons (mapcar #'+ resources robots) robots)
				   next-step-resources-and-robots)))
	   ;; sort by # of resources+robots
	do (setf resources-and-robots (sort next-step-resources-and-robots
					    (lambda (rr1 rr2) (loop for r1 in rr1
								    for r2 in rr2
								    if (> r1 r2) do (return t)
								    if (< r1 r2) do (return nil)
								      finally (return nil)))
					    ;; 4-tuple of res+rob, rarest first.
					    :key (lambda (res-rob)
						   (reverse (mapcar #'+ (car res-rob) (cdr res-rob))))))
	   ;; keep the best 8k solutions so far (1k was fine with part1, 8k solved part2).
	do (setf resources-and-robots
		 (nbutlast resources-and-robots (max 0 (- (length resources-and-robots) 8000))))
	   
	finally (return
		  (first (first (sort resources-and-robots #'> :key (lambda (res-rob)
								      (nth 3 (car res-rob)))))))))

(defun part1 (&optional (input *input*))
  (loop for (id . costs) in input
	sum (* id (first (last (maximize-geode-robots costs 24))))))

(defun part2 (&optional (input *input*))
  (loop for (id . costs) in input
	while (< id 4)
	collect (first (last (maximize-geode-robots costs  32)))))
