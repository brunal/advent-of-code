(require :uiop)
(require :cl-ppcre)
(require :alexandria)

(defparameter *input* (uiop:read-file-lines "16.input"))
(defparameter *input-test* (uiop:split-string "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II" :separator (string #\newline)))

(defparameter *line-pattern* "^Valve (..) has flow rate=(.+); tunnels? leads? to valves? (.+)$")

(defun parse-input (input)
  "Returns 2 alists: volumes and paths."
  (let ((volumes nil)
	(tunnels nil))
    (loop for line in input
	  collect (cl-ppcre:register-groups-bind (valve flow paths)
		      (*line-pattern* line)
		    (setf volumes
			  (acons (intern valve) (parse-integer flow) volumes))
		    (setf tunnels
			  (acons (intern valve)
				 (loop for valve in (uiop:split-string paths :separator ", ")
				       unless (string= "" valve) collect (intern valve))
				 tunnels))))
    (cons volumes tunnels)))

(defun get-edge-weight (graph from to)
  (cdr (assoc to (cdr (assoc from graph)))))

(defun set-edge-weight (graph start end weight)
  (let* ((start-and-destinations (assoc start graph))
	 (edge-weight (assoc end (cdr start-and-destinations))))
    (if (null edge-weight)
	(push (cons end weight) (cdr start-and-destinations))
	(setf (cdr edge-weight) weight))))

(defun build-complete-graph (tunnels)
  ;; fill in initial values.
  (let ((edges (loop for (from . tos) in tunnels collect from))
	(graph (loop for (from . tos) in tunnels
		     collect (cons from (loop for to in tos collect (cons to 1))))))
    ;; use floyd-warshall to complete the graph.
    (loop for k in edges
	  do (loop for i in edges
		     do (loop for j in edges
			      for current-w = (get-edge-weight graph i j)
			      for w1 = (get-edge-weight graph i k)
			      for w2 = (get-edge-weight graph k j)
			      if (and (not (eq i j)) w1 w2 (or (null current-w) (< (+ w1 w2) current-w)))
				do (set-edge-weight graph i j (+ w1 w2)))))
    graph))

(defun remove-valves (graph valves-to-remove)
  "Returns a graph without valves."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for (valve . current-destinations) of-type (symbol . t) in graph
	if (not (member valve valves-to-remove))
	  collect (cons valve (loop for (to . distance) of-type (symbol . fixnum) in current-destinations
				    if (not (member to valves-to-remove))
				      collect (cons to distance)))))

(declaim (ftype (function
		 (list list &key (:current symbol) (:valves-open list) (:time-left fixnum))
		 fixnum)
		max-pressure-release))
(defun max-pressure-release (graph volumes &key current valves-open time-left)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  ;; note: time left is after opening current.
  (+ (the fixnum (* time-left (the fixnum (cdr (assoc current volumes)))))
     (the fixnum (loop for (next . distance) of-type (symbol . fixnum) in (cdr (assoc current graph))
		       if (and (< distance time-left) ; we have time to reach it and open it.
			       (not (member next valves-open))) ;  it's not already open.
			 maximize (max-pressure-release graph
							volumes
							:current next
							:valves-open (cons current valves-open)
							:time-left (- time-left distance 1))))))

(defun part1 (&optional (input *input*))
  (destructuring-bind (volumes . tunnels) (parse-input input)
    (max-pressure-release
     (remove-valves (build-complete-graph tunnels)
		    (loop for (valve . volume) in volumes
			  if (and (not (eq 'AA valve)) (zerop volume))
			    collect valve))
     volumes
     :current 'AA
     :valves-open nil
     :time-left 30)))

(assert (= (part1 *input-test*) 1651))

;; part2: we have 26 minutes and are in 2 locations at a time.
;; we build all partitions of the valves and assign me/the elephant a partition
;; each. sum the pressures, take the best of all partitions.
(defun partitions (items)
  "Returns the list of all (set1 set2) where set1 & set2 are a partition of items."
  (if (null items)
      (cons nil nil)
      (destructuring-bind (first . rest) items
	(loop for (s1 s2) in (partitions rest)
	      collect (list (cons first s1) s2)
	      collect (list s1 (cons first s2))))))

(defun part2 (&optional (input *input*))
  (destructuring-bind (volumes . tunnels) (parse-input input)
    (let* ((graph (remove-valves (build-complete-graph tunnels)
			       (loop for (valve . volume) in volumes
				     if (and (not (eq 'AA valve)) (zerop volume))
				       collect valve)))
	   (valves-to-partition (loop for (valve . _) in graph unless (eq 'AA valve) collect valve)))
      (loop for (v1 v2) in (partitions valves-to-partition)
	    for g1 = (remove-valves graph v1)
	    for g2 = (remove-valves graph v2)
	    maximize (+
		      (max-pressure-release g1 volumes :current 'AA :valves-open nil :time-left 26)
		      (max-pressure-release g2 volumes :current 'AA :valves-open nil :time-left 26))))))

(assert (= (part2 *input-test*) 1707))
