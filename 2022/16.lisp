(require :uiop)
(require :cl-ppcre)


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
			  (acons valve (parse-integer flow) volumes))
		    (setf tunnels
			  (acons valve
				 (loop for valve in (uiop:split-string paths :separator ", ")
				       unless (string= "" valve) collect valve)
				 tunnels))))
    (values volumes tunnels)))


(defun max-pressure-release (volume tunnels current-spot time-left valves-open)
  (if (eql 0 time-left)
      0
      (let ((result-if-open-current-spot
	      (if (or (eql 0 (cdr (assoc current-spot volume :test #'string=)))
		      (member current-spot valves-open))
		  ;; already open or worthless
		  0
		  ;; recurse
		  (+ (* time-left (cdr (assoc current-spot tunnels :test #'string=)))
		     (max-pressure-release volume
					   tunnels
					   current-spot
					   (1- time-left)
					   (cons current-spot valves-open))))))
	(apply #'max
	       result-if-open-current-spot
	       (loop for next-spot in (cdr (assoc current-spot tunnels :test #'string=))
		     collect (max-pressure-release volume tunnels next-spot (1- time-left) valves-open))))))

(defun part1 (input)
  (multiple-value-bind (volume tunnels) (parse-input input)
    (max-pressure-release volume tunnels "AA" 30 nil)))

;; do we really need all this?

(defun find-distance-by-dfs (start volume tunnels)
  "Returns a list of (end . distance) where end are the car of volume elements."
  (loop for next in (cdr (assoc start tunnels))
	do (format t "~A can reach ~A~%" start next)))

(defun build-distances (volume tunnels)
  "Returns a list of (start end distance)."
  (loop for (start . flow) in volume
	nconc (loop for (end . distance) in (find-distance-by-dfs start volume tunnels)
		    collect (list start end distance))))

(defun build-graph (volume tunnels)
  "Returns 2 values: volume-non-zero and list of (start end distance)."
  (let ((volume-non-zero (loop for valve-and-flow in volume
			       if (> (cdr valve-and-flow) 0)
			       collect valve-and-flow)))
    (values volume-non-zero (build-distances volume-non-zero tunnels))))
