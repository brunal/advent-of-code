(require :uiop)

(defparameter *input* (uiop:read-file-lines "06.input"))
(defparameter *input-test* (uiop:split-string "Time:      7  15   30
Distance:  9  40  200" :separator (string #\newline)))

(defun parse-input (input)
  (flet ((get-numbers (line)
	   (mapcar #'parse-integer
		   (rest
		    (remove-if #'uiop:emptyp
			       (uiop:split-string line :separator (string #\Space)))))))
    (apply #'mapcar #'list
	   (mapcar #'get-numbers input))))

(defun distance-reached (total-time time-waited)
  (* time-waited (- total-time time-waited)))

(defun solve-race (time distance)
  (destructuring-bind (x1 x2) (solve-race-equation time distance)
    ;; we want the integers that do better than distance, i.e. the
    ;; integers between `x1` and `x2`. Due to floating point & co., we
    ;; look at the integers around to find the smallest (for x1) and
    ;; highest (for x2) that are better than distance.
    ;; for x1, it's the smallest integer higher than x1.
    (let* ((x1 (ceiling x1))
	   (x2 (floor x2)))
      (unless (> (distance-reached time x1) distance) (incf x1))
      (unless (> (distance-reached time x2) distance) (decf x2))
      (1+ (- x2 x1)))))

(defun solve-race-equation (time distance)
  ;; Returns the 2 floats waiting times that reach `distance`.
  ;; Race has time `time`, best distance is `distance`
  ;; After waiting for `k` time, our speed is `k`, so we reach
  ;; distance `k * (time - k)`.
  ;; we solve `k * (time - k) > distance`
  ;; k * time - k*k > distance
  ;; k*k - k*time + distance < 0
  ;; delta = time*time - 4 * distance
  ;; solutions (time +- sqrt(delta)) / 2
  (let* ((delta (- (* time time) (* 4 distance)))
	 (x1 (/ (+ time (sqrt delta)) 2))
	 (x2 (/ (- time (sqrt delta)) 2)))
    ;; we don't just use floor & ceiling as we want *strict* inequality.
    ;; replace (floor x) with (1- (ceiling x)), and (ceiling x) with
    ;; (1+ (floor x)).
    ;; somehow this gives off-by-one error on the read dataset...
    (list x2 x1)))

(defun part1 (input)
  (apply #'*
	 (loop for (time distance) in (parse-input input)
	       collect (solve-race time distance))))

(print (part1 *input*))

(defun parse-input-part2 (input)
  (flet ((get-number (line)
	   (parse-integer
	    (remove-if (lambda (c) (char= c #\Space))
		       (second (uiop:split-string line :separator (string #\:)))))))
    (mapcar #'get-number input)))

(defun part2 (input)
  (apply #'solve-race (parse-input-part2 input)))

(print (part2 *input*))
