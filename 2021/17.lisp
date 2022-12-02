(ql:quickload 'cl-ppcre)

(defun parse (input)
  (mapcar #'parse-integer
	  (coerce
	   (nth-value
	    1
	    (cl-ppcre:scan-to-strings
	     "^target area: x=(\\d+)\\.\\.(\\d+), y=(-\\d+)\\.\\.(-\\d+)$"
	     input))
	   'list)))


(defparameter *input-test* (parse "target area: x=20..30, y=-10..-5"))
(defparameter *input* (parse (first (uiop:read-file-lines "17.input"))))

;; v_x(n) = max(v_x(0) - n, 0)
;; pos_x(n) = sum(v_x(k <= n))
;; v_y(n) = v_y(0) - n
;; pos_y(n) = sum(v_y(k <= n))

;; pos_x(k =  vx0 - n) = pos_x(k=0) + k * (k + 1) / 2
;; pos_x(n <= vx0) = pos_x(n=vx0) + (vx0 - n) * (vx0 - n + 1) / 2
;; pos_x(n=0) = pos_x(n=vx0) + vx0 * (vx0 + 1) / 2 = 0 ; boundary condition
;; pos_x(n=vx0) = - vx0 * (vx0 + 1) / 2
;; pos_x(n >= vx0) = pos_x(vx0)
(defun sumsq (n) (/ (* n (1+ n)) 2))

(defun posx (vx0 n)
  (- (sumsq vx0) (sumsq (max 0 (- vx0 n)))))

;; pos_y(k = vy0 - n) = pos_y(k=0) + k * (k + 1) / 2
;; pos_y(n <= vy0) = pos_y(n=vy0) + (vy0 - n) * (vy0 - n + 1) / 2
;; pos_y(n=0) = pos_y(n=vy0) + vy0 * (vy0 + 1) / 2
;; pos_y(n=vy0) = - vy0 * (vy0 + 1) / 2

;; pos_y(n = k + vy0) = pos_y(vy0) - k * (k + 1) / 2
;; pos_y(n >= vy0) = pos_y(vy0) - (n - vy0) * (n - vy0 + 1) / 2
(defun posy (vy0 n)
  (if (< vy0 0)
      (- (* vy0 n) (sumsq (1- n)))
      (- (sumsq vy0) (sumsq (abs (- vy0 n))))))

(defun hits-the-box (vx0 vy0 xmin xmax ymin ymax &optional (n 0))
  (let* ((x (posx vx0 n))
	 (y (posy vy0 n)))
    (cond
      ((and (<= xmin x xmax) (<= ymin y ymax)) t)
      ((and (>= n vx0) (not (<= xmin x xmax))) nil)
      ((< y ymin)  nil)
      (t (hits-the-box vx0 vy0 xmin xmax ymin ymax (1+ n))))))

(defun highest-velocity-that-hits-the-box (xmin xmax ymin ymax)
  ;; vy in [ymax, -ymin]
  ;; vx in [sqrt(2*(xmin-1)), xmax]
  (loop for vy from (- ymin) downto ymax
	do (loop for vx from (floor (sqrt (* 2 (1- xmin)))) upto xmax
		 do (if (hits-the-box vx vy xmin xmax ymin ymax)
			(return-from highest-velocity-that-hits-the-box (complex vx vy))))))

(defun part1 (&optional (coords-list *input*))
  (sumsq
   (imagpart
    (apply #'highest-velocity-that-hits-the-box coords-list))))

(print (part1)) 

(defun all-velocities-that-hit-the-box (xmin xmax ymin ymax)
  ;; vy in [ymin, -ymin]
  ;; vx in [sqrt(2*(xmin-1)), xmax]
  (loop for vy from ymin to (- ymin)
	nconc (loop for vx from (floor (sqrt (* 2 (1- xmin)))) upto xmax
		    if (hits-the-box vx vy xmin xmax ymin ymax)
		      collect (complex vx vy))))


(defun part2 (&optional (coords-list *input*))
  (length
    (apply #'all-velocities-that-hit-the-box coords-list)))


(print (part2))
