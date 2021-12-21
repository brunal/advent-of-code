(ql:quickload 'queues)
(asdf:oos 'asdf:load-op :queues.priority-queue)

(defun parse (input)
  (mapcar (lambda (line) (digit-char-p (first (last (coerce line 'list))))) input))

(defparameter *input* (parse (uiop:read-file-lines "21.input")))
(defparameter *input-test* (list 4 8))

(defun new-die ()
  (let* ((current 0)
	 (roll (lambda ()
		 (values
		  (mod (+ (incf current) (incf current) (incf current)) 100)
		  current))))
    roll))

(defun mod+ (n m) (1+ (mod (1- n) m)))

(defun play-one (pos score die)
  ;; die goes 1 to 100 (not 0-99), positions 1-10.
  (let* ((rolls (* 3 (1+ (mod+ die 100))))
	 (new-position (mod+ (+ pos rolls) 10)))
    (values new-position
	    (+ score new-position)
	    (+ die 3))))

(defun play (pos1 pos2 &optional (score1 0) (score2 0) (die 1))
  (multiple-value-bind (pos1 score1 die) (play-one pos1 score1 die)
    (if (>= score1 1000)
	(* score2 (1- die))
	(multiple-value-bind (pos2 score2 die) (play-one pos2 score2 die)
	  (if (>= score1 1000)
	      (* score1 (1- die))
	      (play pos1 pos2 score1 score2 die))))))

(defun part1 (&optional (input *input*))
  (apply #'play input))

;; (score-and-pos-delta . count) list.
(defparameter *rolls-outcome*
  '((3 . 1)
    (4 . 3)
    (5 . 6)
    (6 . 7)
    (7 . 6)
    (8 . 3)
    (9 . 1)))

;; universe: (s1 s2 p1 p2)
;; universe-table: hashtable universe -> count
;; universes-array: universe-table[41]
(defun make-universes-array (&optional (size 41))
  (make-array (list size) :initial-contents
	      (loop for i from 0 below size
		    collect (make-hash-table :test #'equal))))

(defun add-universe-to-process (universes-array universe count)
  "Records universe in universes-array for future processing."
  (incf (gethash universe
		 (aref universes-array (+ (first universe) (second universe)))
		 0)
	count))

(defun new-universe (universe delta player-id)
  "Returns (new-universe . won)."
  (destructuring-bind (s1 s2 p1 p2) universe
    (if (= 1 player-id)
	(let ((new-position (mod+ (+ p1 delta) 10)))
	  (cons (list (+ s1 new-position) s2 new-position p2)
		(>= (+ s1 new-position) 21)))
	(let ((new-position (mod+ (+ p2 delta) 10)))
	  (cons (list s1 (+ s2 new-position) p1 new-position)
		(>= (+ s2 new-position) 21))))))

(defun play-next-set-of-universes (universes-array index)
  "Plays the universes at index, stores new universes. Returns (wins1 . wins2)."
  (let ((universes-table (aref universes-array index)))
    (loop with wins1 = 0
	  with wins2 = 0
	  for universe being the hash-key in universes-table
	    using (hash-value count)
	  ;; 1st player.
	  do (loop for (delta1 . count1) in *rolls-outcome*
		   for (new-universe1 . wins) = (new-universe universe delta1 1)
		   for total-count1 = (* count count1)
		   do (if wins
			  (incf wins1 total-count1)
			  ;; 2nd player.
			  (loop for (delta2 . count2) in *rolls-outcome*
				for (new-universe2 . wins) = (new-universe new-universe1 delta2 2)
				for total-count2 = (* total-count1 count2)
				do (if wins
				       (incf wins2 total-count2)
				       ;; enqueue the new universes.
				       (add-universe-to-process universes-array
								new-universe2
								total-count2)))))
	  finally (return (values wins1 wins2)))))

(defun full-dirac (pos1 pos2)
  (let ((universes (make-universes-array)))
    (add-universe-to-process universes (list 0 0 pos1 pos2) 1)
    (loop with wins1 = 0
	  with wins2 = 0
	  for score from 0 upto 40
	  do (multiple-value-bind (new-wins1 new-wins2)
		 (play-next-set-of-universes universes score)
	       (incf wins1 new-wins1)
	       (incf wins2 new-wins2)
	       ;; clear universes of score.
	       (setf (aref universes score) nil))
	  finally (return (values wins1 wins2)))))
(assert (equal (multiple-value-list (full-dirac 4 8))
	       '(444356092776315 341960390180808)))

(defun part2 (&optional (input *input*))
  (apply #'max
	 (multiple-value-list
	  (apply #'full-dirac input))))
