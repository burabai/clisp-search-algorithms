
(defun state-eq? (state1 state2)
  (equal state1 state2))

(defun taken? (state x &optional (y nil))
  (if (null y)
      (if (null (nth x state)) (return-from taken? nil) t)
      (progn
        (if (null (nth x state)) (return-from taken? nil))
        (= (nth x state) y))))

(defun goal? (state)
  (loop for x in state never (null x)))

(defun safe? (current-state x y)
  (loop for i from 0 upto x do
        (when (taken? current-state i y) (return-from safe? nil)))
  (loop for i from x downto 0
        for j from y downto 0 do
        (when (taken? current-state i j) (return-from safe? nil)))
  (loop for i from x downto 0
        for j from y upto 7 do
        (when (taken? current-state i j) (return-from safe? nil)))
  t)

(defun expand (state &aux children)
  (loop for x from 0 to 7 do
        (when (not (taken? state x))
          (loop for y from 0 to 7 do
                (when (safe? state x y)
                  (push (replace (copy-tree state) (list y) :start1 x :end1 (+ x 1)) children)))
          (return-from expand children))))

(defun dequeue (queue) (reverse (cdr (reverse queue))))

(defun tree-search (&optional (verbose nil) &aux FRINGE CURRENT SOLUTIONS)
  (push '(nil nil nil nil nil nil nil nil) FRINGE)
  (loop while FRINGE do
        (setf CURRENT (car (last FRINGE)))
        (when verbose (format t "~A~%" CURRENT))
        (when (goal? CURRENT) (push CURRENT SOLUTIONS))
        (setf fringe (dequeue fringe))
        (loop for next in (expand CURRENT) do
              ;(format t "  CHILD: ~A~%" next)
              (push next FRINGE))
        ;(terpri)
        )
  SOLUTIONS)