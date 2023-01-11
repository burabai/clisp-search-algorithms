; ----------------------------------------
; TASK 1 - PART A - BREADTH-FIRST PRINTING
; ----------------------------------------

(defun node_name (node) (car node))
(defun node_children (node) (cdr node))
(defun dequeue (queue) (reverse (cdr (reverse queue))))

(defun bft (tree)
  (when (null tree) (return-from bft nil))
  (defvar *fringe* nil)
  (push tree *fringe*)
  (loop while *fringe*
    do (let ((level_nodes (list-length *fringe*)))
         (loop while (> level_nodes 0)
           do (let ((current_node (car (last *fringe*))))
           (setf *fringe* (dequeue *fringe*))
           (format t "~a " (node_name current_node))
           (dolist (child (node_children current_node))
                    (if child (push child *fringe*)))
           (decf level_nodes)))
         (terpri))))

; (print "TASK 1, part a: BREADTH-FIRST TRAVERSAL")
; (terpri)
; (bft '(A (B (C (D)) (E (F) (G (H)))) (J (K (L (M) (N (P)))))))
; (terpri)

; ------------------------------------------
; TASK 1 - PART B - CHILD SUCCESSOR PRINTING
; ------------------------------------------

(defun cst (tree)
  (when (null tree) (return-from bft nil))
  (defvar *fringe* nil)
  (push tree *fringe*)
  (loop while *fringe*
    do (let ((current_node (car (last *fringe*))))
         (setf *fringe* (dequeue *fringe*))
         (format t "~a, " (node_name current_node))
         (dolist (child (node_children current_node))
                 (if child (progn
                            (push child *fringe*)
                            (format t "~a " (node_name child)))))
         (terpri))))

; (print "TASK 1, part b: CHILD SUCCESSOR TRAVERSAL")
; (terpri)
; (cst '(A (B (C (D)) (E (F) (G (H)))) (J (K (L (M) (N (P)))))))

; -----------------------------
; TASK 2 - SAME, BUT WITH COSTS
; -----------------------------
(defun node_true_name (node) (caar node))
(defun node_cost (node) (cadar node))

(defun bft_cost (tree)
  (when (null tree) (return-from bft nil))
  (defvar *fringe* nil)
  (push tree *fringe*)
  (loop while *fringe*
    do (let ((level_nodes (list-length *fringe*)))
         (loop while (> level_nodes 0)
           do (let ((current_node (car (last *fringe*))))
           (setf *fringe* (dequeue *fringe*))
           (format t "~a,~a " (node_true_name current_node) (node_cost current_node))
           (dolist (child (node_children current_node))
                    (if child (progn
                               (incf (cadar child) (node_cost current_node))
                               (push child *fringe*))))
           (decf level_nodes)))
         (terpri))))

(defun cst_cost (tree)
 (when (null tree) (return-from bft nil))
 (defvar *fringe* nil)
 (push tree *fringe*)
 (loop while *fringe*
   do (let ((current_node (car (last *fringe*))))
        (setf *fringe* (dequeue *fringe*))
        (format t "~a,~a; " (node_true_name current_node) (node_cost current_node))
        (dolist (child (node_children current_node))
                (if child (progn
                           (incf (cadar child) (node_cost current_node))
                           (push child *fringe*)
                           (format t "~a,~a " (node_true_name child) (node_cost child)))))
        (terpri))))

; (print "TASK 2, part a: BREADTH-FISRT TRAVERSAL WITH COSTS")
; (terpri)
; (bft_cost '((A 0) ((B 5) ((C 3) ((D 4))) ((E 2) ((F 1)) ((G 7) ((H 9))))) ((J 1) ((K 3) ((L 1) ((M 7)) ((N 1) ((P 2))))))))
;
; (print "TASK 2, part b: CHILD-SUCCESSOR TRAVERSAL WITH COSTS")
; (terpri)
; (cst_cost '((A 0) ((B 5) ((C 3) ((D 4))) ((E 2) ((F 1)) ((G 7) ((H 9))))) ((J 1) ((K 3) ((L 1) ((M 7)) ((N 1) ((P 2))))))))
