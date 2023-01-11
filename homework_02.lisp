(defun divmod-excel (n &aux a b)
  (setf a (truncate n 26))
  (setf b (mod n 26))
  (if (= b 0) 
      (list (- a 1) (+ b 26))
      (list a b)))

(defun get-symbol (code &aux (char-list nil))
  "Returns the symbol representation of the node based on its position.
  Example: (get-symbol 0) => A; (get-symbol 25) => Z; (get-symbol 26) => AA; (get-symbol 51) => AZ, etc."
  (incf code)
  (when (< code 0) (return-from get-symbol nil))
  (when (= code 0) (return-from get-symbol 'A))
  (loop while (> code 0)
        do (let ((current-char (cadr (divmod-excel code))))
             (setf code (car (divmod-excel code)))
             (push current-char char-list)))
  (setf char-list (mapcar #'(lambda (x) (+ 64 x)) char-list))
  (setf char-list (mapcar #'(lambda (x) (code-char x)) char-list))
  (return-from get-symbol (intern (format nil "~{~A~}" char-list))))

(defun count-children (depth factor)
  (reduce #'+ (loop for n from 1 to (- depth 1) 
                    collect (expt factor n))))

(defun k-tree (depth factor &optional (current-node 0) &aux child-count)
  "Returns a perfect k-tree of the specified depth.
  The nodes are ordered in the depth-first manner."
  (when (< factor 1) (return-from k-tree nil))
  (setf child-count (count-children depth factor))
  ;(format t "depth: ~A, factor: ~A, current node: ~A, child count: ~A~%" depth factor current-node child-count)
  (when (< depth 1) (return-from k-tree nil))
  (when (= depth 1) (return-from k-tree (list (get-symbol current-node))))
  (append (list (get-symbol current-node)) 
          (loop for child from 0 to (1- factor) 
                collect (k-tree (1- depth) factor (+ current-node (* (truncate child-count factor) child) 1)))))

(defun node-name (node) (car node))
(defun node-children (node) (cdr node))
(defun dequeue (queue) (reverse (cdr (reverse queue))))

(defun dfs (node target &aux found?)
  "Performs DEPTH-FIRST SEARCH on the given tree.
  Outputs T is found, NIL if not found.
  Node traversal is printed out."
  (when (null node) (return-from dfs nil))
  (if (equal (node-name node) target) 
      (progn 
        (format t "~A" (node-name node))
        (return-from dfs t))
      (format t "~A " (node-name node)))
  (dolist (child (node-children node))
    (setf found? (dfs child target))
    (if found? 
        (return-from dfs t)
        (format t "~A " (node-name node)))))

(defun dls (node target depth &aux result found? remaining? any-remaining?)
  "Performs DEPTH-LIMITED SEARCH of the specified depth on the given tree."
  (if (= depth 0)
      (if (equal (node-name node) target)
          (progn
            (format t "~A " (node-name node))
            (return-from dls (list t nil)))
          (progn
            (if (equal (node-name node) 'A) nil (format t "~A " (node-name node)))
            (return-from dls (list nil (if (node-children node) t nil)))))
      (if (> depth 0) 
          (progn
            (setf any-remaining? nil)
            ;(format t "~A " (node-name node))
            (dolist (child (node-children node))
              (format t "~A " (node-name node))
              (setf result (dls child target (1- depth)))
              (setf found? (first result))
              (setf remaining? (car (last result)))
              (if found? (return-from dls (list t remaining?)))
              ;(format t "~A " (node-name node))
              (if remaining? (setf any-remaining? t)))
            (return-from dls (list nil any-remaining?))))))

(defun iddfs (node target &aux (depth 0) found? remaining?)
  "Performs ITERATIVE-DEEPENING DEPTH-FIRST SEARCH."
  (loop 
    (setf result (dls node target depth))
    (setf found? (first result))
    (setf remaining? (car (last result)))
    ;(format t "depth ~A, remaining nodes: ~A~%" depth remaining?)
    (if found? (return-from iddfs t))
    (if (null remaining?) (return-from iddfs nil))
    (incf depth)))