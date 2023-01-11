;; ROMANIAN PROBLEM

(defstruct (city (:type list))
  name 
  hcost 
  neighbors 
  (visited? nil))

(defstruct (node (:type list))
  city 
  (parent nil)
  path 
  (gcost 0) 
  (hcost 0)
  (fcost 0)
  (cost 1))

(defparameter *romania-map*
  '((Arad 366 ((Zerind . 75) (Sibiu . 140) (Timisoara . 118)) nil)
    (Bucharest 0 ((Fagaras . 211) (Pitesti . 101) (Giurgiu . 90) (Urziceni . 85)) nil)
    (Craiova 160 ((Dobreta . 120) (Rimnicu . 146) (Pitesti . 138)) nil)
    (Dobreta 242 ((Mehadia . 75) (Craiova . 120)) nil)
    (Eforie 161 ((Hirsova . 86)) nil)
    (Fagaras 176 ((Sibiu . 99) (Bucharest . 211)) nil)
    (Giurgiu 77 ((Bucharest . 90)) nil)
    (Hirsova 151 ((Urziceni . 98) (Eforie . 86)) nil)
    (Iasi 226 ((Neamt . 87) (Vaslui . 92)) nil)
    (Lugoj 244 ((Timisoara . 111) (Mehadia . 70)) nil)
    (Mehadia 241 ((Lugoj . 70) (Dobreta . 75)) nil)
    (Neamt 234 ((Iasi . 87)) nil)
    (Oradea 380 ((Zerind . 71) (Sibiu . 151)) nil)
    (Pitesti 100 ((Rimnicu . 97) (Craiova . 138) (Bucharest . 101)) nil)
    (Rimnicu 193 ((Sibiu . 80) (Pitesti . 97) (Craiova . 146)) nil)
    (Sibiu 253 ((Arad . 140) (Oradea . 151) (Fagaras . 99) (Rimnicu . 80)) nil)
    (Timisoara 329 ((Arad . 118) (Lugoj . 111)) nil)
    (Urziceni 80 ((Bucharest . 85) (Hirsova . 98) (Vaslui . 142)) nil)
    (Vaslui 199 ((Iasi . 92) (Urziceni . 142)) nil)
    (Zerind 374 ((Arad . 75) (Oradea . 71)) nil)))

(defun reload-map ()
  (defparameter *map* (copy-tree *romania-map*))
  (defparameter *romania-table* (make-hash-table))
  (loop for city in *map* do
      (setf (gethash (city-name city) *romania-table*) city)))

(defun get-city (name)
  (gethash name *romania-table*))

(defun get-neighbors (name)
  (mapcar #'(lambda (n) (gethash (car n) *romania-table*)) (city-neighbors (gethash name *romania-table*))))

(defun neighbors? (name1 name2)
  (cdr (assoc name2 (city-neighbors (gethash name1 *romania-table*)))))

(defun est-dist (a b)
  (abs (- (city-hcost a) (city-hcost b))))

(defun heuristic-decreasing (node)
  (- 600 (node-fcost node)))

(defun heuristic-greedy (node)
  (node-hcost node))

(defun heuristic-A* (node)
  (node-fcost node))

(defun root-node (city goal)
  (make-node :city city
             :path (list (list (city-name city) (list 0 (est-dist city goal))))
             :gcost 0
             :hcost (est-dist city goal)
             :fcost (est-dist city goal)
             :cost 0))

(defun expand (node goal)
  (setf (city-visited? (node-city node)) t)
  (loop for neighbor in (get-neighbors (city-name (node-city node))) unless (city-visited? neighbor)
        collect (make-node :city neighbor 
                           :parent (node-city node) 
                           :gcost (+ (node-gcost node) (neighbors? (city-name (node-city node)) (city-name neighbor)))
                           :hcost (est-dist neighbor goal)
                           :fcost (+ (+ (node-gcost node) 
                                        (neighbors? (city-name (node-city node)) 
                                                    (city-name neighbor)))
                                     (est-dist neighbor goal))
                           :path (append (node-path node) (list (list (city-name neighbor) (list (+ (node-gcost node) (neighbors? (city-name (node-city node)) (city-name neighbor))) 
                                                                                                                      (est-dist neighbor goal)))))
                           :cost (neighbors? (city-name (node-city node)) (city-name neighbor)))))

(defun node-in-fringe? (node fringe)
  (find-if #'(lambda (x) (equal (city-name (node-city x)) (city-name (node-city node))))
           fringe))

(defun best-first-search (start-name goal-name &optional (strategy #'heuristic-A*) &aux FRINGE CURRENT DUPLICATE?)
  (reload-map)
  (push (root-node (get-city start-name) (get-city goal-name)) FRINGE)
  (loop while FRINGE do
        (sort FRINGE #'< :key strategy)
        ;(format t "SORTED FRINGE: ~{~A, ~}~%" (loop for elem in FRINGE collect (list (city-name (node-city elem)) (node-fcost elem))))
        (setf CURRENT (car FRINGE))
        ;(format t "CURRENT CITY NAME: ~A~%" (city-name (node-city CURRENT)))
        (when (equal (city-name (node-city CURRENT)) goal-name) (return-from best-first-search 
                                                                             (list (node-path CURRENT)
                                                                                   (loop for nd in FRINGE collect 
                                                                                         (list (city-name (node-city nd)) (list (node-gcost nd) (node-hcost nd)))))))
        (pop fringe)
        (loop for neighbor in (expand CURRENT (get-city goal-name)) do
              (block continue
                     ;(format t "  NEIGHBOR: ~A~%" (list (city-name (node-city neighbor)) (node-fcost neighbor)))
                     (setf DUPLICATE? (node-in-fringe? neighbor FRINGE))
                     (if DUPLICATE? 
                         (if (>= (node-fcost neighbor) (node-fcost DUPLICATE?)) 
                           (return-from continue)
                           (setf FRINGE (remove DUPLICATE? fringe))))
                     (push neighbor FRINGE)))
        ;(terpri)
        ))

(defun print-solution (solution &aux fringe path comma-count)
  (setf fringe (cadr solution))
  (setf path (car solution))
  (format t "PATH:~%")
  (setf comma-count (1- (list-length path)))
  (loop for node in path do
        (format t "~A (~A+~A)" (car node) (caadr node) (cadadr node))
        (when (> comma-count 0) (format t ", "))
        (decf comma-count))
  (terpri)
  (format t "FRINGE:~%")
  (setf comma-count (1- (list-length fringe)))
  (loop for node in fringe do
        (format t "~A (~A+~A)" (car node) (caadr node) (cadadr node))
        (when (> comma-count 0) (format t ", "))
        (decf comma-count)))

(defun search-print (start-name goal-name &optional (strategy #'heuristic-A*))
  (print-solution (best-first-search start-name goal-name strategy)))