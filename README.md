# Artificial Intelligence Common Lisp assignment solutions

## [Assignment 1 - BREADTH-FIRST TREE TRAVERSAL](homework_01.lisp)

This assignment consisted of two major parts: implementing a breadth-first traversal algorithm, and applying it to parse a weighted tree.
The algorithm itself is iterative in this case. A fringe in the form of a queue is created, and nodes are pushed to it as they are "discovered".
Also, at each iteration, the number of nodes in the fringe is taken and used to determine how many nodes will be printed out on one line.
The weighted-node version is essentially the same, expect node names are lists, and children inherit their parents' node costs plus their own cost.

### Functions:
- `node_name (node)`: gets the name of node (it is just the car of the whole node)
- `node_children (node)`: alias for cdr to get the children of node
- `dequeue (queue)`: returns queue without the last element (to mutate the queue)
- `node_true_name (node)`: returns the name of a complex node
- `node_cost (node)`: returns the cost of a complex node
- `bft (tree)`: regular BFT without costs and redundancy
- `cst (tree)`: BFT with children of each expanded node printed out
- `bft_cost/cst_cost (tree)`: ditto as the last two, but with costs

### Usage:
1. From the CLisp console in the project directory, call:
    ```
    (load "homework_01.lisp")
    ```

2. Call `bft(tree)` or `cst(tree)` like this:
    ```
    (bft '(A (B (C (D)) (E (F) (G (H)))) (J (K (L (M) (N (P)))))))
    ```

3. Call `bft_cost(tree)` or `cst_cost(tree)` like this:
    ```
    (bft_cost '((A 0) ((B 5) ((C 3) ((D 4))) ((E 2) ((F 1)) ((G 7) ((H 9))))) ((J 1) ((K 3) ((L 1) ((M 7)) ((N 1) ((P 2))))))))
    ```

## [Assignment 2 - TREE GENERATION, SEARCH ALGORITHMS](homework_02.lisp)

For this assignment, I implemented the generation of k-tree where the nodes are ordered in the depth-first manner, as well as three search algorithms: Depth-First Search, Depth-Limited Search, and Iterative-Deepening Depth-First Search (which the assignment manual wrongly called Breadth-First Search).

DLS was implemented because IDDFS uses it internally.

### Usage

1. Enter the CLisp console.
2. Call `(load "homework_02.lisp")`.
3. Generate a k-tree by calling `(ktree [depth] [factor])`.
4. Perform DFS by calling `(dfs [tree] [target])`.
5. Perform DLS by calling `(dfs [tree] [target] [depth])`.
6. Perform IDDFS by calling `(iddfs [tree] [target])`.

## [Assignment 3 - BEST-FIRST SEARCH, EIGHT-QUEENS](homework_03.lisp) ([Bonus](bonus_03.lisp))

For this assignment, Best-First Search (A*) was implemented along with several suboptimal heuristics. The heuristic for A* is admissible and consistent, so the search algorithm is guaranteed to be optimal. The others (greedy and decreasing heuristics) produce suboptimal results in graph search, since they are not consistent.

The EIGHT-QUEENS problem was solved as a bonus task using the BREADTH-FIRST SEARCH algorithm. I represented each state as a list where entries are columns, while the values are row numbers (NILs are empty spaces).

### Usage

1. Enter the CLisp console.
2. Call `(load "homework_03.lisp")`, and `(load "bonus_03.lisp")` when testing the bonus problem.
3. Call `(best-first-search [start-name] [goal-name] <strategy>)`, where strategy is optional (it is the heuristic function, choose from `#'heuristic-A*`, `#'heuristic-greedy`, or `#'heuristic-decreasing` for different results). The output of the function is a list containing the path and the fringe.
4. For pretty output, call `(search-print [start-name] [goal-name] <strategy>)`.
5. For the bonus, call `(tree-search <verbose>)`, where verbose is optional (if you like waterfalls, set verbose to `T`). The output is all 92 solutions of the problem in form of a list.