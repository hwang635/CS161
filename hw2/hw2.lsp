
; 1. BFS performs breadth-first search of a tree. It takes in TREE which is a list rep
; of the tree and returns 1 top-level list of nodes as visited by left to right BFS.
; The BFS fx works by recursively traversing through the list with 1st base case when TREE
; is null, meaning that everything has been traversed through and we stop, and 2nd base case
; when TREE is null, meaning the current element is a leaf node and can be directly added to
; the front of the list and then the rest of the list is created by doing BFS on the unadded
; rest of the TREE (left to right order). If TREE is a list, that means we're looking at a list
; of child nodes so we separate the first element from the rest of the list, add it to the back
; of the "queue", do BFS on the manipulated list, and recursively continue until the tree has
; been traversed through.

(defun BFS (TREE)
    (cond
        ((null TREE) nil) ; no more on this path, done
        ((atom (car TREE)) (append (list (car TREE)) (BFS (cdr TREE)))) ; if leaf, append to list + do BFS to recursively add rest of list
        ((list TREE) (BFS (append (cdr TREE) (car TREE)))) ; if list of child nodes, unwrap first elem 1x + add to end of q + do BFS
        (t (nil)) ; should never be reached
    )
)

; 2. DFS performs depth-first search of a TREE. It takes in a single argument TREE that is a list
; rep of a tree and returns 1 list in order of a left to right DFS traversal. The DFS fx works by
; taking in list TREE and checking if it has reached the end, is a leaf node (atom), or list of children.
; If the current element is an atom, we've reached the last node in the path and directly return it
; as a list to pass it upwards to the previous call and get appended to the rest of the list. If
; the current element is a list, we separate the leftmost element from the rest and call DFS on both to
; recursively unwrap each section until everything is broken down into single elements and passed upwards
; to be combined into a list and returned.
(defun DFS (TREE)
    (cond
        ((null TREE) nil)
        ((atom TREE) (list TREE)) ; reached deepest node, return
        ((list TREE) (append (DFS (car TREE)) (DFS (cdr TREE)))) ; recursively unwrap from left to right and combine after passed up
        (t (nil))
    )
)

; 3. DFID takes in 2 arguments: TREE, the list representation of the tree, and MAXDEPTH, an integer
; that's the max depth of the tree. It returns 1 top-level list where the nodes are in order of a right to left
; DF interative-deepening search (nodes that are visited multiple times are returned multiple times).
; The DFID fx works by stopping when the tree is null or the max depth has been reached; otherwise it
; recursively calls itself with a decremented depth and DFShelper to do the actual DFS, then appends
; together the results of each call of DFShelper.
(defun DFID (TREE MAXDEPTH)
    (cond
        ((null TREE) nil)
        ((< MAXDEPTH 0) nil)
        (t (append (DFID TREE (- MAXDEPTH 1)) (DFShelper TREE MAXDEPTH)))
    )
)

; DFS helper takes in TREE, a list representation of the tree, and CURRDEPTH, the depth limit for DFS 
; traversal. It returns a list with nodes up to the depth limit in right to left order in a similar way
; as the #2 DFS function but decrements the CURRDEPTH variable each time an element is extracted to keep
; track of the depth limit of the search. 
(defun DFShelper (TREE CURRDEPTH)
    (cond
        ((null TREE) nil)
        ((< CURRDEPTH 0) nil) ; past depth limit, stop
        ((atom TREE) (list TREE)) ; reached deepest node, return
        ((list TREE) 
            (append (DFShelper (cdr TREE) CURRDEPTH) (DFShelper (car TREE) (- CURRDEPTH 1)))) ; recursively unwrap from right to left, --depth when unwrap
        (t (nil))
    )
)

; Depth-first solver for River-Boat problem using hw2_skeleton

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise. The final-state fx compares S against
; (3 3 NIL), returns true if they're the same and false otherwise.
(defun final-state (s)
    (if (equal s '(3 3 NIL))
        t
        nil
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
; The next-state fx works by storing the current number of X's and O's on the
; current side by getting them from the s list, subtracting from the other side to
; get the number of Xs and Os on the other side, and adding the arguments m, c to
; get the projected Xs and Os. Then it checks whether there are enough X/Os on the current
; side to move the desired amounts, if there will be more Os than Xs, and returns nil
; if the projected move is invalid and the next state inside a list wrapper only if valid.
(defun next-state (s m c)
    (let ((currentM (first s)) ; get num X, 0s on this side right now
        (currentC (second s))
        (otherM (- 3 (first s))) ; 3 - this side is num X, 0s on other side
        (otherC (- 3 (second s)))
        (newHereM (- (first s) m)) ; num X, Os on this side if moved
        (newHereC (- (second s) c))
        (otherSide (not (third s))))

        (cond
            ((< currentM m) nil) ; not enough X or Os to move
            ((< currentC c) nil)
            ((AND (< (+ otherM m) (+ otherC c)) (> (+ otherM m) 0)) nil) ; if more Os than Xs after move, invalid
            ((AND (< newHereM newHereC) (> newHereM 0)) nil)
            (t (list (list (+ otherM m) (+ otherC c) otherSide)))
        )   
    )
)

; TESTS
(next-state '(3 3 t) 1 0)
(next-state '(3 3 t) 0 1)
(next-state '(3 3 nil) 2 2)
(next-state '(3 3 t) 2 1)
(next-state '(2 0 nil) 2 1)
(next-state '(1 1 nil) 2 2)
(next-state '(1 0 nil) 2 2)
(next-state '(1 3 nil) 1 2)
