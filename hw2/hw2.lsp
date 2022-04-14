
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

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

; The boat can carry up to 2 people at a time, so the valid movements are 1 0,
; 0 1, 1 1, 2 0, and 0 2. The succ-fn tries all of these movements and returns
; the combined list of all valid states by calling the next-state since next-state returns
; nil if the next state is invalid.
(defun succ-fn (s)
    (append (next-state s 1 0) (next-state s 0 1) (next-state s 1 1)
        (next-state s 2 0) (next-state s 0 2))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
    (cond
        ((null states) nil) ; stack done, not found
        ((equal (car states) s) t); s matches first item on states stack
        (t (on-path s (cdr states)))
    )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
; The mult-dfs function checks if there are any valid states and stops if there aren't
; any. Otherwise, if there are valid next states it performs a DFS and recursively calls
; itself to continue searching the rest of the states.
(defun mult-dfs (states path)
    (if (null states)
        nil
        (let ((search (mc-dfs (car states) path)))
            (cond
                ((NOT (null search)) search)
                (t (mult-dfs (cdr states) path))
            )
        )
    )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
; The mc-dfs function works by checking if we've reached the final state s, upon which
; we return the path. If we are still looking for the final state, the fx keeps
; searching through the path for the next possible states and calls the mult-dfs
; helper fx.
(defun mc-dfs (s path)
    (cond
        ((final-state s) (cons s path)) ; if final state, done w/ path
        ((on-path s path) nil) ; s in path, return to prevent repeating/looping
        (t (mult-dfs (succ-fn s) (cons s path)))
    )
)

; TESTS
;(bfs '((A (B)) C (D)))
;(bfs '((w x) (y z)))
;(bfs '((w (a b) x (c (d))) (y z)))
;(bfs '())
;(bfs '(A))
;(bfs '(A (B C) (D) (E (F G))))

;(dfs '((A (B)) C (D)))
;(dfs '((w x) (y z)))
;(dfs '((w (a b) x (c (d))) (y z)))
;(dfs '())
;(dfs '(A))
;(dfs '(A (B C) (D) (E (F G))))

;(dfid '((A (B)) C (D)) 3) 
;(dfid '((A (B)) C (D)) 0) 
;(dfid '() 3)
;(dfid '((w (a b) x (c (d))) (y z)) 4)
;(dfid '((w x) (y z)) 2)
;(dfid '(A (B C) (D) (E (F G))) 3)

;(final-state '(3 3 nil))
;(final-state '(3 3 t))
;(final-state '(0 2 t))

;(next-state '(3 3 t) 1 0)
;(next-state '(3 3 t) 0 1)
;(next-state '(3 3 nil) 2 2)
;(next-state '(3 3 t) 2 1)
;(next-state '(2 0 nil) 2 1)
;(next-state '(1 1 nil) 2 2)
;(next-state '(1 0 nil) 2 2)
;(next-state '(1 3 nil) 1 2)

;(succ-fn '(3 3 t))
;(succ-fn '(1 1 t))
;(succ-fn '(2 1 nil))
;(succ-fn '(0 0 nil))
;(succ-fn '(0 3 nil))

;(on-path '(3 1 T) '((3 1 T) (3 2 T)))
;(on-path '(3 1 T) '((3 1 nil) (3 2 T) (1 3 T)))
;(on-path '(2 2 T) '())
;(on-path '(3 1 nil) '((2 1 nil) (2 2 T) (3 2 T)))
;(on-path '(3 2 nil) '((2 1 nil) (2 2 T) (3 2 nil)))

;(mc-dfs '(3 3 T) nil)
;(mc-dfs '(2 2 nil) '((3 1 T) (0 3 NIL) (3 2 T)
; (1 1 NIL) (3 3 T)))