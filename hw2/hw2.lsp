
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