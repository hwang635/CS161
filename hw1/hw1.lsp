					; 1. TREE-CONTAINS takes in ordered tree TREE and number N, it checks if N is contained within TREE by recursively checking the left and right subtrees until it reaches base case where TREE is 1 element and that element is compared to N or the case where TREE is nil, meaning all elements in this subtree have been checked and not found to match N. The fx returns T if N is contained in TREE and NIL if N isn't in TREE.

(defun TREE-CONTAINS (N TREE)
  (cond
   ((null TREE) nil) ; base case when tree done
   ((atom TREE) (equal TREE N)) ; base case for 1 elem, check for match
   ((listp TREE) (or (TREE-CONTAINS N (car TREE))
		     (TREE-CONTAINS N (cdr TREE))))
   )
  )

					; 2. TREE-MAX takes in an ordered TREE and returns the max number in TREE, which is the number furthest to the right in the tree list. The fx recursively gets the rightmost element and returns the last rightmost number.

(defun TREE-MAX (TREE)
  (cond
   ((null TREE) nil)
   ((numberp TREE) TREE)
   (t (TREE-MAX (caddr TREE)))
   )
 )

					; 3. TREE-ORDER takes in ordered TREE and returns a post-ordered (left subtree, right subtree, root) traversed list of the numbers in TREE. It recursively goes through and appends to a list the left, right, and middle subtree.

(defun TREE-ORDER (TREE)
  (cond
   ((null TREE) nil)
   ((atom TREE) (list TREE)) ; last 1 elem, return as list
   (t (append (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE)) (TREE-ORDER (second TREE))))
   )
  )
