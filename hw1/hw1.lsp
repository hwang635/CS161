; Each function follows the same overall format. Almost every function is recursive and performs the required logic by splitting
; the list argument into sections and recursing until reaching either the base case where the element is null, meaning there's no more
; to split, or the list is 1 element, meaning some action can be performed on the single element and returned. In general, the
; functions move forward/downward through the list using car, cdr, first, second, and third to separate the list until the list
; is a format that can be worked on. There is a more specific description of what each function does in the commment directly above
; each function.

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
   (t (TREE-MAX (caddr TREE))) ; third elem is the most right bc of ordered tree struct
   )
 )

; 3. TREE-ORDER takes in ordered TREE and returns a post-ordered (left subtree, right subtree, root) traversed list of the numbers in TREE. It recursively goes through and appends to a list the left, right, and middle subtree.

(defun TREE-ORDER (TREE)
  (cond
   ((null TREE) nil)
   ((atom TREE) (list TREE)) ; last 1 elem, return as list to avoid dotted pair
   (t (append (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE)) (TREE-ORDER (second TREE))))
   )
  )

; 4. SUB-LIST takes in a list L, integers START and LEN, and returns a sublist taken from L that starts at START and has length LEN. The fx creates the sublist by decrementing START with each recursive call and getting the tail of the list until it reaches the start index, then it decrements LEN and adds onto the sublist until LEN = 0.

(defun SUB-LIST (L START LEN)
  (cond
   ((null L) nil)
   ((= LEN 0) nil)
   ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN)) ; decrement start and cut list
   ((= START 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1)))) ; add onto returned list and decrement len each time
   (t nil)
   )
  )

; 5. SPLIT-LIST takes in list L and returns a combined list made of the 2 halves of L. The fx does that by finding the length of the list, calling SUB-LIST twice with start/length depending on whether the original list length is even or odd, and then returning the combination of the sub-lists.

(defun SPLIT-LIST (L)
  (let ((origLen (length L)))
    (if (evenp origLen)
	(list (SUB-LIST L 0 (/ origLen 2)) (SUB-LIST L (/ origLen 2) (/ origLen 2))) ; even, lists are same length
      (list (SUB-LIST L 0 (/ (+ origLen 1) 2)) (SUB-LIST L (/ (+ origLen 1) 2) (/ (- origLen 1) 2)))) ; odd, first list is longer
    )
  )

; 6. BTREE-HEIGHT takes in a binary tree TREE and returns the height (length of longest path from node to end). The fx does that by recursively traversing through the left and right subtrees of each node and incrementing height each time until eventually returning the greater of the left or right heights, which is the tree height.

(defun BTREE-HEIGHT (TREE)
  (cond
   ((null TREE) 0)
   ((atom TREE) 0)
   (t (let ((leftHeight (+ (BTREE-HEIGHT (car TREE)) 1)) ;++height
	    (rightHeight (+ (BTREE-HEIGHT (cadr TREE)) 1)))
	(if (> leftHeight rightHeight) ; return greater of left/right
	    leftHeight
	  rightHeight)
	))
   )
  )

; 7. LIST2BTREE takes in a list of atoms LEAVES and returns a binary tree. The fx does this by using SPLIT-LIST and splitting every list into 2 sublists if it has more than 2 nodes, which recursively creates a binary tree.

(defun LIST2BTREE (LEAVES)
  (cond
   ((null LEAVES) nil)
   ((= (length LEAVES) 1) (car LEAVES)) ; return only 1 elem
   ((= (length LEAVES) 2) LEAVES) ; 2 elem, return as list
   (t (let ((splitList (SPLIT-LIST LEAVES))) ; get list w/ 2 half lists, recurse on each half list
	(list (LIST2BTREE (car splitList))
	      (LIST2BTREE (second splitList)))))
   )
  )

; 8. BTREE2LIST takes in a binary tree INPUT and returns a list of atoms with all of the tree elements in left to right order. The fx does this by recursing through the tree and appending the first element in the tree, then calling itself to add the first element of the rest of the tree until all elements have been added.
(defun BTREE2LIST (TREE)
  (cond
   ((null TREE) nil)
   ((atom TREE) (list TREE)) ; return single elem as list
   (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (second TREE)))) 
   )
  )

; 9. IS-SAME takes in 2 LISP expressions E1, E2 and checks if they are the same. The fx does this by recursively getting each atom in the list, checking if the elements are atoms at the same time and if each atom is equal to its corresponding element in the other expression, and then calling itself with the rest of the list.
(defun IS-SAME (E1 E2)
  (cond
    ((AND (null E1) (null E2)) t) ; both null, ok
    ((OR (null E1) (null E2)) nil); only 1 null, not the same
    ((AND (numberp E1) (numberp E2)) (= E1 E2)) ; both nums, check if equal
    ((AND (listp E1) (listp E2)) (AND (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))) ; recursively check first elem + rest of list
    (t nil) ; all other cases are false
  )
)

; 10. FLATTEN-APPEND takes in 2 expressions E1, E2 and returns a combined list that has all of the atoms in E2 added onto E1 in order. The fx does this by recursing through E2 and adding it onto E1 when E2 is an atom or null, otherwise it calls itself with the first part of E2 to add the first part onto E1 and then adds the rest of E2 onto the returned list until all of E2 has been traversed through.
(defun FLATTEN-APPEND (E1 E2)
  (cond
    ((null E2) E1) ; E2 done, return E1
    ((atom E2) (append E1 (list E2))) ; single elem, directly add as list (otherwise dotted pair)
    ((listp E2) (FLATTEN-APPEND (FLATTEN-APPEND E1 (car E2)) (cdr E2)))
  )
)