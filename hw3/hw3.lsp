;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; NOTE: CHANGED TO BE (r c)
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 ;(list x row)
		 (list row x) ; CHANGED TO BE (r c) instead of (c r)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END GIVEN HELPER FXS

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; The goal test is when every movable entity (boxes, keeper) is in a goal pos.
; So there can't be any misplaced boxes (box) or keeper (keeper) not on the goal,
; only box-star or keeper-star (on the goal). This fx checks if state s is the
; goal state by verifying that there aren't any boxes/keeper not on a goal.
(defun goal-test (s)
	(cond
		((null s) t) ; done checking, no box/keeper found
		((atom s) (if (or (isBox s) (isKeeper s)) 
						NIL 
						t)) ; single elem, false if box/keeper
		(t (and (goal-test (car s)) (goal-test (cdr s)))) ; check current elem + rest of state
	)
  );end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEXT-STATE + HELPER FXS

; get-square takes in state S, row num r, col num c and returns integer content
; of S at square (r, c). If the square is outside scope, return wall (1). The fx
; checks if the passed in r, c are within the limits of S by checking S's length
; and nested length, then gets the row/element at (r, c) assuming the top left
; corner is (0, 0).
(defun get-square (S r c)
	(cond
		((< r 0) wall) ; under bounds, return wall
		((< c 0) wall)
		((>= r (length S)) wall) ; over bounds, return wall
		((>= c (length (car S))) wall)
		; get (r, c) from row = sublist r, nthcdr does n cdr's then car gets the first elem
		(t (let ((row (car (nthcdr r S))))
			(car (nthcdr c row)))
		)
	)
)

; set-square takes in state S, row num r, col num c, and square content int v.
; Then it returns new state S' where (r, c) has been set to v without modifying
; parameter S. The set-square fx recurses and each time removes a row and 
; decrements r to reach the desired row, then calls the helper function 
; set-square col to actually set the value. set-square-col takes in column num c,
; the current row, the value v and returns the row w/ the value v set. The 
; set-square-col fx works by calling itself and decrementing c and cdr row each
; time to count until the correct position, then it inserts value v at the desired
; position and passes it upward to combine with the start of the row.
(defun set-square-col (c currentRow v)
	(cond
		((< c 0) S) ; out of bounds, return row
		((>= c (length currentRow)) S)
		((= c 0) (cons v (cdr currentRow))) ; at correct col pos, set square to v
		; decrement col, cdr currentRow until @ pos
		(t (cons (car currentRow) (set-square-col (- c 1) (cdr currentRow) v)))
	)
)

(defun set-square (S r c v)
	(cond
		((< r 0) S) ; out of bounds/bad input, return S
		((< c 0) S)
		((>= r (length S)) S)
		((>= c (length (car S))) S)
		; when at correct row, call helper w/ just this row + combine w/ rest of S
		((= r 0) (cons (set-square-col c (car S) v) (cdr S)))
		; decrement r counter when not at correct row, remove start row each time
		(t (cons (car S) (set-square (cdr S) (- r 1) c v)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; try-move and helpers

; get-next-loc takes in row num r, col num c, lengths rowLength, colLength, and
; direction to move in D and returns new position (r', c'). If the (r, c) is
; out of bounds of game state, it returns (-1, -1) but doesn't check (r', c').
(defun get-next-loc (r c rowLength colLength D)
	(cond
		((or (< r 0) (< c 0)) (list -1 -1)) ; out of bounds, return list (-1 -1)
		((or (>= r rowLength) (>= c colLength)) (list -1 -1))
		((equal D 'UP) (list (- r 1) c))
		((equal D 'DOWN) (list (+ r 1) c))
		((equal D 'LEFT) (list r (- c 1)))
		((equal D 'RIGHT) (list r (+ c 1)))
		(t (list -1 -1))
	)
)

; handle-walk takes in current state S, new state newS, keeper coordinates
; (keeperRow, keeperCol) and handles the keeper just moving w/o pushing box 
; by resetting its former spot. it returns changed state S' w/ the reset spot.
(defun handle-walk (S newS keeperRow keeperCol)
	(let ((keeperType (get-square S keeperRow keeperCol)))
		; if current is just keeper, directly move + put blank at former spot
		; if currently keeper star (on goal), replace former spot w/ goal
		(if (isKeeper keeperType)
			(set-square newS keeperRow keeperCol blank)
			(set-square newS keeperRow keeperCol star)
		)
	)
)

; push-box tries to have the keeper push the box by checking if the location next
; to the box can be moved to (not wall), moving the box and setting the value
; to box or box star depending on if there's a goal there, moving the keeper
; where the box formely was, and clearing the former keeper location the same
; as in handle-walk.
; push-box takes in current state S, keeper coordinates (keeperRow, keeperCol),
; coordinates where the box is and keeper will be (nextCoordX, nextCoordY),
; whether the box is a box or box on a goal boxType, and the location the box
; will be pushed to nextBoxLoc. It returns a new state S' if the box is pushed
; or nil if there's a wall so it can't be pushed.
(defun push-box (S keeperRow keeperCol nextCoordX nextCoordY boxType nextBoxLoc)
	; check whether the spot the box will be moved to is valid (not wall or out of bounds)
	(let ((nextBoxLocContains (get-square S (first nextBoxLoc) (second nextBoxLoc)))
			(newKeeperType (if (isBox boxType) keeper keeperstar))) ; check if keeper is on goal after move
		(cond
			((isWall nextBoxLocContains) nil) ; wall, can't be moved there
			((or (isBlank nextBoxLocContains) (isStar nextBoxLocContains)) ; blank or wall, can be walk + push
				(let* ((movedKeeperState (set-square S nextCoordX nextCoordY newKeeperType))
						(clearedAfterMove (handle-walk S movedKeeperState keeperRow keeperCol))
						(newBoxType (if (isBlank nextBoxLocContains) box boxstar))) ; check if box will be on goal
					(set-square clearedAfterMove (first nextBoxLoc) (second nextBoxLoc) newBoxType)
				)
			)
			(t nil)
		)
	)
)

; try-move takes in state S, move direction D and returns the state that results
; from moving keeper in state S in direction D or NIL if the move is invalid (ex:
; wall in that direction). The try-move tries to move in the given direction by
; checking what the next position would be and calling the helper function for just
; moving to a blank/goal spot or moving to a box and trying to push it.
(defun try-move (S D)
	(let* ((keeperLoc (getKeeperPosition S 0))
			(keeperRow (first keeperLoc))
			(keeperCol (second keeperLoc))
			(rowLen (length S))
			(colLen (length (car S)))
			(nextCoord (get-next-loc keeperRow keeperCol rowLen colLen D))
			(nextCoordX (first nextCoord))
			(nextCoordY (second nextCoord))
			(nextCoordContains (get-square S nextCoordX nextCoordY)))

			(cond
				((isWall nextCoordContains) nil) ; if wall, can't move there
				((isBlank nextCoordContains) ; nothing there, just walk
					; move keeper to where the blank is, set keeper there
					(let ((movedState (set-square S nextCoordX nextCoordY keeper)))
							(handle-walk S movedState keeperRow keeperCol)
					)
				)
				((isStar nextCoordContains) ; goal, just walk
					; move keeper to where the goal is, set keeperstar there
					(let ((movedState (set-square S nextCoordX nextCoordY keeperstar)))
							(handle-walk S movedState keeperRow keeperCol)
					)
				)
				; if box/box on goal, try pushing box in dir D
				((or (isBox nextCoordContains) (isBoxStar nextCoordContains))
					(let ((nextBoxLoc (get-next-loc nextCoordX nextCoordY rowLen colLen D)))
						(push-box S keeperRow keeperCol nextCoordX nextCoordY nextCoordContains nextBoxLoc)
					)
				)
				(t nil)
			)
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
; next-states call helper function try-move for each possible direction up, down,
; left, and right and combines the results into a list. Then cleanUpList removes any
; invalid NIL result and returns the list.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	;(x (car pos))
 	;(y (cadr pos))
	;x and y are now the coordinate of the keeper in s. UNUSED
	(result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))))
    
	(cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; Simple fx that always returns constant 0.
(defun h0 (s)
  0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; The fx works by going through the given state s and recursively adds up how
; many boxes aren't on a goal (2), which is how many misplaced boxes. This fx
; is admissible, since it never overestimates the cost of reaching the goal state
; because it counts 1 move per box and each box has to be moved at least once
; to get to the goal state. 
; https://www.engati.com/glossary/admissible-heuristic 
; https://jtra.cz/stuff/lisp/sclr/count.html
(defun h1 (s)
	(cond
		((null s) 0) ; done checking or nothing
		(t (+ (count box (car s)) (h1 (cdr s)))) ; otherwise get # boxes in start row, recursively call for rest of state 
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; h2 and helper fxs
; findItems looks through state S, starting at row r to find the coordinates of 
; all items of given itemType
(defun findItems (S row itemType)
	(cond
		((null s) nil)
		(t (let ((foundCols (findItemsCol (car s) 0 itemType)))
				(if foundCols
						; if found, combine to get (r, c) coords for this row + continue
						(append (combineCoords row foundCols) (findItems (cdr S) (+ row 1) itemType))
						(findItems (cdr S) (+ row 1) itemType)
				)
		))	
	)
)

; combineCoords takes in row r, columns colList and combines them into
; pairs of coordinates (r, c)
(defun combineCoords (r colList)
	(cond
		((null colList) nil)
		(t (append (list (list r (car colList))) ; combine first num, then do rest of list
						(combineCoords r (cdr colList))))
	)
)

; findItemsCol looks through the given row starting at col c to find all items of given type
; locations.
(defun findItemsCol (row c itemType)
	(cond
		((null row) nil)
		; if matching item found, append col index + recurse
		((equal itemType (car row)) (cons c (findItemsCol (cdr row) (+ c 1) itemType)))
		; no box found, look through rest
		(t (findItemsCol (cdr row) (+ c 1) itemType))
	)
)

; findMinDistToGoal takes in the location of a boxCoord (r, c) and a list of goal locs
; Then it finds the manhattan dist from the box to each of the goals and returns
; the shortest distance to a goal found
(defun findMinDistToGoal (boxCoord goalList)
	(cond
		((null boxCoord) nil)
		((null goalList) nil)
		(t (if (= (length goalList) 1)
				(calcManhattanDist (first boxCoord) (second boxCoord) (car goalList))
				(min (calcManhattanDist (first boxCoord) (second boxCoord) (car goalList))
						(findMinDistToGoal boxCoord (cdr goalList)))
		))
	)
)

; calcManhattanDist calculates the manhattan distance (distance btw 2 pts measured
; along axes at right angles) using the absolute value distance between coords
; p1 (r1, c1) and p2 (r2, c2), to be used in the heuristic h2.
; https://xlinux.nist.gov/dads/HTML/manhattanDistance.html
(defun calcManhattanDist (r1 c1 p2)
	(cond
		((null p2) 0)
		(t (let ((x2 (car p2))
				(y2 (second p2)))
			(+ (abs (- r1 x2)) (abs (- c1 y2)))
		))
	)
)

; findDistPerBox takes in a list of box locations (r, c) and goal locations (r, c).
; It calls a helper function to find the shortest distance from a box to a goal
; and returns the sum of all of the distances for every box.
(defun findDistPerbox1 (boxList goalList)
	(cond
		((null boxList) 0)
		((null goalList) 0)
		; just 1 box, find its distance
		((atom boxList) (findMinDistToGoal boxList goalList))
		; list of boxes, find distance for head of list + sum w/ rest of list
		(t (+ (findMinDistToGoal (car boxList) goalList) (findDistPerBox1 (cdr boxList) goalList)))
	)
)

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h2 (s)
	(let ((boxList (findItems s 0 box))
			(goalList (findItems s 0 star)))
			(findDistPerBox boxList goalList)
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

(setq s1 '((1 1 1 1 1)
 (1 4 0 0 1)
 (1 0 2 0 1)
 (1 0 3 0 1)
 (1 0 0 0 1)
 (1 1 1 1 1)
 ))  

;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15) 7 x 6
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

(setq goalstate1'((1 1 1 1 1)
 (1 4 0 0 1)
 (1 0 1 0 1)
 (1 0 1 0 1)
 (1 0 0 0 1)
 (1 5 1 1 1)
 ))

  (setq goalstate2'((1 1 1 1 1)
 (1 4 0 0 1)
 (1 0 1 0 1)
 (1 0 1 5 1)
 (1 7 6 0 1)
 (1 1 1 1 1)
 )) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MORE GIVEN HELPER FXS

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
