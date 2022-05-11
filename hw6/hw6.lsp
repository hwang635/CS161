;
; CS161 Spring 2020 HW6 Problem 3: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
; var index = (n-1) * k + c
(defun node2var (n c k)
  (+ c (* (- n 1) k))
)

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
; Call helper fx node2var so that node n gets at least 1
; color c from interval [c, k]
(defun at-least-one-color (n c k)
  (cond
    ((> c k) nil); invalid
    ((= c k) (list (node2var n c k))) ; c = k so only 1 option
    (t (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k)))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the index set {c,c+1,...,k}."
; Get all pairs of negative literals since if we get at most
; 1 colour, there is only 1 positive literal
(defun at-most-one-color (n c k)
    (cond
      ((> c k) nil)
      ((= c k) nil)
      (t (let ((newC (+ c 1)))
          (append (findNegPairs n c newC k) (at-most-one-color n newC k)))
      )
    )
)

; helper fx for at-most-one-color that takes in node n, start s, current
; min index c, and max index k. It finds all pairs of negative literals
; btw index set {c ... k} by calling node2var
(defun findNegPairs (n s c k )
  (cond
    ((> c k) nil)
    (t (cons (getPair n s c k) (findNegPairs n s (+ c 1) k)))
  )
)

; helper fx that gets current pair for findNegPairs, taking in the same
; parameters as findNegPairs
(defun getPair (n s c k)
  (list (- (node2var n s k)) (- (node2var n c k))) ; make negative
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
; Fx takes in node n, max index k, rets list of clauses where node n
; is coloured w/ exactly 1 colour in set {1, 2, ... k} by calling
; at least 1 colour and at most 1 colour starting w/ c = 1, giving 1 colour.
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
; Takes in edge e w/ form (x y), max color index k and returns list of clauses
; where node x, y can't have same colour in set {1, 2, ... k}
(defun generate-edge-clauses (e k)
  (genEdgeHelper (car e) (second e) 1 k)
)

; Helper fx for generate-edge-clauses, takes in 2 nodes n1 n2 that share an 
; edge, min index c, and max index k. n1, n2 can't have same colour
(defun genEdgeHelper (n1 n2 c k)
  (cond
    ((> c k) nil)
    (t (cons (list (car (getPair n1 c c k)) (car (getPair n2 c c k))) (genEdgeHelper n1 n2 (+ c 1) k)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
