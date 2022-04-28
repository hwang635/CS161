;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

;; RESOURCES
; SAT solver to test expected output: 
; https://jgalenson.github.io/research.js/demos/minisat.html

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
; sat? takes in n (# of variables), delta (CNF represented as list of lists)
; and calls the performBacktracking function that performs backtracking to
; solve the CNF SAT problem.
(defun sat? (n delta)
  (performBacktracking n delta '()) ; call w/ empty list to start
)

; performBacktracking recursively performs backtracking to search for
; assignments that will satisfy the given CNF delta. It takes in n (# of vars),
; delta (CNF rep as list of lists), and assignedVars (list of vars that have
; been tried + assigned values). If the variables are all valid for the CNF and
; have all been assigned, the assignedVars list is returned as the solution.
; Otherwise the function returns false if the list is invalid and tries
; another variable value until every variable has been gone through.
(defun performBacktracking (n delta assignedVars)
  (let ((isValid (checkValidCNF assignedVars delta))) ; each step check if CNF still valid
    (cond
      ((null isValid) nil) ; is invalid, return nil + don't continue on this path
      ((> (length assignedVars) n) nil) ; list too longer (> num of vars), return nil + stop
      ; valid and all vars have been assigned (list has n vars, which is all of them)
      ((and (not (null isValid)) (= (length assignedVars) n)) assignedVars)
      ; OK, try next variable w/ both options T/F
      (t (or (performBacktracking n delta (assignNextVar assignedVars 'TRUE))
              (performBacktracking n delta (assignNextVar assignedVars 'FALSE)))
      )
    )
  )
)

; getNextVar takes in m (next var place) and value (T or F) and assigns
; +/- m to the next variable. helper fx for assignNextVar
(defun getNextVar (nextVarNum value)
  (if (equal value 'TRUE)
      (list nextVarNum) ; if true, return +m to rep mth var
      (list (- nextVarNum)) ; if false, return -m to rep mth var
  )
)

; assignNextVar takes in assignedVars (current list of assigned variables)
; and value (whether next var sb T/F), and calls a helper function to try 
; both values T/F for the next x+1 variable. It returns a new list w/
; assignedVars + the next var.
(defun assignNextVar (assignedVars value)
  (let* ((nextVarNum (+ (length assignedVars) 1)) ; get m
          (posVal (getNextVar nextVarNum 'TRUE)) ; get T/F version of mth var
          (negVal (getNextVar nextVarNum 'FALSE)))
          (if (equal value 'TRUE) ; add T/F val based on flag
              (append assignedVars posVal)
              (append assignedVars negVal)
          )
  )
)

; checkValidVar checks if the assignedVars works for 1 variable, it's a helper
; fx for checkValidClause.
; It takes in the list of assignedVars and the variable to check;
(defun checkValidVar (assignedVars var)
  (cond
    ((null var) nil)
    ((null assignedVars) t)
    (t (let ((curr (car assignedVars)))
          (cond
            ((null curr) t)
            ((= var curr) t) ; return true if var = assignment
            ((= (abs var) (abs curr)) nil) ; negative, ret false
            (t (checkValidVar (cdr assignedVars) var)) ; not @ right var yet
          )
        )
    )
  )
)

; checkValidClause checks if the assignedVars satisfy a clause by 
; recursively checking each variable.
; It takes in the list of assignedVars and the clause to check.
(defun checkValidClause (assignedVars clause)
  (cond
    ((null clause) nil) ; went through clause + no true found
    ; stop when find var in clause that is true, otherwise cont through clause
    (t (or (checkValidVar assignedVars (car clause)) 
            (checkValidClause assignedVars (cdr clause)))
    )
  )
)

; checkValidCNF checks if the list of assignedVars is valid for the CNF delta.
; It recursively checks each clause and returns false if any clause isn't valid.
(defun checkValidCNF (assignedVars delta)
  (cond
    ((null delta) t) ; done, didn't find anything wrong
    ; all clauses must be true
    (t (and (checkValidClause assignedVars (car delta)) 
            (checkValidCNF assignedVars (cdr delta))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))