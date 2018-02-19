;Group 2
; - Jonathan Henley
; - Rachel Pavlakovic
; - Shannon Stork

(require "simpleParser.scm")

;Interpret takes a filename and runs the code in the file
(define interpret
  (lambda (fileName)
    (parseRecurse (parser fileName) '(() ()))))

;Parserecurse recurses through parsed code and returns 
(define parseRecurse
  (lambda (statement state)
    (cond
      ((isReturnPresent state) (getReturnIfPresent state))
      (else (parseRecurse (cdr statement) (M_state (car statement) state))))))

;--------------M_state-----------------
;M_state is the main dispatch center which calls different state functions depending on the command in the code statement
(define M_state
  (lambda (x state)
    (cond
      ((null? x) state)
      ((eq? (getKey x) 'if) (M_state_if x state))
      ((eq? (getKey x) 'while) (M_state_while x state))
      ((and (eq? (getKey x) 'var) (not (pair? (operand4 x)))) (addToState (getVar x) 'NULL state))
      ((eq? (getKey x) 'var) (M_state_dec&assign (getVar x) (M_value_expr (operand2 x) state) (addToState (getVar x) 'NULL state)))
      ((eq? (getKey x) 'return) (addToState 'return (M_value x state) state))
      ((eq? (getKey x) '=) (M_state_assign (getVar x) (getExpr x) state))
      ((member (getKey x) (expressions)) (M_state_expr x state ))
      (else state))))

;M_state_if when the code has an 'if command this fucntion breaks it down into condition, then, and else and chooses them based on the condition
(define M_state_if
  (lambda (x state)
    (if (M_value_expr (getCondition x) state)
        (M_state_stmt (getThen x) state)
        (M_state_stmt (getElse x) state))))

;M_state_while when the code has a 'while this function finds its condition and loopbody then recursively calls the loopbody until the condition is no longer true
(define M_state_while
  (lambda (x state)
    (if (M_value_expr (getCondition x) state)
        (M_state_while x (M_state_stmt (getLoopbody x) (M_state_cond (getCondition x) state)))
        (M_state_cond (getCondition x) state))))

;M_state_dec&assign when a variable is declared and assigned in the same line of code, this adds both the variable and value to the state
(define M_state_dec&assign
  (lambda (var expr state)
    (replaceInState var (M_value_expr expr (M_state_expr expr state)) (M_state_expr expr state))))

;M_state_assign when a variable is already declared, and its value is being redefined
(define M_state_assign
  (lambda (var expr state)
    (cond
      ((and (isDeclared var (getVarLis state)) (eq? var expr)) state)
      ((isAssigned var state) (replaceInState var (M_value_expr expr state) state))
      ((isDeclared var (getVarLis state)) (addToState var (M_value_expr expr (M_state_expr expr (removeFromState var state))) (M_state_expr expr (removeFromState var state)))))))

;M_state_expr when M_state doesn't find the key to be if, while, return, etc. this checks the expr to see if it is a statement or value
(define M_state_expr
  (lambda (expr state)
    (cond
      ((null? expr) state)
      ((number? expr) state)
      ((not (list? expr)) state)
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'true)) state)
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'false)) state)
      ((not (pair? (cdr expr))) state)
      (else (M_state_stmt expr state)))))

;takes a statement and determines if 'return or '= are called if not returns state
(define M_state_stmt
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((not (pair? stmt)) state)
      ((eq? (getKey stmt) 'return) (addToState 'return (M_value stmt state) state))
      ((eq? (getKey stmt) '=) (M_state_assign (operand1 stmt) (operand2 stmt) state))
      (else state))))

;returns state after a condition
(define M_state_cond
  (lambda (con state)
    (cond
      ((null? con) state)
      (else state))))

;---------- M_value-----------
;M_value is the main dispatch center for determining the value of code segments
(define M_value
   (lambda (x state)
    (cond
      ((eq? (getKey x) 'var) (M_value_var x state))
      ((eq? (getKey x) 'return) (M_value_return x state))
      ((eq? (getKey x) '=) (M_value_assign x state)) 
      ((member (getKey x) (expressions)) (M_value_expr x state )))))

;returns the value of var
(define M_value_var
  (lambda (var state)
    (getValueFromState var state)))

;returns value of expr
(define M_value_assign
  (lambda (expr state)
    (cond
      ((null? expr))
      ((number? (getExpr expr)) (getExpr expr))
      ((or (eq? (getExpr expr) "true") (eq? (getExpr expr) "false")) (getExpr expr))
      ((pair? (getExpr expr)) (M_value_expr (getExpr expr) state))
      (else (getValueFromState (getExpr expr) state)))))

;takes value of expr and converts #t and #f to true and false
(define M_value_return
  (lambda (expr state)
    (cond
      ((null? expr) expr)
      ((eq? (M_value_expr (operand1 expr) state) #t) 'true)
      ((eq? (M_value_expr (operand1 expr) state) #f) 'false)
      (else (M_value_expr (operand1 expr) state)))))

;determines if expr is a single value, variable or an operation. if so what kind of operation. this returns the value of the expr
(define M_value_expr
  (lambda (expr state)
    (cond
      ((null? expr) expr)
      ((number? expr) expr)
      ((eq? expr #t) #t)
      ((eq? expr #f) #f)
      ((not (list? expr)) (getValueFromState expr state))
      ((and (not (pair? (cdr expr))) (number? (operator expr))) (operator expr))
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'true)) #t)
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'false)) #f)
      ((not (pair? (cdr expr))) (M_value_var (car expr) state))
      ((or (eq? (operator expr) '+)(eq? (operator expr) '-)(eq? (operator expr) '*)(eq? (operator expr) '/)(eq? (operator expr) '%)) (M_value_int expr state))
      ((or (eq? (operator expr) '&&)(eq? (operator expr) '||)(eq? (operator expr) '!)) (M_value_bool expr state))
      ((or (eq? (operator expr) '>)(eq? (operator expr) '<)(eq? (operator expr) '>=)(eq? (operator expr) '<=)(eq? (operator expr) '==)(eq? (operator expr) '!=)) (M_value_comp expr state))
      (else (error badop)))))

;takes an expr that starts with a math symbol and recursively evaluates all of the operations in the expression, returns final value of math expression
(define M_value_int
  (lambda (lis state)
    (cond
      ((number? lis) lis)
      ((and (not (pair? lis)) (isAssigned lis state)) (M_value_var lis state))
      ((eq? '+ (operator lis)) (+ (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((and (eq? '- (operator lis)) (not (pair? (operand4 lis)))) (- (M_value_int (operand1 lis) state)))
      ((and (eq? '- (operator lis)) (pair? (operand4 lis))) (- (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((eq? '* (operator lis)) (* (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((eq? '/ (operator lis)) (quotient (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((eq? '% (operator lis)) (remainder (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      (else (M_value_expr lis state)))))

;takes an expr that starts with a logic symbol and recursively evaluates all of the operations in the expression. if an operator
; is not in the list the code calls M_value_expr in order to determine value of different internal operations, returns final boolean
(define M_value_bool
  (lambda (lis state)
    (cond
      ((eq? lis 'true) #t)
      ((eq? lis 'false) #f)
      ((eq? lis #t) #t)
      ((eq? lis #f) #f)
      ((and (not (pair? lis)) (isAssigned lis state)) (M_value_var lis state))
      ((eq? '&& (operator lis)) (and (M_value_bool (operand1 lis) state) (M_value_bool (operand2 lis) state)))
      ((eq? '|| (operator lis)) (or (M_value_bool (operand1 lis) state) (M_value_bool (operand2 lis) state)))
      ((eq? '! (operator lis)) (not (M_value_bool (operand1 lis) state)))
      (else (M_value_expr lis state)))))

;takes an expr that starts with a comapison symbol and recursively evaluates all of the operations in the expression. if an operator
; is not in the list the code calls M_value_expr in order to determine the value of different internal operations, returns final boolean
(define M_value_comp
  (lambda (lis state)
    (cond
      ((number? lis) lis)
      ((and (not (pair? lis)) (isAssigned lis state)) (M_value_var lis state))
      ((eq? '> (operator lis)) (> (M_value_comp (operand1 lis)  state) (M_value_comp (operand2 lis) state)))
      ((eq? '< (operator lis)) (< (M_value_comp (operand1 lis) state) (M_value_comp (operand2 lis) state)))
      ((eq? '>= (operator lis)) (>= (M_value_comp (operand1 lis) state) (M_value_comp (operand2 lis) state)))
      ((eq? '<= (operator lis)) (<= (M_value_comp (operand1 lis) state) (M_value_comp (operand2 lis) state)))
      ((eq? '== (operator lis)) (eq? (M_value_comp (operand1 lis) state) (M_value_comp (operand2 lis) state)))
      ((eq? '!= (operator lis)) (not (eq? (M_value_comp (operand1 lis) state) (M_value_comp (operand2 lis) state))))
      (else (M_value_expr lis state)))))

;--------------M_bool-----------------
;M_bool checks if bool is true or false, returns boolean
(define M_bool
  (lambda (bool)
    (or (eq? bool #t) (eq? bool #f))))

;-------------- Helper Methods-----------------
;When a return is found, a variable 'return is added to the state with the value of the return's expression
;this method is used to check in parseRecurse if a return has been called and stop program execution
(define isReturnPresent
  (lambda (state)
    (isReturnPresentHelper (getVarLis state))))

;parses through state to find variable 'return, returns true if present false otherwise
(define isReturnPresentHelper
  (lambda (varLis)
    (cond
      ((null? varLis) #f)
      ((eq? 'return (car varLis)) #t)
      (else (isReturnPresentHelper (cdr varLis))))))

;this variable is called in the event isReturnPresent is true, in parseRecurse
;returns the value of the return
(define getReturnIfPresent
  (lambda (state)
    (getValueFromState 'return state)))

;--Error Checking Helpers--
(define isDeclared
  (lambda (var varLis)
    (cond
      ((null? varLis) (error "Undeclared variable"))
      ((eq? (car varLis) var) #t)
      (else (isDeclared var (cdr varLis))))))

(define isAssigned
  (lambda (var state)
    (isAssignedHelper var (getVarLis state))))

(define isAssignedHelper
  (lambda (var varLis)
    (cond
      ((null? varLis) (error "Unassigned variable"))
      ((eq? var (car varLis)) #t)
      (else (isAssignedHelper var (cdr varLis))))))

;--State Helpers--
(define getVarLis
  (lambda (state)
    (car state)))

(define getValLis
  (lambda (state)
    (cadr state)))

(define addToState
  (lambda (var val state)
    (list (cons var (getVarLis state)) (cons val (getValLis state)))))

(define removeFromState
  (lambda (var state)
    (cond
      ((null? (getVarLis state)) state)
      ((eq? (car (getVarLis state)) var) (list (cdr (getVarLis state)) (cdr (getValLis state))))
      (else (list (cons (car (getVarLis state)) (car (removeFromState var (list (cdar state) (cdadr state)))))
                  (cons (car (getValLis state)) (cadr (removeFromState var (list (cdar state) (cdadr state))))))))))

(define getValueFromState
  (lambda (var state)
    (cond
      ((null? (getVarLis state)) (error "Undeclared variable"))
      ((and (eq? (car (getVarLis state)) var)(eq? (car (getValLis state)) 'NULL)) (error "Unassigned variable"))
      ((eq? (car (getVarLis state)) var) (car (getValLis state)))
      (else (getValueFromState var (list (cdar state) (cdadr state)))))))

(define replaceInState
  (lambda (var val state)
    (cond
      ((null? (getVarLis state)) (error "Undeclared variable" ))
      ((eq? (car (getVarLis state)) var) (list (cons var (cdr (getVarLis state))) (cons val (cdr (getValLis state)))))
      (else (list (cons (car (getVarLis state)) (car (replaceInState var val (list (cdar state) (cdadr state)))))
                  (cons (car (getValLis state)) (cadr (replaceInState var val (list (cdar state) (cdadr state))))))))))

;-------------- Abstractions-----------------
(define operator
  (lambda (e)
    (car e)))

(define operand1 cadr)

(define operand2 caddr)

(define operand3 cadddr)

(define operand4 cddr)

(define getCondition cadr)

(define getThen caddr)

(define getElse
  (lambda (line)
    (cond
      ((null? (cdddr line)) '())
      (else (cadddr line)))))

(define getLoopbody caddr)

(define getKey
  (lambda (line)
    (car line)))

(define getVar
  (lambda (line)
    (cadr line)))

(define getExpr
  (lambda (line)
    (caddr line)))

(define expressions
  (lambda ()
    '(+ - * / % < > <= >= == != || && !)))
    
