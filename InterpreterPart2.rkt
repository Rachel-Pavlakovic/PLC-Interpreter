;Group 2
; - Jonathan Henley
; - Rachel Pavlakovic
; - Shannon Stork

(require "simpleParser.scm")

;Interpret takes a filename and runs the code in the file
(define interpret
  (lambda (fileName)
    (parseRecurse (parser fileName) '((() ())) (lambda (v1) v1) (lambda (v2) v2))))

;Parserecurse recurses through parsed code and returns 
(define parseRecurse
  (lambda (statement state break continue)
    (cond
      ((isReturnPresent state) (getReturnIfPresent state))
      (else (parseRecurse (rest statement) (M_state (first statement) state break continue) (lambda (v1) v1) (lambda (v2) v2))))))

;--------------M_state-----------------
;M_state is the main dispatch center which calls different state functions depending on the command in the code statement
(define M_state
  (lambda (exp state break continue)
    (cond
      ((null? exp) state)
      ((eq? (getKey exp) 'if) (M_state_if exp state break continue))
      ((eq? (getKey exp) 'while) (call/cc (lamba (brk) (M_state_while exp state brk continue))))
      ((eq? (getKey exp) 'break) (break (removeLayerFromState state)))
      ((eq? (getKey exp) 'continue) (continue (removeLayerFromState))) ;NEEDS TESTING
      ((eq? (getKey exp) 'try) (M_state_try exp state))
      ((eq? (getKey exp) 'finally) (M_state_finally exp state))
      ((and (eq? (getKey exp) 'var) (not (pair? (restOfRest exp)))) (addToState (getVar exp) 'NULL state)) ;declaration no assignment
      ((eq? (getKey exp) 'var) (M_state_dec&assign (getVar exp) (M_value_expr (operand2 exp) state) (addToState (getVar exp) 'NULL state))) ;declaration with assignment
      ((eq? (getKey exp) 'return) (addToState 'return (M_value exp state) state)) ;assignment without built in declaration
      ((eq? (getKey exp) '=) (M_state_assign (getVar exp) (getExpr exp) state))
      ((eq? (getKey exp) 'begin) (M_state_begin (rest exp) state break continue))
      ((member (getKey exp) (expressions)) (M_state_expr exp state))
      (else state))))

;M_state_if when the code has an 'if command this fucntion breaks it down into condition, then, and else and chooses them based on the condition
(define M_state_if
  (lambda (exp state break continue)
    (if (M_value_expr (getCondition exp) state)
        (if (eq? (getThen exp) 'begin)
            (M_state (getThen exp) state break continue)
            (M_state_stmt (getThen exp) state))
        (if (eq? (getThen exp) 'begin)
            (M_state (getElse exp) state break continue)
            (M_state_stmt (getElse exp) state)))))

;M_state_while when the code has a 'while this function finds its condition and loopbody then recursively calls the loopbody until the condition is no longer true
(define M_state_while
  (lambda (exp state)
    (if (M_value_expr (getCondition exp) state)
        (if (eq? (getLoopbody exp) 'begin)
            (M_state_while exp (M_state (getLoopbody exp) (M_state_cond (getCondition exp) state)))
            (M_state_while exp (M_state_stmt (getLoopbody exp) (M_state_cond (getCondition exp) state))))
        (M_state_cond (getCondition exp) state))))

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
      ((isDeclared var (getVarLis state)) (replaceInState var (M_value_expr expr state) state)))))
;addToState var (M_value_expr expr (M_state_expr expr (removeFromState var state))) (M_state_expr expr (removeFromState var state)

;M_state_expr when M_state doesn't find the key to be if, while, return, etc. this checks the expr to see if it is a statement or value
(define M_state_expr
  (lambda (expr state)
    (cond
      ((null? expr) state)
      ((number? expr) state)
      ((not (list? expr)) state)
      ((and (not (pair? (rest expr))) (eq? (operator expr) 'true)) state)
      ((and (not (pair? (rest expr))) (eq? (operator expr) 'false)) state)
      ((not (pair? (rest expr))) state)
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

;returns the state after a block, starting with begin
(define M_state_begin
  (lambda (block state break continue)
    (removeLayerFromState (parseRecurse block (addLayerToState state) break continue))))

;returns the state after a try block
(define M_state_try
  (lambda (block state)
    (call/cc
     (lambda (break)
       (M_state_try_helper block state break)))))
      
;helper for try       
(define M_state_try_helper
  (lambda (block state break)
    (if (null? block)
      (state)
      (if (catchExists block)
          ((eq? (getKey block) 'throw) (break (M_state_catch (getCatchBlock block) (getVarName block) (cdr block) state)))
          (if (finallyExists block)
              ((eq? (getKey block) 'throw) (break (M_state_finally (getFinally block) state)))
              (M_state block state '() '()))))))
          
;returns the state after a catch block
(define M_state_catch
  (lambda (block var val state)
    (removeLayerFromState (M_state block (addToState var val (addLayerToState state)) '() '()))))

;returns the state after a finally block
(define M_state_finally
  (lambda (block state)
    (removeLayerFromState (M_state block (addLayerToState state) '() '()))))
    
(define catchExists
  (lambda (stmt)
    (cond
      ((null? stmt) #f)
      ((eq? (getKey stmt) 'catch) #t)
      (else (catchExists (rest stmt))))))

(define getCatchBlock
  (lambda (stmt)
    (cond
      ((null? stmt) '())
      ((eq? (getKey stmt) 'catch) (caddr stmt))
      (else (getCatchBlock (cdr stmt))))))

(define finallyExists
  (lambda (stmt)
    (cond
      ((null? stmt) #f)
      ((eq? (getKey stmt) 'finally) #t)
      (else (finallyExists (rest stmt))))))

(define getFinally
  (lambda (stmt)
    (cond
      ((null? stmt) '())
      ((eq? (getKey stmt) 'finally) (caddr stmt))
      (else (getFinally (cdr stmt))))))

(define getVarName
  (lambda (stmt)
    (cond
      ((null? stmt) '())
      ((eq? (getKey stmt) 'catch) (cadr stmt))
      (else (getVarName (cdr stmt))))))

;---------- M_value-----------
;M_value is the main dispatch center for determining the value of code segments
(define M_value
   (lambda (exp state)
    (cond
      ((eq? (getKey exp) 'var) (M_value_var exp state))
      ((eq? (getKey exp) 'return) (M_value_return exp state))
      ((eq? (getKey exp) '=) (M_value_assign exp state)) 
      ((member (getKey exp) (expressions)) (M_value_expr exp state )))))

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
      ((M_bool expr) (M_value_bool expr state))
      ((not (list? expr)) (getValueFromState expr state))
      ((and (not (pair? (rest expr))) (number? (first expr))) (first expr))
      ((and (not (pair? (rest expr))) (eq? (first expr) 'true)) #t)
      ((and (not (pair? (rest expr))) (eq? (first expr) 'false)) #f)
      ((not (pair? (rest expr))) (M_value_var (first expr) state))
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
      ((and (eq? '- (operator lis)) (not (pair? (restOfRest lis)))) (- (M_value_int (operand1 lis) state)))
      ((and (eq? '- (operator lis)) (pair? (restOfRest lis))) (- (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((eq? '* (operator lis)) (* (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((eq? '/ (operator lis)) (quotient (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      ((eq? '% (operator lis)) (remainder (M_value_int (operand1 lis) state) (M_value_int (operand2 lis) state)))
      (else (M_value_expr lis state)))))

;takes an expr that starts with a logic symbol and recursively evaluates all of the operations in the expression. if an operator
;is not in the list the code calls M_value_expr in order to determine value of different internal operations, returns final boolean
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
;is not in the list the code calls M_value_expr in order to determine the value of different internal operations, returns final boolean
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
;M_bool checks if bool is true or false, returns true if boolean or false otherwise
(define M_bool
  (lambda (bool)
    (cond
      ((or (eq? bool #t) (eq? bool #f)) #t)
      ((or (eq? bool 'true) (eq? bool 'false)) #t)
      (else #f))))

;-------------- Helper Methods-----------------
;When a return is found, a variable 'return is added to the state with the value of the return's expression
;this function is used to check in parseRecurse if a return has been called and stop program execution
;TAIL RECURSIVE
(define isReturnPresent
  (lambda (state)
    (isReturnPresentHelper (getVarLis state) (lambda (v) v))))

;parses through state to find variable 'return, returns true if present false otherwise
;TAIL RECURSIVE
(define isReturnPresentHelper
  (lambda (varLis return)
    (cond
      ((null? varLis) (return #f))
      ((eq? 'return (first varLis))(return  #t))
      (else (isReturnPresentHelper (rest varLis) return)))))

;this function is called in the event isReturnPresent is true, in parseRecurse
;returns the value of the return
(define getReturnIfPresent
  (lambda (state)
    (getValueFromState 'return state)))

;--Error Checking Helpers--

;checks to see if a variable has been declared or not
(define isDeclared
  (lambda (var varLis)
    (cond
      ((null? varLis) (error "Undeclared variable"))
      ((eq? (first varLis) var) #t)
      (else (isDeclared var (rest varLis))))))

;checks to see if a varaible has been assigned or not
(define isAssigned
  (lambda (var state)
    (isAssignedHelper var (getVarLis state))))

;parses through state to find input variable if it is present
(define isAssignedHelper
  (lambda (var varLis)
    (cond
      ((null? varLis) (error "Unassigned variable"))
      ((eq? var (first varLis)) #t)
      (else (isAssignedHelper var (rest varLis))))))

;--State Helpers--

;state is stored as a list with two sublists. The first sublist is the variable names, the second is the corresponding variable values (NULL if the variable is unassigned)

;adds a new layer to the state
(define addLayerToState
  (lambda (state)
    (cons '(()()) state)))

;removes the top layer of the state
(define removeLayerFromState
  (lambda (state)
    (rest state)))

;adds a variable and its corresponding value to the state
(define addToState
  (lambda (var val state)
    (cons (addToStateHelper var val (getNextLayer state)) (rest state))))

;removes a value from the state
(define removeFromState
  (lambda (var state)
    (cons (removeFromStateHelper var (getNextLayer state)) (rest state))))

;returns the value of a variable in the state
(define getValueFromState
  (lambda (var state)
    (cond
      ((and (eq? "Undeclared variable" (getValueFromStateHelper var (first state))) (null? (rest state))) (error "Undeclared variable"))
      ((and (eq? "Undeclared variable" (getValueFromStateHelper var (first state))) (not (null? (rest state)))) (getValueFromState var (rest state)))
      ((eq? "Unassigned variable" (getValueFromStateHelper var (first state))) (error "Unassigned variable"))
      (else (getValueFromStateHelper var (first state))))))

;replaces a variable in the state with the given value
(define replaceInState
  (lambda (var val state)
    (cond
      ((and (eq? "Undeclared variable" (getValueFromStateHelper var (first state))) (null? (rest state))) (error "Undeclared variable"))
      ((and (eq? "Undeclared variable" (getValueFromStateHelper var (first state))) (not (null? (rest state)))) (cons (first state) (replaceInState var val (rest state))))
      (else (cons (replaceInStateHelper var val (first state)) (rest state))))))

;gets the list of variable names from the state
(define getVarLis
  (lambda (state)
    (first state)))

;gets the list of variable values from the state
(define getValLis
  (lambda (state)
    (firstOfRest state)))

;adds a variable and its corresponding value to a layer of the state
(define addToStateHelper
  (lambda (var val state)
    (list (cons var (getVarLis state)) (cons val (getValLis state)))))

;removes a variable and its corresponding value to a layer of the state
(define removeFromStateHelper
  (lambda (var state)
    (cond
      ((null? (getVarLis state)) state)
      ((eq? (first (getVarLis state)) var) (list (rest (getVarLis state)) (rest (getValLis state))))
      (else (list (cons (first (getVarLis state)) (first (removeFromState var (list (restOfFirst state) (cdadr state)))))
                  (cons (first (getValLis state)) (firstOfRest (removeFromState var (list (restOfFirst state) (cdadr state))))))))))

;given a variable name, this function returns that variables value from any layer in the state
(define getValueFromStateHelper
  (lambda (var state)
    (cond
      ((null? (getVarLis state)) "Undeclared variable")
      ((and (eq? (first (getVarLis state)) var)(eq? (first (getValLis state)) 'NULL)) "Unassigned variable")
      ((eq? (first (getVarLis state)) var) (first (getValLis state)))
      (else (getValueFromStateHelper var (list (restOfFirst state) (cdadr state)))))))

;replaces an already existing variable value pair with an updated value in the same layer - used for already declared variables that are being assigned or reassigned
(define replaceInStateHelper
  (lambda (var val state)
    (cond
      ((null? (getVarLis state)) "Undeclared variable")
      ((eq? (first (getVarLis state)) var) (list (cons var (rest (getVarLis state))) (cons val (rest (getValLis state)))))
      (else (list (cons (first (getVarLis state)) (first (replaceInStateHelper var val (list (restOfFirst state) (cdadr state)))))
                  (cons (first (getValLis state)) (firstOfRest (replaceInStateHelper var val (list (restOfFirst state) (cdadr state))))))))))

;-------------- Abstractions-----------------

;get operator for math expressions and comparisons
(define operator car)

;get the first operand in a math expression or a comparison
(define operand1 cadr)

;get the second operand in a math expression or a comparison
(define operand2 caddr)

;first and rest already predefined fucntions for car and cdr
;functions as cadr
(define firstOfRest cadr)

;functions as caar
(define firstOfFirst caar)

;functions as cddr
(define restOfRest cddr)

;functions as cdar
(define restOfFirst cdar)

;gets the condition from an if statement or while loop
(define getCondition cadr)

;gets the then clause from an if statement - what happens if the condition is true
(define getThen caddr)

;gets the else clause from an if statement - what happens if the condition is false
(define getElse
  (lambda (line)
    (cond
      ((null? (cdddr line)) '())
      (else (cadddr line)))))

;gets the body of a while loop statement - what happens while the loop condition is true
(define getLoopbody caddr)

;gets the key word for an expression - ex: if, while, return
(define getKey car)

;gets the variable out of an assignment style statement
(define getVar cadr)

;gets the expression out of an assignemnt style statement - what the variable is being assigned to 
(define getExpr caddr)

;valid math and comparison expressions
(define expressions
  (lambda ()
    '(+ - * / % < > <= >= == != || && !)))
    
;Returns the first layer fo the state to check
(define getNextLayer
  (lambda (state)
    (car state))) 
    
