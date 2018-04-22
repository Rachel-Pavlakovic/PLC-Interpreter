;Group 2
; - Jonathan Henley
; - Rachel Pavlakovic
; - Shannon Stork

(require "classParser.scm")

;Interpret takes a filename and runs the code in the file
(define interpret
  (lambda (fileName)
    (call/cc
     (lambda (return)
       (parseRecurse (parser fileName) '((() ())) (lambda (v1) (error "Invalid use of break")) (lambda (v2) (error "Invalid use of continue")) return (lambda (v3 v4) (error "Invalid use of throw")))))))

;Parserecurse recurses through parsed code and returns the return value
(define parseRecurse
  (lambda (statement state break continue return throw)
    (cond
      ((null? statement) (error "No return statement"))
      (else (parseRecurse (rest statement) (M_state (first statement) state break continue return throw) break continue return throw)))))

;Parserecurse recurses through parsed code and returns the state after a block of code
(define parseRecurseBlock
  (lambda (statement state break continue return throw)
    (cond
      ((null? statement) state)
      (else (parseRecurseBlock (rest statement) (M_state (first statement) state break continue return throw) break continue return throw)))))


;-----------------------------------------------------------------------------------------------------------------------
;                                            M_state functions
;-----------------------------------------------------------------------------------------------------------------------

;M_state is the main dispatch center which calls different state functions depending on the command in the code statement
(define M_state
  (lambda (exp state break continue return throw)
    (cond
      ((null? exp) state)
      ((eq? (getKey exp) 'if) (M_state_if exp state break continue return throw))
      ((eq? (getKey exp) 'while) (call/cc (lambda (brk) (M_state_while exp state brk continue return throw))))
      ((eq? (getKey exp) 'break) (break (removeLayerFromState state)))
      ((eq? (getKey exp) 'continue) (continue state))
      ((eq? (getKey exp) 'try) (M_state_try (rest exp) state break continue return throw))
      ((eq? (getKey exp) 'throw) (throw (M_value (firstOfRest exp) state break continue return throw) state))
      ((and (eq? (getKey exp) 'var) (not (pair? (restOfRest exp)))) (addToState (getVar exp) 'NULL state)) ;declaration no assignment
      ((eq? (getKey exp) 'var) (M_state_dec&assign (getVar exp) (M_value_expr (operand2 exp) state break continue return throw) (addToState (getVar exp) 'NULL state) break continue return throw)) ;declaration with assignment
      ((eq? (getKey exp) 'return) (return (M_value exp state break continue return throw))) 
      ((eq? (getKey exp) '=) (M_state_assign (getVar exp) (getExpr exp) state break continue return throw)) ;assignment without built in declaration
      ((eq? (getKey exp) 'begin) (M_state_begin (rest exp) state break continue return throw))
      ((eq? (getKey exp) 'function)  (M_state_func exp state break continue return throw))
      ((eq? (getKey exp) 'funcall) (M_state_funcall exp state break continue return throw)) 
      ((member (getKey exp) (expressions)) (M_state_expr exp state))
      (else state))))

;M_state_if when the code has an 'if command this fucntion breaks it down into condition, then, and else and chooses them based on the condition
(define M_state_if
  (lambda (exp state break continue return throw)
    (cond
      ((M_value_expr (getCondition exp) state break continue return throw) (M_state (getThen exp) state break continue return throw))
      ((null? (restOfRestOfRest exp)) state)
      (else (M_state (getElse exp) state break continue return throw)))))

;M_state_while when the code has a 'while this function finds its condition and loopbody then recursively calls the loopbody until the condition is no longer true
(define M_state_while
  (lambda (exp state break continue return throw)
    (cond
      ((M_value_expr (getCondition exp) state break continue return throw) (M_state_while exp (call/cc (lambda (cont) (M_state (getLoopbody exp) state break cont return throw))) break continue return throw))
      (else state))))

;M_state_dec&assign when a variable is declared and assigned in the same line of code, this adds both the variable and value to the state
(define M_state_dec&assign
  (lambda (var expr state break continue return throw)
    (replaceInState var (M_value_expr expr (M_state_expr expr state) break continue return throw) (M_state_expr expr state))))

;M_state_assign when a variable is already declared, and its value is being redefined
(define M_state_assign
  (lambda (var expr state break continue return throw)
    (cond
      ((and (isDeclared var state) (eq? var expr)) state)
      ((isAssigned var state) (replaceInState var (M_value_expr expr state break continue return throw) state))
      ((isDeclared var state) (replaceInState var (M_value_expr expr state break continue return throw) state)))))

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
  (lambda (block state break continue return throw)
     (removeLayerFromState (call/cc (lambda (cont)(parseRecurseBlock block (addLayerToState state) break cont return throw))))))

;returns the state after a try block (may or may not include catch and/or finally)
(define M_state_try
  (lambda (block state break continue return throw)
    (cond
      ((finallyExists block) (parseRecurseBlock (getFinally block) (removeLayerFromState (M_state_try_catch block (addLayerToState state) break continue return throw))break continue return throw))
      (else (M_state_try_catch block (addLayerToState state) break continue return throw)))))

;called by try, handles the case when there is a throw to a catch and when there is not
(define M_state_try_catch
  (lambda (block state break continue return throw)
    (cond
      ((catchExists block) (call/cc (lambda (thrw) (parseRecurseBlock (first block) state break continue return (lambda (v s) (thrw (parseRecurseBlock (getCatchCode block) (addToState (getCatchVarName block) v s) break continue return throw)))))))
      (else (call/cc (lambda (thrw2) (parseRecurseBlock (first block) state break continue return (lambda (v s) (thrw2 s)))))))))

;if the function is the main funtion the block of code is run, otherwise the function information is stored for later use
(define M_state_func
  (lambda (func state break continue return throw)
    (if (eq? (firstOfRest func) 'main)
        (removeLayerFromState (parseRecurseBlock (firstOfRestOfRestOfRest func) (addLayerToState state) break continue return throw))
        (addFunctionToState func state))))

;returns the state after a function is called
(define M_state_funcall
  (lambda (funcall state break continue return throw)
    (call/cc
     (lambda (funcState)
       (append (firstOfRest (stripLayers (getFuncLayers (firstOfRest funcall) state) state)) (removeLayerFromState (parseRecurseBlock (getFuncBody (firstOfRest funcall) state)
                                                (createFuncEnv (getFuncParams (firstOfRest funcall) state) (restOfRest funcall) (first (stripLayers (getFuncLayers (firstOfRest funcall) state) state)) state break continue return throw) break continue (lambda (v) (funcState state)) (lambda (v s) (throw v state)))))))))

;-----------------------------------------------------------------------------------------------------------------------
;                                            M_value functions
;-----------------------------------------------------------------------------------------------------------------------

;M_value is the main dispatch center for determining the value of code segments
(define M_value
   (lambda (exp state break continue return throw)
    (cond
      ((number? exp) exp)
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((not (pair? exp)) (getValueFromState exp state))
      ((eq? (getKey exp) 'var) (M_value_var exp state break continue return throw))
      ((eq? (getKey exp) 'return) (M_value_return exp state break continue return throw))
      ((eq? (getKey exp) '=) (M_value_assign exp state break continue return throw))
      ((eq? (getKey exp) 'funcall) (M_value_func exp state break contnue return throw))
      ((member (getKey exp) (expressions)) (M_value_expr exp state break continue return throw)))))

;returns the value of var
(define M_value_var
  (lambda (var state break continue return throw)
    (getValueFromState var state)))

;returns value of expr
(define M_value_assign
  (lambda (expr state break continue return throw)
    (cond
      ((null? expr))
      ((number? (getExpr expr)) (getExpr expr))
      ((or (eq? (getExpr expr) "true") (eq? (getExpr expr) "false")) (getExpr expr))
      ((pair? (getExpr expr)) (M_value_expr (getExpr expr) state break continue return throw))
      (else (getValueFromState (getExpr expr) state)))))

;takes value of expr and converts #t and #f to true and false
(define M_value_return
  (lambda (expr state break continue return throw)
    (cond
      ((null? expr) expr)
      ((eq? (M_value_expr (operand1 expr) state break continue return throw) #t) 'true)
      ((eq? (M_value_expr (operand1 expr) state break continue return throw) #f) 'false)
      (else (M_value_expr (operand1 expr) state break continue return throw)))))

;determines if expr is a single value, variable or an operation. if so what kind of operation. this returns the value of the expr
(define M_value_expr
  (lambda (expr state break continue return throw)
    (cond
      ((null? expr) expr)
      ((number? expr) expr)
      ((M_bool expr) (M_value_bool expr state break continue return throw))
      ((not (list? expr)) (getValueFromState expr state))
      ((and (not (pair? (rest expr))) (number? (first expr))) (first expr))
      ((and (not (pair? (rest expr))) (eq? (first expr) 'true)) #t)
      ((and (not (pair? (rest expr))) (eq? (first expr) 'false)) #f)
      ((not (pair? (rest expr))) (M_value_var (first expr) state break continue return throw))
      ((or (eq? (operator expr) '+)(eq? (operator expr) '-)(eq? (operator expr) '*)(eq? (operator expr) '/)(eq? (operator expr) '%)) (M_value_int expr state break continue return throw))
      ((or (eq? (operator expr) '&&)(eq? (operator expr) '||)(eq? (operator expr) '!)) (M_value_bool expr state break continue return throw))
      ((or (eq? (operator expr) '>)(eq? (operator expr) '<)(eq? (operator expr) '>=)(eq? (operator expr) '<=)(eq? (operator expr) '==)(eq? (operator expr) '!=)) (M_value_comp expr state break continue return throw))
      ((eq? (operator expr) 'funcall) (M_value_funcall expr state break continue return throw))
      (else (error badop)))))

;takes an expr that starts with a math symbol and recursively evaluates all of the operations in the expression, returns final value of math expression
(define M_value_int
  (lambda (lis state break continue return throw)
    (cond
      ((number? lis) lis)
      ((and (not (pair? lis)) (isAssignedError lis state)) (M_value_var lis state break continue return throw))
      ((eq? '+ (operator lis)) (+ (M_value_int (operand1 lis) state break continue return throw) (M_value_int (operand2 lis) state break continue return throw)))
      ((and (eq? '- (operator lis)) (not (pair? (restOfRest lis)))) (- (M_value_int (operand1 lis) state break continue return throw)))
      ((and (eq? '- (operator lis)) (pair? (restOfRest lis))) (- (M_value_int (operand1 lis) state break continue return throw) (M_value_int (operand2 lis) state break continue return throw)))
      ((eq? '* (operator lis)) (* (M_value_int (operand1 lis) state break continue return throw) (M_value_int (operand2 lis) state break continue return throw)))
      ((eq? '/ (operator lis)) (quotient (M_value_int (operand1 lis) state break continue return throw) (M_value_int (operand2 lis) state break continue return throw)))
      ((eq? '% (operator lis)) (remainder (M_value_int (operand1 lis) state break continue return throw) (M_value_int (operand2 lis) state break continue return throw)))
      (else (M_value_expr lis state break continue return throw)))))

;takes an expr that starts with a logic symbol and recursively evaluates all of the operations in the expression. if an operator
;is not in the list the code calls M_value_expr in order to determine value of different internal operations, returns final boolean
(define M_value_bool
  (lambda (lis state break continue return throw)
    (cond
      ((eq? lis 'true) #t)
      ((eq? lis 'false) #f)
      ((eq? lis #t) #t)
      ((eq? lis #f) #f)
      ((and (not (pair? lis)) (isAssignedError lis state)) (M_value_var lis state break continue return throw))
      ((eq? '&& (operator lis)) (and (M_value_bool (operand1 lis) state break continue return throw) (M_value_bool (operand2 lis) state break continue return throw)))
      ((eq? '|| (operator lis)) (or (M_value_bool (operand1 lis) state break continue return throw) (M_value_bool (operand2 lis) state break continue return throw)))
      ((eq? '! (operator lis)) (not (M_value_bool (operand1 lis) state break continue return throw)))
      (else (M_value_expr lis state break continue return throw)))))

;takes an expr that starts with a comapison symbol and recursively evaluates all of the operations in the expression. if an operator
;is not in the list the code calls M_value_expr in order to determine the value of different internal operations, returns final boolean
(define M_value_comp
  (lambda (lis state break continue return throw)
    (cond
      ((number? lis) lis)
      ((and (not (pair? lis)) (isAssignedError lis state)) (M_value_var lis state break continue return throw))
      ((eq? '> (operator lis)) (> (M_value_comp (operand1 lis)  state break continue return throw) (M_value_comp (operand2 lis) state break continue return throw)))
      ((eq? '< (operator lis)) (< (M_value_comp (operand1 lis) state break continue return throw) (M_value_comp (operand2 lis) state break continue return throw)))
      ((eq? '>= (operator lis)) (>= (M_value_comp (operand1 lis) state break continue return throw) (M_value_comp (operand2 lis) state break continue return throw)))
      ((eq? '<= (operator lis)) (<= (M_value_comp (operand1 lis) state break continue return throw) (M_value_comp (operand2 lis) state break continue return throw)))
      ((eq? '== (operator lis)) (eq? (M_value_comp (operand1 lis) state break continue return throw) (M_value_comp (operand2 lis) state break continue return throw)))
      ((eq? '!= (operator lis)) (not (eq? (M_value_comp (operand1 lis) state break continue return throw) (M_value_comp (operand2 lis) state break continue return throw))))
      (else (M_value_expr lis state break continue return throw)))))

;returns the value of a function call
(define M_value_funcall
  (lambda (funcall state break continue return throw)
    (call/cc
     (lambda (funcReturn)
       (append (firstOfRest (stripLayers (getFuncLayers (firstOfRest funcall) state) state)) (removeLayerFromState (parseRecurseBlock (getFuncBody (firstOfRest funcall) state)
                                                (createFuncEnv (getFuncParams (firstOfRest funcall) state) (restOfRest funcall) (first (stripLayers (getFuncLayers (firstOfRest funcall) state) state)) state break continue return throw) break continue funcReturn (lambda (v s) (throw v state)))))))))

;-----------------------------------------------------------------------------------------------------------------------
;                                            M_bool functions
;-----------------------------------------------------------------------------------------------------------------------

;M_bool checks if bool is true or false, returns true if boolean or false otherwise
(define M_bool
  (lambda (bool)
    (cond
      ((or (eq? bool #t) (eq? bool #f)) #t)
      ((or (eq? bool 'true) (eq? bool 'false)) #t)
      (else #f))))

;-----------------------------------------------------------------------------------------------------------------------
;                                            Helper functions
;-----------------------------------------------------------------------------------------------------------------------

;assigns values to the parameters for a function call
(define assignValuesToParameters
  (lambda (paramLis valLis strippedState state break continue return throw)
    (cond
      ((and (null? paramLis) (not (null? valLis))) (error "Arity mismatch: Too many values passed into function call"))
      ((and (null? valLis) (not (null? paramLis))) (error "Arity mismatch: Not enough values passed into function call"))
      ((and (null? paramLis) (null? valLis)) strippedState)
      (else (assignValuesToParameters (rest paramLis) (rest valLis) (addToState (first paramLis) (M_value (first valLis) state break continue return throw) strippedState) state break continue return throw)))))


;checks to see if there is a catch statement that exists
(define catchExists
  (lambda (stmt)
    (cond
      ((null? stmt) #f)
      ((null? (first stmt)) #f)
      ((eq? (firstOfFirst stmt) 'catch) #t)
      (else (catchExists (rest stmt))))))

;called only when we know that a catch statement exists, returns the block of code that makes up the catch block
(define getCatchBlock
  (lambda (block)
    (restOfFirstOfRest block)))

;called only when we know that a catch statement exists, returns the code that needs to be executed in the catch 
(define getCatchCode
  (lambda (block)
    (firstOfRest (getCatchBlock block))))

;called only when we know that a catch statement exists, returns the variable name that is being fed into the catch block 
(define getCatchVarName
  (lambda (block)
    (firstOfFirst (getCatchBlock block))))

;checks to see if there is a finally after thr try or catch
(define finallyExists
  (lambda (stmt)
    (cond
        ((null? stmt) #f)
        ((null? (rest stmt)) #f)
        ((null? (restOfRest stmt)) #f)
        ((null? (firstOfRestOfRest stmt)) #f)
        (else #t))))

;called only when we know there is going to be a finally block; returns the block of code that makes up the finally
(define getFinally
  (lambda (stmt)
    (firstOfRest (firstOfRestOfRest stmt))))

;gets the name of the variable that is being thrown
(define getVarName
  (lambda (stmt)
    (cond
      ((null? stmt) '())
      ((eq? (getKey stmt) 'catch) (firstOfRest stmt))
      (else (getVarName (rest stmt))))))

;checks to see if a variable is declared in the state- does not return any errors
(define isDeclardInState
  (lambda (var state)
    (cond
      ((isDeclaredMain var state) #t)
      (else #f))))

;-----------------------------------------------------------------------------------------------------------------------
;                                        Error checking helper functions
;-----------------------------------------------------------------------------------------------------------------------

;checks to see if a variable has been declared or not
(define isDeclared
  (lambda (var state)
    (cond
      ((isDeclaredMain var state) #t)
      (else (error "Undeclared variable")))))

;Helper method for determining whether of not a variable has been declared
(define isDeclaredMain
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (rest state)) (isDeclaredHelper var (getVarLis (first state))))
      (else (or (isDeclaredHelper var (getVarLis (first state))) (isDeclaredMain var (rest state)))))))

;Helper method for determining whether of not a variable has been declared
(define isDeclaredHelper
  (lambda (var varLis)
    (cond
      ((null? varLis) #f)
      ((eq? (first varLis) var) #t)
      (else (isDeclaredHelper var (rest varLis))))))

;checks to see if a varaible has been assigned or not
(define isAssigned
  (lambda (var state)
    (cond
      ((isAssignedMain var state) #t)
      (else #f))))

;checks and potentially throws an error for unasigned variables
(define isAssignedError
  (lambda (var state)
    (cond
      ((isAssignedMain var state) #t)
      (else (error "Unassigned Variable")))))

;helper for the isAssigned method
(define isAssignedMain
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (rest state)) (isAssignedHelper var (getVarLis (first state)) (getValLis (first state))))
      (else (or (isAssignedHelper var (getVarLis (first state)) (getValLis (first state))) (isAssignedMain var (rest state)))))))

;parses through state to find input variable if it is present
(define isAssignedHelper
  (lambda (var varLis valLis)
    (cond
      ((null? varLis) #f)
      ((and (eq? var (first varLis)) (not (eq? 'NULL (first valLis)))) #t)
      (else (isAssignedHelper var (rest varLis) (rest valLis))))))

;-----------------------------------------------------------------------------------------------------------------------
;                                          State helper functions
;-----------------------------------------------------------------------------------------------------------------------

;state is stored as a list with two sublists. The first sublist is the variable names, the second is the corresponding variable values (NULL if the variable is unassigned)
(define addClassToState
  (lambda (classCode state break continue return throw)
    (addToState (firstOfRest classCode) (getClassClosure classCode state break continue return throw) state)))
    
;creates the environment for a function
(define createFuncEnv
  (lambda (paramLis valLis strippedState state break continue return throw)
    (assignValuesToParameters paramLis valLis (addLayerToState strippedState) state break continue return throw)))

;adds a function and its closure to the state
(define addFunctionToState
  (lambda (functionCode state)
    (addToState (firstOfRest functionCode) (getFuncClosure functionCode state) state)))

;returns the class closure in the form '((parent class) (instance fields) (functions and closures))
(define getClassClosure
  (lambda (classCode state break continue return throw)
    (list (firstOfRestOfRest classCode) (getInstanceFields (firstOfRestOfRestOfRest classCode) '((()())) break continue return throw) (getFuncClosures (firstOfRestOfRestOfRest classCode) '((()())) break continue return throw))))

(define getInstanceFields
  (lambda (classBody state break continue return throw)
    (cond
      ((null? classBody) state)
      ((eq? (firstOfFirst classBody) 'var) (getInstanceFields (rest classBody) (M_state (first classBody) state break continue return throw) break continue return throw))
      (else (getInstanceFields (rest classBody) state break continue return throw)))))

(define getFuncClosures
  (lambda (classBody state break continue return throw)
    (cond
      ((null? classBody) state)
      ((or (eq? (firstOfFirst classBody) 'function) (eq? (firstOfFirst classBody) 'static-function)) (getFuncClosures (rest classBody) (M_state (first classBody) state break continue return throw) break continue return throw))
      (else getFuncClosure (rest classBody) state break continue return throw))))

;returns the closure in the form '((formal parameter list) (function body) (new state))
(define getFuncClosure
  (lambda (functionCode state)
    (list (firstOfRestOfRest functionCode) (firstOfRestOfRestOfRest functionCode) (getNumLayers state)))) 

;returns the number of layers
(define getNumLayers
  (lambda (state)
    (cond
     ((null? (rest state)) 1)
     (else (+ 1 (getNumLayers (rest state))))))) 
  
;strips layers off of the state
(define stripLayers
  (lambda (num state)
    (cond
      ((eq? num 1) (stripLayersHelper2 state))
      (else (stripLayersHelper (- (getNumLayers state) num) state)))))

;helper method for stripLayers
(define stripLayersHelper
  (lambda (num state)
    (cond
      ((eq? 0 num) (list state '()))
      ;((and (eq? (getNumLevels (cadr (stripLayersHelper (- num 1) (rest state)))) 1) (not (null? (cadr (stripLayersHelper (- num 1) (rest state)))))) (list (first (stripLayersHelper (- num 1) (rest state))) (list (first state))))
      ((null? (firstOfRest (stripLayersHelper (- num 1) (rest state)))) (list (first (stripLayersHelper (- num 1) (rest state))) (first state)))
      (else (list (first (stripLayersHelper (- num 1) (rest state))) (list (cadr (stripLayersHelper (- num 1) (rest state))) (first state)))))))

;second helper method for stripLayers
(define stripLayersHelper2
  (lambda (state)
    (list (first (stripLayersHelper 0 (rest state))) (list (first state)))))

;gets the number of levels
(define getNumLevels
  (lambda (state)
    (cond
      ((null? state) 1)
      ;((null? (rest state)) 1)
      (else (+ 1 (getNumLevels (first state)))))))

;makes a new state by adding the function closure to the top layer of the state
(define getStateFromFunc
  (lambda (functionCode state)
    ;uncomment the following line to add things from the body if needed
    ;(addBody (firstOfRestOfRestOfRest functionCode) (addParams (firstOfRestOfRest functionCode) state))))
    (addParams (firstOfRestOfRest functionCode) state)))

;adds the formal parammeters to the state as 'NULL
(define addParams
  (lambda (paramList state)
    (cond
      ((null? paramList) state)
      (else (addParams (rest paramList) (addToState (first paramList) 'NULL state))))))

;this is probably unnecessary, but keeping it for now
;(define addBody
  ;(lambda (body state)
    ;(cond
      ;((null? (first body)) state)
      ;((eq? (getKey (first body)) 'var) (addBody (rest body) (addToState (getVar body) 'NULL)))
      ;(else (addBody (rest body) state)))))

;gets the formal parameter list
(define getFuncParams
  (lambda (funcName state)
    (first (getValueFromState funcName state))))

;gets the function body
(define getFuncBody
  (lambda (funcName state)
    (firstOfRest (getValueFromState funcName state))))

;gets the new state with the parameters added
(define getFuncLayers
  (lambda (funcName state)
    (firstOfRestOfRest (getValueFromState funcName state)))) 
     
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
    (list (cons var (getVarLis state)) (cons (box val) (getValLis state)))))

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
      ((and (eq? (first (getVarLis state)) var)(eq? (unbox (first (getValLis state))) 'NULL)) "Unassigned variable")
      ((eq? (first (getVarLis state)) var) (unbox (first (getValLis state))))
      (else (getValueFromStateHelper var (list (restOfFirst state) (cdadr state)))))))

;replaces an already existing variable value pair with an updated value in the same layer - used for already declared variables that are being assigned or reassigned
(define replaceInStateHelper
  (lambda (var val state)
    (cond
      ((null? (getVarLis state)) "Undeclared variable")
      ((eq? (first (getVarLis state)) var) (let ((set-result (set-box! (first (getValLis state)) val)))
                                                             (list (getVarLis state) (cons (first (getValLis state)) (rest (getValLis state))))))
      (else (list (cons (first (getVarLis state)) (first (replaceInStateHelper var val (list (restOfFirst state) (cdadr state)))))
                  (cons (first (getValLis state)) (firstOfRest (replaceInStateHelper var val (list (restOfFirst state) (cdadr state))))))))))
;-----------------------------------------------------------------------------------------------------------------------
;                                            Abstractions
;-----------------------------------------------------------------------------------------------------------------------

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

;functions as caddr
(define firstOfRestOfRest caddr)

;fucntions as cdadr
(define restOfFirstOfRest cdadr)

;functions as cdddr
(define restOfRestOfRest cdddr)

;functions as cadddr
(define firstOfRestOfRestOfRest cadddr)

;gets the condition from an if statement or while loop
(define getCondition cadr)

;gets the then clause from an if statement - what happens if the condition is true
(define getThen caddr)

;gets the else clause from an if statement - what happens if the condition is false
(define getElse
  (lambda (line)
    (cond
      ((null? (restOfRestOfRest line)) '())
      (else (firstOfRestOfRestOfRest line)))))

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
    (first state)))