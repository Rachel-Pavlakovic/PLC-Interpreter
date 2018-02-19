;Group 2
; - Jonathan Henley
; - Rachel Pavlakovic
; - Shannon Stork

(require "simpleParser.scm")

(define interpret
  (lambda (fileName)
    (parseRecurse (parser fileName) '(() ()))))

(define parseRecurse
  (lambda (statement state)
    (cond
      ((isReturnPresent state) (getReturnIfPresent state))
      (else (parseRecurse (cdr statement) (M_state (car statement) state))))))

(define isReturnPresent
  (lambda (state)
    (isReturnPresentHelper (getVarLis state))))

(define isReturnPresentHelper
  (lambda (varLis)
    (cond
      ((null? varLis) #f)
      ((eq? 'return (car varLis)) #t)
      (else (isReturnPresentHelper (cdr varLis))))))
      
(define getReturnIfPresent
  (lambda (state)
    (getValueFromState 'return state)))
    
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

(define M_state_if
  (lambda (x state)
    (if (M_value_expr (getCondition x) state)
        (M_state_stmt (getThen x) state)
        (M_state_stmt (getElse x) state))))

(define M_state_while
  (lambda (x state)
    (if (M_value_expr (getCondition x) state)
        (M_state_while x (M_state_stmt (getLoopbody x) (M_state_cond (getCondition x) state)))
        (M_state_cond (getCondition x) state))))

(define getCondition cadr)
(define getThen caddr)
(define getElse
  (lambda (line)
    (cond
      ((null? (cdddr line)) '())
      (else (cadddr line)))))
(define getLoopbody caddr)

(define M_state_dec&assign
  (lambda (var expr state)
    (replaceInState var (M_value_expr expr (M_state_expr expr state)) (M_state_expr expr state))))

(define M_state_assign
  (lambda (var expr state)
    (cond
      ((and (isDeclared var (getVarLis state)) (eq? var expr)) state)
      ((isAssigned var state) (replaceInState var (M_value_expr expr state) state))
      ((isDeclared var (getVarLis state)) (addToState var (M_value_expr expr (M_state_expr expr (removeFromState var state))) (M_state_expr expr (removeFromState var state)))))))

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

(define M_state_stmt
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((not (pair? stmt)) state)
      ((eq? (getKey stmt) 'return) (addToState 'return (M_value stmt state) state))
      ((eq? (getKey stmt) '=) (M_state_assign (operand1 stmt) (operand2 stmt) state))
      (else state))))

(define M_state_cond
  (lambda (con state)
    (cond
      ((null? con) state)
      (else state))))

(define M_value
   (lambda (x state)
    (cond
      ((eq? (getKey x) 'var) (M_value_var x state))
      ((eq? (getKey x) 'return) (M_value_return x state))
      ((eq? (getKey x) '=) (M_value_assign x state)) 
      ((member (getKey x) (expressions)) (M_value_expr x state )))))

(define M_value_var
  (lambda (var state)
    (getValueFromState var state)))

(define M_value_assign
  (lambda (expr state)
    (cond
      ((null? expr))
      ((number? (getExpr expr)) (getExpr expr))
      ((or (eq? (getExpr expr) "true") (eq? (getExpr expr) "false")) (getExpr expr))
      ((pair? (getExpr expr)) (M_value_expr (getExpr expr) state))
      (else (getValueFromState (getExpr expr) state)))))

(define M_value_return
  (lambda (expr state)
    (cond
      ((null? expr) expr)
      ((eq? (M_value_expr (operand1 expr) state) #t) 'true)
      ((eq? (M_value_expr (operand1 expr) state) #f) 'false)
      (else (M_value_expr (operand1 expr) state)))))
      
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


(define operator
  (lambda (e)
    (car e)))

(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define operand4 cddr)
    

(define M_bool
  (lambda (bool)
    (or (eq? bool #t) (eq? bool #f))))