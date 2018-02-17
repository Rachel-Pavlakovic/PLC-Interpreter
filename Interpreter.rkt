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
      ;((null? statement))
      ((null? (cdr statement)) (M_value (car statement) state))
      (else (parseRecurse (cdr statement) (M_state (car statement) '() '() '() '() state))))))
    
(define getVarLis
  (lambda (state)
    (car state)))

(define getValLis
  (lambda (state)
    (cadr state)))

(define addToState
  (lambda (var val state)
    (cons var (getVarLis state))
    (cons val (getValLis state))))

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
      ((null? (getVarLis state)) state)
      ((eq? (car (getVarLis state)) var) (car (getValLis state)))
      (else (getValueFromState var (list (cdar state) (cdadr state)))))))

(define M_state
  (lambda (x cond then else loopbody state)
    (cond
      ((null? x) state)
      ((eq? (getKey x) 'if) (M_state_if cond then else state))
      ((eq? (getKey x) 'while) (M_state_while cond loopbody state))
      ((eq? (getKey x) 'var) (addToState (getExpr x) 'NULL))
      ((eq? (getKey x) 'return) state)
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
  (lambda (cond then else state)
    (if (M_value_bool cond state)
        (M_state_stmt then state)
        (M_state_stmt else state))))
   
(define M_state_while
  (lambda (cond loopbody state)
    (if (M_bool cond state)
        (M_state_while cond loopbody (M_state_stmt loopbody (M_state_cond cond state)))
        (M_state_cond cond state))))

(define M_state_assign
  (lambda (var expr state)
    (addToState var (M_value_expr expr (M_state_expr expr state)) (M_state_expr expr state))
    (removeFromeState var (M_state_expr expr state))))

;(define M_state_var)

(define M_state_expr
  (lambda (expr state)
    (cond
      ((null? expr) state)
      ((and (not (pair? (cdr expr))) (number? (operator expr))) state)
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'true)) state)
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'false)) state)
      ((not (pair? (cdr expr))) state)
      (else (M_state_stmt expr state)))))

(define M_state_stmt
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      ((not (pair? stmt)) state)
      ((eq? (getKey stmt) 'return) state)
      ((eq? (getKey stmt) '=) (M_state_assign stmt))
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
      (else (M_value_expr (operand1 expr) state)))))
      
(define M_value_expr
  (lambda (expr state)
    (cond
      ((null? expr) expr)
      ((number? expr) expr)
      ; deal with variables here
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
      ((not (pair? lis)) (M_value_var lis state))
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
      ((not (pair? lis)) (M_value_var lis state))
      ((eq? '&& (operator lis)) (and (M_value_bool (operand1 lis) state) (M_value_bool (operand2 lis) state)))
      ((eq? '|| (operator lis)) (or (M_value_bool (operand1 lis) state) (M_value_bool (operand2 lis) state)))
      ((eq? '! (operator lis)) (not (M_value_bool (operand1 lis) state)))
      (else (M_value_expr lis state)))))

(define M_value_comp
  (lambda (lis state)
    (cond
      ((number? lis) lis)
      ((not (pair? lis)) (M_value_var lis state))
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