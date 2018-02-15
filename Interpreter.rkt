;Group 2
; - Jonathan Henley
; - Rachel Pavlakovic
; - Shannon Stork

(require "simpleParser.scm")

(define interpret
  (lambda (fileName)
    ()))
    
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

(define M_state)

(define M_state_if
  (lambda (cond then else state)
    (if (M_bool cond state)
        (M_state_stmt then state)
        (M_state_stmt else state))))
   
(define M_state_while)

(define M_state_assign)

(define M_state_var)

(define M_state_expr)

(define M_state_stmt)

(define M_value)

(define M_value_var
  (lambda (var state)

(define M_value_assign)

(define M_value_return
  (lambda (expr state)
    (cond
      ((null? expr) expr)
      (else (M_value_expr expr state)))))

(define M_value_expr
  (lambda (expr state)
    (cond
      ((null? expr) expr)
      ((and (not (pair? (cdr expr))) (number? (operator expr))) (operator expr))
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'true)) #t)
      ((and (not (pair? (cdr expr))) (eq? (operator expr) 'false)) #f)
      ((not (pair? (cdr expr))) (M_value_var expr state))
      ((or (eq? (operator expr) '+)(eq? (operator expr) '-)(eq? (operator expr) '*)(eq? (operator expr) '/)(eq? (operator expr) '%)) (M_value_int expr state))
      ((or (eq? (operator expr) '&&)(eq? (operator expr) '||)(eq? (operator expr) '!)) (M_value_bool expr state))
      ((or (eq? (operator expr) '>)(eq? (operator expr) '<)(eq? (operator expr) '>=)(eq? (operator expr) '<=)(eq? (operator expr) '==)(eq? (operator expr) '!=)) (M_value_comp expr state))
      (else (error badop)))))

(define M_value_int
  (lambda (lis state)
    (cond
      ((number? lis) lis)
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
      ((eq? '&& (operator lis)) (and (M_value_bool (operand1 lis) state) (M_value_bool (operand2 lis) state)))
      ((eq? '|| (operator lis)) (or (M_value_bool (operand1 lis) state) (M_value_bool (operand2 lis) state)))
      ((eq? '! (operator lis)) (not (M_value_bool (operand1 lis) state)))
      (else (M_value_expr lis state)))))

(define M_value_comp
  (lambda (lis state)
    (cond
      ((number? lis) lis)
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
