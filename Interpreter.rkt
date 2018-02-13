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

(define addValToState)
(define addVarToState)
(define removeFromState)
(define getValueFromState)

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
      ((and (not (list? expr)) (number? expr)) expr)
      ((and (not (list? expr)) (eqv? expr "true")) "true")
      ((and (not (list? expr)) (eqv? expr "false")) "false")
      ((not (list? expr)) (M_value_var expr state))
      (else (M_value_int(expr))))))

(define M_value_int
  (lambda (lis)
    (cond
      ((number? lis) lis)
      ((eq? '+ (operator lis)) (+ (M_value_int (operand1 lis)) (M_value_int (operand2 lis))))
      ((eq? '- (operator lis)) (- (M_value_int (operand1 lis)) (M_value_int (operand2 lis))))
      ((eq? '* (operator lis)) (* (M_value_int (operand1 lis)) (M_value_int (operand2 lis))))
      ((eq? '/ (operator lis)) (quotient (M_value_int (operand1 lis)) (M_value_int (operand2 lis))))
      ((eq? '% (operator lis)) (remainder (M_value_int (operand1 lis)) (M_value_int (operand2 lis))))
      (else (error 'badop "Undefined operator")))))

(define operator
  (lambda (e)
    (car e)))

(define operand1 cadr)

(define operand2 caddr)

(define M_bool)
