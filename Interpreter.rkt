;Group 2
; - Jonathan Henley
; - Rachel Pavlakovic
; - Shannon Stork

(require "simpleParser.scm")

(define interpret
  (lambda (fileName)
    ()))

(define M_state)

(define M_state_if
  (lambda (cond then else state)
    (if (M_bool cond state)
        (M_state_stmt then state)
        (M_state_stmt else state))))
   
(define M_state_while)
(define M_state_assign)
(define M_state_return)
(define M_state_var)
(define M_state_expr)
(define M_state_stmt)

(define M_value)

(define M_value_var)
(define M_value_assign)
(define M_value_return)
(define M_value_expr)
(define M_value_int
  (lambda (e)
    (cond
      ((number? e) e)
      ((eq? '+ (operator e)) (+ (M_value_int (operand1 e)) (M_value_int (operand2 e))))
      ((eq? '- (operator e)) (- (M_value_int (operand1 e)) (M_value_int (operand2 e))))
      ((eq? '* (operator e)) (* (M_value_int (operand1 e)) (M_value_int (operand2 e))))
      ((eq? '/ (operator e)) (quotient (M_value_int (operand1 e)) (M_value_int (operand2 e))))
      ((eq? '% (operator e)) (remainder (M_value_int (operand1 e)) (M_value_int (operand2 e))))
      (else (error 'badop "Undefine operator")))))

(define operator
  (lambda (e)
    (cadr e)))

(define operand1 car)
(define operand2 caddr)

(define M_bool)