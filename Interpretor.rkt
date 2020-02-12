#lang racket
; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
; The following mathematical operations are implemented : +, -, *, /, % (including the unary -),
; the following comparison operators are implemented: ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !.
; Variables may store values of type int as well as true and false. You do not have to detect an error if a program uses a type
; incorrectly (but it is not hard to add the error check). You do not have to implement short-circuit evaluation of && or ||, but you are welcome to do so.
; This is an example of using abstraction to make the code easier to read and maintain
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '== (operator expression)) if (eq? leftoperand rightoperand)
                                       #t)
      ((eq? '!= (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '< (operator expression)) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '> (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'badop "The operator is not known")))))

; The helper methods to define and abstract away the operator, leftoperand, and rightoperand parts of an expression
(define operator
  (lambda (expression)
    (cadr expression)))

(define leftoperand car)
(define rightoperand caddr)