#lang racket
#lang racket
; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
;
; This is an example of using abstraction to make the code easier to read and maintain
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'parser "parser should have caught this"))
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '* (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'badop "The operator is not known")))))

; The helper methods to define and abstract away the operator, leftoperand, and rightoperand parts of an expression
(define operator
  (lambda (expression)
    (cadr expression)))

(define leftoperand car)
(define rightoperand caddr)