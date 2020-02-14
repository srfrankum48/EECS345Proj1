#lang racket
load "simpleParser.rkt"

; interpret starts the parsing of the 
(define interpret
  (lambda (filename)
    (Mvalue 'return (Mstate (parser filename) '()))))

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
      ((and (symbol? expression) (declared? expression state)) (Lookup expression state))
      ((and (symbol? expression) (error 'Mvalue "You have not declared this variable")))
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '== (operator expression)) (eq? leftoperand rightoperand))
      ((eq? '!= (operator expression)) (not (eq? leftoperand rightoperand))) 
      ((eq? '< (operator expression)) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '> (operator expression)) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression))(<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ;((eq? '&& (operator expression)) (bool? (Mvalue (leftoperand expression) state) 
      (else (error 'badop "The operator is not known")))))

; General description of Mstate:
; When we declare a variable, it gets added to the end of the list of variables, with no corresponding value.
; This means that if we try to call Mvalue on it, it will raise an error, as Lookup will have to exhaust the list of values first.
; This is what we want! declared? will also raise an error if the variable has not been declared yet.
; When we then try to assign a value to a variable, we first remove it from the list of names, and then add its name to the beginning of the
; list of names, and its value to the beginning of the list of values.
; The idea is to maintain separation between the instantiated and uninstantiated variables. We ensure in our Add and Remove functions that this
; is the case.
(define Mstate
  (lambda (expression state)
    (cond
      [(null? expression) state]
      [(list? (car expression)) (Mstate (cdr expression) (Mstate (car expression) state))]
      [(and (eq? '= (operator expression)) (declared? (leftoperand expression) state))(Add (leftoperand expression) (Mvalue (rightoperand expression) state) (Remove (leftoperand expression) state))]
      [(eq? '= (operator expression)) (error 'variable "Using variable without declaring it first")]
      [(eq? 'var (keyword expression)) (Add* (leftoperand expression) state)])))

; Abstraction is maintained through the use of Add, Add*, Remove, and Lookup functions as the only ways of accessing the state.
; Add is exactly the way we defined in class: Add(name, value, state) -> state with value as the value of name.
; Only used with the = operator (and syntactic sugar like ++, maybe).
(define Add
  (lambda (name value state)
    (cons (cons name (car state)) (cons (cons value (cadr state)) '()))))

; Add* is like Add, except it only adds a name. This corresponds to the var keyword. Since adding only happens through the state, we can control which one we use.
(define Add*
  (lambda (name state)
    (cons (append (car state) (cons name '())) (cons (cadr state) '()))))

; Helper function. Takes two values and a list containing two lists, and returns the list with the values added to each list with cons.
; Also allows Remove to be O(n) instead of O(2^n) without using let.
(define doublecons
  (lambda (v1 v2 lis)
    (cons (cons v1 (car lis)) (cons (cons v2 (cadr lis)) '()))))

; Takes a list containing two lists and returns a list containing the cdr of each list.
(define doublecdr
  (lambda (lis)
    (cons (cdr (car lis)) (cons (cdr (cadr lis)) '()))))

; Removes a variable and its value from the state.
; Right now, it will return the list without the variable and its value whether or not it is declared or initialized.
; Possible change is to create an error if (car state) is ever null, as that means we're trying to remove a variable that
; hasn't been declared. Only problem is that if we want to handle re-declaration (i.e. "var x" twice), this would break.
; It also mixes state operations with error messages, so maybe not the best idea.
(define Remove
  (lambda (name state)
    (cond
      [(null? (car state)) '(()())]
      [(and (eq? (car (car state)) name) (null? (cadr state))) (cons (cdr (car state)) '(()))]
      [(eq? (car (car state)) name) (doublecdr state)]
      [(null? (cadr state)) (cons (car (Remove name (cons (cdr (car state)) '(())))) '(()))]
      [else (doublecons (car (car state)) (car (cadr state)) (Remove name (doublecdr state)))])))

; Check if a variable has been declared.
; Note that the list of values gets thrown out after the first call, as it is not relevant. This avoids the need to handle the case where it is null.
(define declared?
  (lambda (name state)
    (cond
      [(null? (car state)) #f]
      [(eq? name (car (car state))) #t]
      [else (declared? name (cons (cdr (car state)) '()))])))

; Checks if a variable has been instantiated
(define instantiated?
  (lambda (varname state)
    (cond
      [(null? (cadr state)) #f]
      [(eq? varname (car (car state))) #t]
      [else (instantiated? varname (cons (cdr (car state)) (cons (cdr (cadr state)) '())))])))

; Returns the value of a variable if it is instantiated, or throws an error if it isn't.
(define Lookup
  (lambda (varname state)
    (cond
      [(null? (cadr state)) (error 'state "variable has not been initialized")]  ; if instantiated is used in Mstate, should never happen
      [(eq? varname (car (car state))) (car (cadr state))]
      [else (Lookup varname (cons (cdr (car state)) (cons (cdr (cadr state)) '())))])))

; Assumes infix notation
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; Get the keyword from a statement. Probably the same as operator, but maybe not.
(define keyword car)