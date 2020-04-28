; Sam Ehrenstein
; Samantha Frankum
; Niharika Karnik

; TODO 4/22: Make class definitions recursive

#lang racket
(require "classParser.rkt")

; interpret starts the parsing of the 

(define create-classes
  (lambda (filename)
    (Mstate (parser filename) 'exec initialState (lambda (r) r) (lambda (k) (error 'flow "Breaking outside of while loop")) (lambda (c) c) initialError)))

(define exec-function-closure
  (lambda (closure name params state return throw)
    (letrec ((body (cadr closure))
          (formal-params (car closure))
          (type (cadddr closure)))
          (if (not (equal? (length params) (length formal-params)))
              (throw "function: incorrect number of parameters")
<<<<<<< HEAD
              (return (Mstate body state return (lambda (k) (error 'flow "Breaking outside of while loop")) (lambda (c) c) throw))))))
=======
              (return (Mstate body name (make-refs (instance-closure type state throw) type state formal-params params throw) return (lambda (k) (error 'flow "Breaking outside of while loop")) (lambda (c) c) throw))))))
>>>>>>> 701acfbb322f6eee4ec84bcd92bab35bd7d9322a

(define interpret
  (lambda (filename classname)
    (let ((toplevel-state (create-classes filename)))
      (exec-function-closure (Lookup 'main (Lookup classname toplevel-state initialError) initialError) 'exec '() toplevel-state (lambda (r) r) initialError))))

;inital error is the default value of the error continuation
(define initialError
  (lambda (m) (error 'error (valueToString m))))

; An Empty layer
(define initialLayer '(()()))

; The initial value of the state
(define initialState (list initialLayer))

; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
; The following mathematical operations are implemented : +, -, *, /, % (including the unary -),
; the following comparison operators are implemented: ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !.
; Variables may store values of type int as well as true and false. You do not have to detect an error if a program uses a type
; incorrectly (but it is not hard to add the error check). You do not have to implement short-circuit evaluation of && or ||, but you are welcome to do so.
; This is an example of using abstraction to make the code easier to read and maintain
(define Operate
  (lambda (expression type state throw)
    (cond
      [(null? expression) (throw "parser: parser should have caught this")]
      [(number?  expression) expression]
      [(eq? 'true expression) 'true]
      [(eq? 'false expression) 'false]
      [(and (symbol? expression) (instantiated? expression state)) (Lookup expression state throw)]
      [(and (symbol? expression) (declared? expression state)) (throw "variable: Using variable without instantiating it first")]
      [(eq? 'dot (operator expression)) (Lookup (caddr expression) (Lookup (cadr expression) state throw) throw)]
      [(and (eq? 'funcall (car expression)) (list? (cadr expression))) (call/cc (lambda (r) (exec-function-closure (Operate (cadr expression) type state throw) (cadr (cadr expression)) (append (Lookup (cadr (cadr expression)) state throw) (cddr expression)) state r throw)))]
      [(eq? 'funcall (operator expression)) (call/cc (lambda (r)(exec-function (cadr expression) (cddr expression) type state r throw)))]
      [(eq? 'new (operator expression)) (instance-closure (cadr expression) state throw)]
      [(symbol? expression) (throw "variable: Using variable without declaring it first")]
      [(and (eq? '- (operator expression)) (not (third? expression)) (- (Operate (leftoperand expression) type state throw)))]
      [(eq? '+ (operator expression)) (+ (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw))]
      [(eq? '- (operator expression)) (- (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw))]
      [(eq? '* (operator expression)) (* (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw))]
      [(eq? '/ (operator expression)) (quotient (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw))]
      [(eq? '% (operator expression)) (remainder (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw))]
      [(equal? '== (operator expression)) (booltoSymbol(eq? (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw)) throw)]
      [(eq? '!= (operator expression)) (booltoSymbol(not (eq? (Operate (leftoperand expression) state throw) (Operate (rightoperand expression) type state throw))) throw)] 
      [(eq? '< (operator expression)) (booltoSymbol(< (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw)) throw)]
      [(eq? '> (operator expression)) (booltoSymbol(> (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw)) throw)]
      [(eq? '<= (operator expression))(booltoSymbol(<= (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw)) throw)]
      [(eq? '>= (operator expression)) (booltoSymbol(>= (Operate (leftoperand expression) type state throw) (Operate (rightoperand expression) type state throw)) throw)]
      [(eq? '&& (operator expression)) (booltoSymbol(and (symboltoBool(Operate (leftoperand expression) type state throw) throw) (symboltoBool(Operate (rightoperand expression) type state throw) throw)) throw)]
      [(eq? '|| (operator expression)) (booltoSymbol(or (symboltoBool(Operate (leftoperand expression) type state throw) throw) (symboltoBool(Operate (rightoperand expression) type state throw) throw)) throw)]
      [(eq? '! (operator expression)) (booltoSymbol(not (symboltoBool(Operate (leftoperand expression) type state throw) throw)) throw)]
      [else (throw "badop: The operator is not known")]
      )))

; Tests if the list has a third element.
(define third?
  (lambda (lis)
    (pair? (cddr lis))))   

; General description of Mstate:
; When we declare a variable, it gets added to the end of the list of variables, with no corresponding value.
; This means that if we try to call Mvalue on it, it will raise an error, as Lookup will have to exhaust the list of values first.
; This is what we want! declared? will also raise an error if the variable has not been declared yet.
; When we then try to assign a value to a variable, we first remove it from the list of names, and then add its name to the beginning of the
; list of names, and its value to the beginning of the list of values.
; The idea is to maintain separation between the instantiated and uninstantiated variables. We ensure in our Add and Remove functions that this
; is the case.
(define Mstate
  (lambda (expression type state return break continue throw)
    (cond
      [(null? expression) state]
      [(list? (car expression)) (Mstate (cdr expression) type (Mstate (car expression) type state return break continue throw) return break continue throw)]  ; using car and cdr here because this is literally a list, not a list representing something
      [(and (eq? 'var (operator expression)) (third? expression)) (Add (leftoperand expression) (Operate (rightoperand expression) type state throw) state)]
      [(and (equal? '= (operator expression)) (declared? (leftoperand expression) state))(SetValue (leftoperand expression) (Operate (rightoperand expression) type state throw) state throw)]
      [(eq? '= (operator expression)) (throw "variable: Using variable out of scope")]
      [(eq? 'var (operator expression)) (Add* (leftoperand expression) state)]
      [(eq? 'begin (operator expression)) (RemoveLayer (Mstate (cdr expression) type (AddLayer state) return break continue throw))]
      [else (keyword expression type state return break continue throw)])))

; Abstraction is maintained through the use of Add, Add*, Remove, and Lookup functions as the only ways of accessing the state.
; Add is exactly the way we defined in class: Add(name, value, state) -> state with value as the value of name.
; Only used with the = operator (and syntactic sugar like ++, maybe).
(define Add
  (lambda (name value state)
    (cons (list (cons name (car (toplayer state))) (cons (box value) (cadr (toplayer state)))) (cdr state))))

; Gets the top layer
(define toplayer car)

; Adds an emptylayer to the state
(define AddLayer
  (lambda (state)
    (cons initialLayer state)))

; Removes top layer
(define RemoveLayer cdr)

; Checks if the given list is a layered state
(define layered?
  (lambda (lis)
    (and (pair? (car lis)) (list? (car (car lis))))))

; Add* is like Add, except it only adds a name. This corresponds to the var keyword. Since adding only happens through the state, we can control which one we use.
(define Add*
  (lambda (name state)
    (cons (doublecons name (box '()) (toplayer state)) (cdr state))))

; Helper function. Takes two values and a list containing two lists, and returns the list with the values added to each list with cons.
; Also allows Remove to be O(n) instead of O(2^n) without using let.
(define doublecons
  (lambda (v1 v2 lis)
    (cons (cons v1 (car lis)) (cons (cons v2 (cadr lis)) '()))))

; Takes a list containing two lists and returns a list containing the cdr of each list.
(define doublecdr
  (lambda (lis)
    (cons (cdr (car lis)) (cons (cdr (cadr lis)) '()))))

; Check if a variable has been declared.
; Note that the list of values gets thrown out after the first call, as it is not relevant. This avoids the need to handle the case where it is null.
; This also uses recursion. If state has at least one layer (as opposed to being a single layer), we check each layer.
; If state is a single layer, we use the logic from project 1.
(define declared?
  (lambda (name state)
    (cond
      [(null? state) #f]
      [(null? (car state)) #f]
      [(layered? state) (or (declared? name (toplayer state)) (declared? name (cdr state)))]
      [(eq? name (car (car state))) #t]
      [else (declared? name (cons (cdr (car state)) '()))])))

; Sets the value of the variable in the given layer 
(define set-value-in-layer
  (lambda (name value layer throw)
    (cond
      [(null? (car layer)) (throw "variable: Variable not declared")]
      [(eq? (car (car layer)) name) (begin (set-box! (car (cadr layer)) value) layer)]
      [else (doublecons (car (car layer)) (car (cadr layer)) (set-value-in-layer name value (doublecdr layer) throw))])))

; Sets the value of the innermost variable with the given name
(define SetValue
  (lambda (name value state throw)
    (cond
      [(null? state) (throw "variable: Undeclared variable")]
      [(declared? name (toplayer state)) (cons (set-value-in-layer name value (toplayer state) throw) (cdr state))]
      [else (cons (toplayer state) (SetValue name value (cdr state) throw))])))

; Checks if a variable has been instantiated
(define instantiated?
  (lambda (varname state)
    (cond
      [(null? state) #f]
      [(layered? state) (or (instantiated? varname (toplayer state)) (instantiated? varname (cdr state)))]
      [(null? (cadr state)) #f]
      [(and (eq? varname (car (car state))) (not (null? (unbox (car (cadr state)))))) #t]
      [(eq? varname (car (car state))) #f]
      [else (instantiated? varname (cons (cdr (car state)) (cons (cdr (cadr state)) '())))])))

; Returns the value of a variable if it is instantiated, or throws an error if it isn't.
(define Lookup
  (lambda (varname state throw)
    (cond
      [(and (layered? state) (declared? varname (toplayer state))) (Lookup varname (toplayer state) throw)]
      [(layered? state) (Lookup varname (cdr state) throw)]
      [(null? (cadr state)) (throw "state: variable has not been initialized")]  ; if instantiated is used in Mstate, should never happen
      [(eq? varname (car (car state))) (unbox (car (cadr state)))]
      [else (Lookup varname (cons (cdr (car state)) (cons (cdr (cadr state)) '())) throw)])))

; Lookup, but without unboxing. Used to look up closures, which are not in boxes.
(define Lookup-closure
  (lambda (varname closure throw)
    (cond
      [(and (layered? closure) (declared? varname (toplayer closure))) (Lookup varname (toplayer closure) throw)]
      [(layered? closure) (Lookup varname (cdr closure) throw)]
      [(null? (cadr closure)) (throw "state: variable has not been initialized")]  ; if instantiated is used in Mstate, should never happen
      [(eq? varname (car (car closure))) (car (cadr closure))]
      [else (Lookup varname (cons (cdr (car closure)) (cons (cdr (cadr closure)) '())) throw)])))

; keyword 
(define keyword
  (lambda (expression type state return break continue throw)
    (cond
      [(null? (car expression)) (throw "state: keyword invalid")]
      [(eq? 'class (car expression)) (Add (cadr expression) (class-closure (caddr expression) (cdddr expression) (cadr expression) state return throw) state)]
      [(eq? 'while (car expression)) (call/cc (lambda (b)(while (cadr expression) (caddr expression) type state return b continue throw)))]
      [(eq? 'if (car expression)) (if* (cadr expression) (caddr expression) (cdddr expression) type state return break continue throw)]
      [(eq? 'break (car expression)) (break (RemoveLayer state))]
      [(eq? 'continue (car expression)) (continue state)]
      [(eq? 'try (car expression)) (try* (cadr expression) (caddr expression) (cadddr expression) type state return break continue throw)]
      [(eq? 'catch (car expression)) (Mstate (cddr expression) type state return break continue throw)]
      [(eq? 'finally (car expression)) (Mstate (cdr expression) type state return break continue throw)]
      [(eq? 'throw (car expression)) (throw (Operate (cadr expression) type state throw))]
      [(eq? 'return (car expression)) (return (Operate (cadr expression) type state throw))]
      [(and (eq? 'funcall (car expression)) (list? (cadr expression))) (exec-function-closure (Operate (cadr expression) state throw) (append (Lookup (cadr (cadr expression)) state throw) (cddr expression)) state (lambda (v) state) throw)]
      [(eq? 'funcall (car expression)) (exec-function (cadr expression) (cddr expression) type state (lambda (v) state) throw)]
      [(eq? 'function (car expression)) (Add (cadr expression) (list (cons 'this (caddr expression)) (cadddr expression) state type) state)]
      [(eq? 'static-function (car expression)) (Add (cadr expression) (list (caddr expression) (cadddr expression) state type) state)])))
;(static-function main () body)
;(function f () body)
; (funcall (dot a add) 10 2)
  
; Creates the class closure
(define class-closure
  (lambda (parent body type state return throw)
    (if (null? parent)
        (Mstate body type initialState return (lambda (b) (error 'flow "Breaking outside of loop")) (lambda (c) c) initialError)
        (Mstate body type (Add 'super (Lookup (cadr parent) state throw) state) return (lambda (b) (error 'flow "Breaking outside of loop")) (lambda (c) c) initialError))))

; Creates the instance closure, which is just a copy of the class closure
(define instance-closure
  (lambda (class state throw)
    (Lookup class state throw)))

;exec-function will execute a function
(define exec-function
  (lambda (name params type state return throw)
    (letrec ((closure (Lookup name state throw))
          (body (cadr closure))
          (formal-params (car closure))
          (environment (Add name (list formal-params body state) (caddr closure)))
          (type-function (caddr closure)))
          (if (not (equal? (length params) (length formal-params)))
              (throw "function: incorrect number of parameters")
              (return (Mstate body type-function (make-refs environment type-function state formal-params params throw) return (lambda (k) (error 'flow "Breaking outside of while loop")) (lambda (c) c) throw))))))

;make-refs will bind formal parameter names to actual parameters
(define make-refs
  (lambda (env type state fp ap throw) 
    (if (null? fp)
       env
       (make-refs (Add (car fp) (Operate (car ap) type state throw) env) type state (cdr fp) (cdr ap) throw))))

; try* defines how to handle the keyword 'try
(define try*
  (lambda (try catch finally type state return break continue throw)
    (define temp (call/cc (lambda (t) (Mstate try type state return break continue t))))
    (if (list? temp)
        (Mstate finally type temp return break continue throw)
        (Mstate finally type (Mstate (caddr catch) type (Add (car (cadr catch)) temp state) return break continue throw) return break continue throw))))

; while* defines how to handle the keyword 'while
(define while*
  (lambda (condition body type state return break continue throw)
    (if (symboltoBool (Operate condition type state throw) throw)
        (while* condition body (Mstate body type state return break continue throw) return break continue throw)
        (break state))))

; while defines how to handle the keyword 'while
(define while
  (lambda (condition body type state return break continue throw)
    (while condition body type (RemoveLayer (call/cc (lambda (c) (while* condition body type state return break c throw)))) return break continue throw)))

; if* defines how to handle the keyword 'if'
(define if*
  (lambda (condition body otherwise type state return break continue throw)
    (if (symboltoBool(Operate condition type state throw) throw)
        (Mstate body type state return break continue throw)
        (if (null? otherwise)
            state
            (Mstate (car otherwise) type state return break continue throw)))))

; Assumes infix notation
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; Convert boolean to symbol
(define booltoSymbol
  (lambda (bool throw)
    (cond
    [(null? bool) (throw "bool: no value provided")]
    [(eq? bool #t) 'true]
    [(eq? bool #f) 'false]
    [else (throw "bool: Non-boolean argument")])))
    
; Convert symbol to boolean
(define symboltoBool
  (lambda (val throw)
    (cond
      [(null? val) (throw "bool: No argument provided")]
      [(eq? val 'true) #t]
      [(eq? val 'false) #f]
      [else (throw " bool: Non-boolean symbol provided")])))

; Convert value to String
(define valueToString
  (lambda (val)
    (cond
      [(string? val) val]
      [(number? val) (number->string val)])))