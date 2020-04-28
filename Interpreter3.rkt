; Sam Ehrenstein
; Samantha Frankum
; Niharika Karnik
#lang racket
(require "classParser.rkt")
; interpret starts the parsing of the 
(define interpret
  (lambda (filename classname)
    (exec-main classname (airquotes-compile filename) initialError (airquotes-compile filename))))

; "Compiles" the code. It actually just creates the global layer, but this functions similarly to compiling.
(define airquotes-compile
  (lambda (filename)
    (compile-classes (parser filename) initialError)))

; Turns all of the classes into a state with class closures
(define compile-classes
  (lambda (parsed-classes throw)
    (if (null? parsed-classes)
      '()
      (cons (doublecons (cadr (car parsed-classes)) (class-closure (cadr (car parsed-classes)) (caddr (car parsed-classes)) (cadddr (car parsed-classes)) initialState throw) initialLayer)
            (compile-classes (cdr parsed-classes) throw)))))

; Creates an instance closure, which is usable as a state layer I think
(define instance-closure
  (lambda (class classes throw)
    (cons class (instance-closure* (cadr (Lookup-closure class classes throw)) throw))))

; Helper function to extract just the variables (we look up the functions from the class)
(define instance-closure*
  (lambda (template throw)
    (cond
      [(null? (car template)) template]
      [(not (list? (unbox (car (cadr template)))))(doublecons (car (car template)) (box (unbox (car (cadr template))))
                                                        (instance-closure* (doublecdr template) throw))]
      [else (instance-closure* (doublecdr template) throw)])))

; Execute the main method
(define exec-main
  (lambda (class classdefs throw type-list)
    (exec-function 'main '() (append (cdr (Lookup-closure class classdefs throw)) classdefs) (lambda (r) r) throw type-list '())))

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
  (lambda (expression state throw type instance)
    (cond
      [(null? expression) (throw "parser: parser should have caught this")]
      [(number?  expression) expression]
      [(eq? 'true expression) 'true]
      [(eq? 'false expression) 'false]
      [(and (symbol? expression) (instantiated? expression state)) (Lookup expression state throw)]
      [(and (symbol? (car expression)) (declared? (car expression) state)) expression]
      [(and (symbol? expression) (declared? expression state)) (throw "variable: Using variable without instantiating it first")]
      [(eq? (operator expression) 'new) (instance-closure (leftoperand expression) state throw)]
      [(eq? (operator expression) 'static-function) (call/cc (lambda (r)(exec-function (cadr expression) (cddr expression) state r throw)))]
      [(eq? (operator expression) 'dot)(Lookup (rightoperand expression) (cdr (Lookup (leftoperand expression) state throw)) throw)]
      [(eq? (operator expression) 'funcall) (call/cc (lambda (r)(exec-with-dot (cadr expression) (cddr expression) state r throw)))]
      [(symbol? expression) (throw "variable: Using variable without declaring it first")]
      [(and (eq? '- (operator expression)) (not (third? expression)) (- (Operate (leftoperand expression) state throw type instance)))]
      [(eq? '+ (operator expression)) (+ (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance))]
      [(eq? '- (operator expression)) (- (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression type instance) state throw type instance))]
      [(eq? '* (operator expression)) (* (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression type instance) state throw type instance))]
      [(eq? '/ (operator expression)) (quotient (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance))]
      [(eq? '% (operator expression)) (remainder (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance))]
      [(equal? '== (operator expression)) (booltoSymbol(eq? (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance)) throw)]
      [(eq? '!= (operator expression)) (booltoSymbol(not (eq? (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance))) throw)] 
      [(eq? '< (operator expression)) (booltoSymbol(< (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance)) throw)]
      [(eq? '> (operator expression)) (booltoSymbol(> (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance)) throw)]
      [(eq? '<= (operator expression))(booltoSymbol(<= (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance)) throw)]
      [(eq? '>= (operator expression)) (booltoSymbol(>= (Operate (leftoperand expression) state throw type instance) (Operate (rightoperand expression) state throw type instance)) throw)]
      [(eq? '&& (operator expression)) (booltoSymbol(and (symboltoBool(Operate (leftoperand expression) state throw type instance) throw) (symboltoBool(Operate (rightoperand expression) state throw type instance) throw)) throw)]
      [(eq? '|| (operator expression)) (booltoSymbol(or (symboltoBool(Operate (leftoperand expression type instance) state throw) throw) (symboltoBool(Operate (rightoperand expression) state throw type instance) throw)) throw)]
      [(eq? '! (operator expression)) (booltoSymbol(not (symboltoBool(Operate (leftoperand expression type instance) state throw) throw)) throw)]
      [else (throw "badop: The operator is not known")]
      )))

(define Lookup-class-closure
  (lambda (name state throw)
    (cdr (Lookup-closure name state throw))))

; Executes a function by closure. We use this so we can look up a function in the class, and then execute it in an instance.
(define exec-with-dot
  (lambda (dot params state return throw)
    (letrec ((instance (Lookup (cadr dot) state throw))
            (closure (Lookup (caddr dot) (Lookup-class-closure (car instance) state throw) throw))
            (class (car instance))
             (body (cadr closure))
             (formal-params (car closure))
             (env-func (caddr closure))
             (environment (cdr (env-func state throw))))
          (if (not (equal? (length params) (- (length formal-params) 1)))
              (throw "function: incorrect number of parameters")
              (return (Mstate body (make-refs environment state formal-params (cons instance params) throw class instance) return
                              (lambda (k) (error 'flow "Breaking outside of while loop")) (lambda (c) c) throw class instance))))))

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
  (lambda (expression state return break continue throw type instance)
    (cond
      [(null? expression) state]
      [(list? (car expression)) (Mstate (cdr expression) (Mstate (car expression) state return break continue throw type instance) return break continue throw type instance)]  ; using car and cdr here because this is literally a list, not a list representing something
      [(and (eq? 'var (operator expression)) (third? expression)) (Add (leftoperand expression) (Operate (rightoperand expression) state throw type instance) state)]
      [(and (equal? '= (operator expression)) (declared? (leftoperand expression) state))(SetValue (leftoperand expression) (Operate (rightoperand expression) state throw type instance) state throw)]
      [(eq? '= (operator expression)) (throw "variable: Using variable out of scope")]
      [(eq? 'var (operator expression)) (Add* (leftoperand expression) state)]
      [(eq? 'begin (operator expression)) (RemoveLayer (Mstate (cdr expression) (AddLayer state) return break continue throw type instance))]
      [else (keyword expression state return break continue throw type instance)])))
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

; Lookup, but for values that are not in boxes (class closures).
(define Lookup-closure
  (lambda (varname state throw)
    (cond
      [(and (layered? state) (declared? varname (toplayer state))) (Lookup-closure varname (toplayer state) throw)]
      [(layered? state) (Lookup-closure varname (cdr state) throw)]
      [(null? (cadr state)) (throw "state: variable has not been initialized")]  ; if instantiated is used in Mstate, should never happen
      [(eq? varname (car (car state))) (car (cadr state))]
      [else (Lookup-closure varname (cons (cdr (car state)) (cons (cdr (cadr state)) '())) throw)])))

; keyword 
(define keyword
  (lambda (expression state return break continue throw type instance)
    (cond
      [(null? (car expression)) (throw "state: keyword invalid")] ; this should never happen since check if keyword prior to calling keyword
      [(eq? 'while (car expression)) (call/cc (lambda (b)(while (cadr expression) (caddr expression) state return b continue throw type instance)))]
      [(eq? 'if (car expression)) (if* (cadr expression) (caddr expression) (cdddr expression) state return break continue throw type instance)]
      [(eq? 'break (car expression)) (break (RemoveLayer state))]
      [(eq? 'continue (car expression)) (continue state)]
      [(eq? 'try (car expression)) (try* (cadr expression) (caddr expression) (cadddr expression) state return break continue throw type instance)]
      [(eq? 'catch (car expression)) (Mstate (cddr expression) state return break continue throw type instance)]
      [(eq? 'finally (car expression)) (Mstate (cdr expression) state return break continue throw type instance)]
      [(eq? 'throw (car expression)) (throw (Operate (cadr expression) state throw))]
      [(eq? 'return (car expression)) (return (Operate (cadr expression) state throw type instance))]
      [(eq? 'funcall (car expression)) (exec-function (cadr expression) (cddr expression) state (lambda (v) state) throw type instance)]
      [(eq? 'class (car expression)) (Add (cadr expression) (class-closure (caddr expression) (cadddr expression) state throw) state)]
      [(eq? 'function (car expression)) (Add (cadr expression) (list (cons 'this (caddr expression)) (cadddr expression)
                                                                    (lambda (s t) (make-env (cadr expression) s t))
                                                                    (lambda (s t) (Lookup-closure (car instance) s t))) state)]
      [(eq? 'static-function (car expression)) (Add (cadr expression) (list (caddr expression) (cadddr expression)
                                                                     (lambda (s t) (make-env (cadr expression) s t))) state)])))


(define pop-toplayer cdr)

; Creates the class closure by running Mstate on the body to create function closures
(define class-closure
  (lambda (class parent body state throw)
    (list parent (toplayer (Mstate body state (lambda (r) r) (lambda (k) (error 'flow "Breaking outside of while loop"))
                               (lambda (c) c) throw class (list class))))))

; Create the state for executing a function from the current state when the function is called.
; We do a cascading recursive list search for the function, first looking for the layer it is in, and then its actual defintion.
; When we hit the function name, the current state contains the function closure and everything that was defined before it.
; This is exactly the same as its scope, so we just return the state.
(define make-env
  (lambda (name state throw)
    (cond
      [(null? state)(throw "state: state should not be empty")]
      [(and (layered? state) (declared? name (toplayer state))) (cons (make-env name (toplayer state) throw) (pop-toplayer state))]
      [(layered? state) (make-env name (pop-toplayer state) throw)]
      [(null? (cadr state)) (throw "state: function or class not in scope")]
      [(eq? name (car (car state))) state]
      [else (make-env name (doublecdr state) throw)])))

;exec-function will execute a function
(define exec-function
  (lambda (name params state return throw type instance)
    (letrec ((closure (Lookup name state throw))
          (body (cadr closure))
          (formal-params (car closure))
          (env-func (caddr closure))
          (environment (env-func state throw)))
          (if (not (equal? (length params) (length formal-params)))
              (throw "function: incorrect number of parameters")
              (return (Mstate body (make-refs environment state formal-params params throw type instance) return
                              (lambda (k) (error 'flow "Breaking outside of while loop")) (lambda (c) c) throw type instance))))))

;make-refs will bind formal parameter names to actual parameter 
(define make-refs
  (lambda (env state fp ap throw type instance) 
    (if (null? fp)
       env
       (make-refs (Add (car fp) (Operate (car ap) state throw type instance) env) state (cdr fp) (cdr ap) throw type instance))))
; try* defines how to handle the keyword 'try
(define try*
  (lambda (try catch finally state return break continue throw type instance)
    (define temp (call/cc (lambda (t) (Mstate try state return break continue t))))
    (if (list? temp)
        (Mstate finally temp return break continue throw type instance)
        (Mstate finally (Mstate (caddr catch) (Add (car (cadr catch)) temp state) return break continue throw type instance)
                return break continue throw type instance))))
; while* defines how to handle the keyword 'while
(define while*
  (lambda (condition body state return break continue throw type instance)
    (if (symboltoBool (Operate condition state throw) throw type instance)
        (while* condition body (Mstate body state return break continue throw type instance) return break continue throw)
        (break state))))
; while defines how to handle the keyword 'while
(define while
  (lambda (condition body state return break continue throw type instance)
    (while condition body (RemoveLayer (call/cc (lambda (c) (while* condition body state return break c throw type instance))))
           return break continue throw type instance)))
; if* defines how to handle the keyword 'if'
(define if*
  (lambda (condition body otherwise state return break continue throw type instance)
    (if (symboltoBool(Operate condition state throw) throw)
        (Mstate body state return break continue throw type instance)
        (if (null? otherwise)
            state
            (Mstate (car otherwise) state return break continue throw type instance)))))
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