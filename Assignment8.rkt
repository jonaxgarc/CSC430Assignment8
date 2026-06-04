#lang typed/racket
(require typed/rackunit)

;; Full core project implemented.
;; This is a small C-inspired language in Typed Racket. It follows the
;; Assignment 4 interpreter shape, but uses local var bindings to feel C-like.

(define-type ExprC (U NumC IdC StringC IfC FnC CallC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct StringC ([str : String]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct FnC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct CallC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

(define-type Value (U NumV BoolV StringV CloV PrimV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StringV ([str : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)

(struct Binding ([name : Symbol] [val : Value]) #:transparent)
(define-type Env (Listof Binding))

(: top-env Env)
(define top-env
  (list (Binding '+ (PrimV '+))
        (Binding '- (PrimV '-))
        (Binding '* (PrimV '*))
        (Binding '/ (PrimV '/))
        (Binding '<= (PrimV '<=))
        (Binding 'equal? (PrimV 'equal?))
        (Binding 'substring (PrimV 'substring))
        (Binding 'strlen (PrimV 'strlen))
        (Binding 'error (PrimV 'error))
        (Binding 'true (BoolV #t))
        (Binding 'false (BoolV #f))))

;;;; reserved? function
; Purpose: returns true when a symbol is reserved syntax instead of a variable name.
(: reserved? (Symbol -> Boolean))
(define (reserved? sym)
  (or (symbol=? sym 'if)
      (symbol=? sym '=)
      (symbol=? sym 'var)
      (symbol=? sym 'fn)
      (symbol=? sym '->)
      (symbol=? sym 'do)))

;;;; has-duplicates? function
; Purpose: returns true when a list of parameter or binding names repeats a name.
(: has-duplicates? ((Listof Symbol) -> Boolean))
(define (has-duplicates? names)
  (match names
    ['() #f]
    [(cons first-name rest-names)
     (or (not (false? (member first-name rest-names)))
         (has-duplicates? rest-names))]))

;;;; parse-binding-name function
; Purpose: extracts the variable name from one var binding.
(: parse-binding-name (Sexp -> Symbol))
(define (parse-binding-name binding)
  (match binding
    [(list (? symbol? name) '= rhs)
     (if (reserved? name)
         (error 'parse "C430: invalid var binding name: ~e" binding)
         name)]
    [_ (error 'parse "C430: invalid var binding: ~e" binding)]))

;;;; parse-binding-val function
; Purpose: parses the value expression from one var binding.
(: parse-binding-val (Sexp -> ExprC))
(define (parse-binding-val binding)
  (match binding
    [(list (? symbol? name) '= rhs) (parse rhs)]
    [_ (error 'parse "C430: invalid var binding: ~e" binding)]))

;;;; parse function
; Purpose: parses one C-inspired source expression into an ExprC AST node.
(: parse (Sexp -> ExprC))
(define (parse s)
  (match s
    [(? real? n) (NumC n)]
    [(? string? str) (StringC str)]
    [(? symbol? sym)
     (if (reserved? sym)
         (error 'parse "C430: invalid identifier: ~e" s)
         (IdC sym))]
    [(list 'if test then else)
     (IfC (parse test) (parse then) (parse else))]
    [(list 'fn (list (? symbol? params) ...) '-> body)
     (define param-list (cast params (Listof Symbol)))
     (cond
       [(ormap reserved? param-list)
        (error 'parse "C430: invalid function parameter: ~e" s)]
       [(has-duplicates? param-list)
        (error 'parse "C430: duplicate function parameter: ~e" s)]
       [else (FnC param-list (parse body))])]
    [(list 'var (list bindings ...) 'do body)
     (define names (map parse-binding-name bindings))
     (define vals (map parse-binding-val bindings))
     (if (has-duplicates? names)
         (error 'parse "C430: duplicate var binding: ~e" s)
         (CallC (FnC names (parse body)) vals))]
    [(list fun args ...)
     (CallC (parse fun) (map parse args))]
    [_ (error 'parse "C430: invalid expression: ~e" s)]))

;;;; lookup function
; Purpose: returns the value bound to a name in the current environment.
(: lookup (Symbol Env -> Value))
(define (lookup name env)
  (match env
    ['() (error 'lookup "C430: unbound identifier: ~e" name)]
    [(cons (Binding bind-name bind-val) rest-env)
     (if (symbol=? name bind-name)
         bind-val
         (lookup name rest-env))]))

;;;; extend function
; Purpose: extends an environment with one new name/value binding.
(: extend (Env Symbol Value -> Env))
(define (extend env name val)
  (cons (Binding name val) env))

;;;; extend-many function
; Purpose: extends an environment with all function parameters and argument values.
(: extend-many (Env (Listof Symbol) (Listof Value) -> Env))
(define (extend-many env params vals)
  (match* (params vals)
    [('() '()) env]
    [((cons first-param rest-params) (cons first-val rest-vals))
     (extend-many (extend env first-param first-val) rest-params rest-vals)]
    [(_ _) (error 'interp "C430: function arity mismatch")]))

;;;; serialize function
; Purpose: converts any interpreted C430 value into its printed string form.
(: serialize (Value -> String))
(define (serialize val)
  (match val
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StringV str) (~v str)]
    [(CloV params body env) "#<procedure>"]
    [(PrimV op) "#<primop>"]))

;;;; numV-n* function
; Purpose: extracts a real number from a value or reports a numeric type error.
(: numV-n* (Value -> Real))
(define (numV-n* val)
  (match val
    [(NumV n) n]
    [_ (error 'interp "C430: expected number, got ~a" (serialize val))]))

;;;; stringV-str* function
; Purpose: extracts a string from a value or reports a string type error.
(: stringV-str* (Value -> String))
(define (stringV-str* val)
  (match val
    [(StringV str) str]
    [_ (error 'interp "C430: expected string, got ~a" (serialize val))]))

;;;; check-args function
; Purpose: verifies that a primitive received exactly the expected number of arguments.
(: check-args ((Listof Value) Natural Symbol -> Void))
(define (check-args args expected op)
  (unless (= (length args) expected)
    (error 'interp "C430: wrong number of arguments for ~e" op)))

;;;; value-equal? function
; Purpose: compares non-function values for the equal? primitive.
(: value-equal? (Value Value -> Boolean))
(define (value-equal? left right)
  (match* (left right)
    [((NumV a) (NumV b)) (= a b)]
    [((BoolV a) (BoolV b)) (equal? a b)]
    [((StringV a) (StringV b)) (equal? a b)]
    [(_ _) #f]))

;;;; valid-substring-index function
; Purpose: converts a real number into a safe string index.
(: valid-substring-index (Real Exact-Nonnegative-Integer Symbol -> Exact-Nonnegative-Integer))
(define (valid-substring-index n strlen op)
  (if (and (integer? n) (<= 0 n) (<= n strlen))
      (cast n Exact-Nonnegative-Integer)
      (error 'interp "C430: invalid substring index for ~e: ~e" op n)))

;;;; apply-primitive function
; Purpose: applies a built-in C430 primitive operator to already-interpreted values.
(: apply-primitive (Symbol (Listof Value) -> Value))
(define (apply-primitive op args)
  (match op
    ['+
     (check-args args 2 op)
     (NumV (+ (numV-n* (first args)) (numV-n* (second args))))]
    ['-
     (check-args args 2 op)
     (NumV (- (numV-n* (first args)) (numV-n* (second args))))]
    ['*
     (check-args args 2 op)
     (NumV (* (numV-n* (first args)) (numV-n* (second args))))]
    ['/
     (check-args args 2 op)
     (if (zero? (numV-n* (second args)))
         (error 'interp "C430: division by zero in /")
         (NumV (/ (numV-n* (first args)) (numV-n* (second args)))))]
    ['<=
     (check-args args 2 op)
     (BoolV (<= (numV-n* (first args)) (numV-n* (second args))))]
    ['equal?
     (check-args args 2 op)
     (BoolV (value-equal? (first args) (second args)))]
    ['substring
     (check-args args 3 op)
     (define str (stringV-str* (first args)))
     (define start (valid-substring-index (numV-n* (second args))
                                          (string-length str)
                                          op))
     (define stop (valid-substring-index (numV-n* (third args))
                                         (string-length str)
                                         op))
     (if (<= start stop)
         (StringV (substring str start stop))
         (error 'interp "C430: substring stop before start: ~e" args))]
    ['strlen
     (check-args args 1 op)
     (NumV (string-length (stringV-str* (first args))))]
    ['error
     (check-args args 1 op)
     (error 'interp "C430: user-error: ~a" (serialize (first args)))]
    [_ (error 'interp "C430: unknown primitive: ~e" op)]))

;;;; interp-args function
; Purpose: interprets each function argument from left to right.
(: interp-args ((Listof ExprC) Env -> (Listof Value)))
(define (interp-args args env)
  (match args
    ['() empty]
    [(cons first-arg rest-args)
     (cons (interp first-arg env)
           (interp-args rest-args env))]))

;;;; interp function
; Purpose: interprets one AST node in the supplied environment.
(: interp (ExprC Env -> Value))
(define (interp exp env)
  (match exp
    [(NumC n) (NumV n)]
    [(StringC str) (StringV str)]
    [(IdC name) (lookup name env)]
    [(IfC test then else)
     (match (interp test env)
       [(BoolV #t) (interp then env)]
       [(BoolV #f) (interp else env)]
       [_ (error 'interp "C430: if test must be boolean in ~e" exp)])]
    [(FnC params body) (CloV params body env)]
    [(CallC fun args)
     (define fun-val (interp fun env))
     (define arg-vals (interp-args args env))
     (match fun-val
       [(PrimV op) (apply-primitive op arg-vals)]
       [(CloV params body saved-env)
        (if (= (length params) (length arg-vals))
            (interp body (extend-many saved-env params arg-vals))
            (error 'interp "C430: function arity mismatch in ~e" exp))]
       [_ (error 'interp "C430: attempted to call non-function in ~e" exp)])]))

;;;; top-interp function
; Purpose: parses, interprets, and serializes one complete C430 program.
(: top-interp (Sexp -> String))
(define (top-interp s)
  (serialize (interp (parse s) top-env)))

(module+ test
  (check-equal? (parse '5) (NumC 5))
  (check-equal? (parse '"hello") (StringC "hello"))
  (check-equal? (parse 'x) (IdC 'x))
  (check-equal? (parse '(if true 1 2))
                (IfC (IdC 'true) (NumC 1) (NumC 2)))
  (check-equal? (parse '(+ 1 2))
                (CallC (IdC '+) (list (NumC 1) (NumC 2))))
  (check-equal? (parse '(var ([x = 1] [y = 2]) do (+ x y)))
                (CallC (FnC (list 'x 'y)
                            (CallC (IdC '+) (list (IdC 'x) (IdC 'y))))
                       (list (NumC 1) (NumC 2))))
  (check-exn #rx"C430" (lambda () (parse #t)))
  (check-exn #rx"C430" (lambda () (parse '(if true 1))))
  (check-exn #rx"C430" (lambda () (parse '(fn (x x) -> x))))
  (check-exn #rx"C430" (lambda () (parse '(var ([x = 1] [x = 2]) do x))))

  (check-equal? (lookup 'x (list (Binding 'x (NumV 9)))) (NumV 9))
  (check-exn #rx"C430" (lambda () (lookup 'missing top-env)))

  (check-equal? (serialize (NumV 34)) "34")
  (check-equal? (serialize (BoolV #t)) "true")
  (check-equal? (serialize (BoolV #f)) "false")
  (check-equal? (serialize (StringV "hi")) "\"hi\"")
  (check-equal? (serialize (PrimV '+)) "#<primop>")
  (check-equal? (serialize (CloV (list 'x) (IdC 'x) top-env)) "#<procedure>")

  (check-equal? (top-interp '(+ 1 2)) "3")
  (check-equal? (top-interp '(- 5 2)) "3")
  (check-equal? (top-interp '(* 3 4)) "12")
  (check-equal? (top-interp '(/ 8 2)) "4")
  (check-equal? (top-interp '(<= 1 2)) "true")
  (check-equal? (top-interp '(equal? "a" "a")) "true")
  (check-equal? (top-interp '(if true 10 20)) "10")
  (check-equal? (top-interp '(if false 10 20)) "20")
  (check-equal? (top-interp '((fn (x) -> (+ x 1)) 5)) "6")
  (check-equal? (top-interp '(((fn (x) -> (fn (y) -> (+ x y))) 10) 5)) "15")
  (check-equal? (top-interp '(var ([x = 3] [y = 4]) do (+ x y))) "7")
  (check-equal? (top-interp '(strlen "hello")) "5")
  (check-equal? (top-interp '(substring "hello" 1 4)) "\"ell\"")

  (check-exn #rx"C430" (lambda () (top-interp '(+ 1 "bad"))))
  (check-exn #rx"C430" (lambda () (top-interp '(/ 1 0))))
  (check-exn #rx"C430" (lambda () (top-interp '(if 1 2 3))))
  (check-exn #rx"C430" (lambda () (top-interp '(1 2))))
  (check-exn #rx"C430: user-error" (lambda () (top-interp '(error "bad")))))
