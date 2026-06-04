#lang typed/racket
(require typed/rackunit)

;; Assignment 8: a small C-like language implemented in Typed Racket.
;; The language uses s-expressions as its source syntax so it is easy to parse
;; in Racket, but the features are C-inspired: variables, assignment, blocks,
;; while loops, functions, arrays, structs, pointers, malloc, and free.

(define-type Location Natural)
(define-type Env (Listof Binding))
(define-type FieldExprs (Listof FieldExpr))
(define-type Fields (Listof FieldLoc))
(define-type ExprC
  (U NumC BoolC StringC IdC IfC WhileC BlockC VarC SetC FnC CallC BinopC
     AddrC DerefC PtrSetC ArrayC ArefC AsetC StructC FieldC FieldSetC
     MallocC FreeC))
(define-type Value (U NumV BoolV StringV CloV PtrV ArrayV StructV NullV))

(struct NumC ([n : Real]) #:transparent)
(struct BoolC ([b : Boolean]) #:transparent)
(struct StringC ([s : String]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct WhileC ([test : ExprC] [body : ExprC]) #:transparent)
(struct BlockC ([exprs : (Listof ExprC)]) #:transparent)
(struct VarC ([name : Symbol] [init : ExprC] [body : ExprC]) #:transparent)
(struct SetC ([name : Symbol] [rhs : ExprC]) #:transparent)
(struct FnC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct CallC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
(struct AddrC ([name : Symbol]) #:transparent)
(struct DerefC ([ptr : ExprC]) #:transparent)
(struct PtrSetC ([ptr : ExprC] [rhs : ExprC]) #:transparent)
(struct ArrayC ([items : (Listof ExprC)]) #:transparent)
(struct ArefC ([arr : ExprC] [index : ExprC]) #:transparent)
(struct AsetC ([arr : ExprC] [index : ExprC] [rhs : ExprC]) #:transparent)
(struct StructC ([fields : FieldExprs]) #:transparent)
(struct FieldC ([target : ExprC] [name : Symbol]) #:transparent)
(struct FieldSetC ([target : ExprC] [name : Symbol] [rhs : ExprC]) #:transparent)
(struct MallocC ([init : ExprC]) #:transparent)
(struct FreeC ([ptr : ExprC]) #:transparent)

(struct FieldExpr ([name : Symbol] [expr : ExprC]) #:transparent)
(struct FieldLoc ([name : Symbol] [loc : Location]) #:transparent)
(struct Binding ([name : Symbol] [loc : Location]) #:transparent)
(struct Store ([next : Location] [cells : (HashTable Location Value)]) #:transparent)
(struct Result ([value : Value] [store : Store]) #:transparent)
(struct AllocResult ([loc : Location] [store : Store]) #:transparent)

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StringV ([s : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PtrV ([loc : Location]) #:transparent)
(struct ArrayV ([base : Location] [size : Natural]) #:transparent)
(struct StructV ([fields : Fields]) #:transparent)
(struct NullV () #:transparent)

(: empty-env Env)
(define empty-env empty)

(: empty-store Store)
(define empty-store (Store 0 (hash)))

;;;; reserved? function
; Purpose: rejects keywords when parsing names that should be normal variables.
(: reserved? (Symbol -> Boolean))
(define (reserved? sym)
  (or (symbol=? sym 'if)
      (symbol=? sym 'while)
      (symbol=? sym 'block)
      (symbol=? sym 'var)
      (symbol=? sym 'set!)
      (symbol=? sym 'fn)
      (symbol=? sym '&)
      (symbol=? sym 'deref)
      (symbol=? sym 'ptr-set!)
      (symbol=? sym 'array)
      (symbol=? sym 'aref)
      (symbol=? sym 'aset!)
      (symbol=? sym 'struct)
      (symbol=? sym 'field)
      (symbol=? sym 'field-set!)
      (symbol=? sym 'malloc)
      (symbol=? sym 'free)
      (symbol=? sym 'null)))

;;;; parse-field function
; Purpose: parses one struct field, written like [field-name value].
(: parse-field (Sexp -> FieldExpr))
(define (parse-field s)
  (match s
    [(list (? symbol? name) val)
     (if (reserved? name)
         (error 'parse "CLANG: invalid struct field name: ~e" name)
         (FieldExpr name (parse val)))]
    [_ (error 'parse "CLANG: invalid struct field: ~e" s)]))

;;;; parse function
; Purpose: turns C-like s-expression source code into the interpreter AST.
(: parse (Sexp -> ExprC))
(define (parse s)
  (match s
    [(? real? n) (NumC n)]
    [(? string? str) (StringC str)]
    ['true (BoolC #t)]
    ['false (BoolC #f)]
    ['null (BlockC empty)]
    [(? symbol? sym)
     (if (reserved? sym)
         (error 'parse "CLANG: invalid identifier: ~e" sym)
         (IdC sym))]
    [(list 'if test then else) (IfC (parse test) (parse then) (parse else))]
    [(list 'while test body) (WhileC (parse test) (parse body))]
    [(list 'block exprs ...) (BlockC (map parse exprs))]
    [(list 'var (? symbol? name) init body)
     (if (reserved? name)
         (error 'parse "CLANG: invalid variable name: ~e" name)
         (VarC name (parse init) (parse body)))]
    [(list 'set! (? symbol? name) rhs) (SetC name (parse rhs))]
    [(list 'fn (list (? symbol? params) ...) body)
     (FnC (cast params (Listof Symbol)) (parse body))]
    [(list (? symbol? op) left right)
     #:when (member op '(+ - * / < <= > >= == != && ||))
     (BinopC op (parse left) (parse right))]
    [(list '& (? symbol? name)) (AddrC name)]
    [(list 'deref ptr) (DerefC (parse ptr))]
    [(list 'ptr-set! ptr rhs) (PtrSetC (parse ptr) (parse rhs))]
    [(list 'array items ...) (ArrayC (map parse items))]
    [(list 'aref arr index) (ArefC (parse arr) (parse index))]
    [(list 'aset! arr index rhs) (AsetC (parse arr) (parse index) (parse rhs))]
    [(list 'struct (list fields ...)) (StructC (map parse-field fields))]
    [(list 'field target (? symbol? name)) (FieldC (parse target) name)]
    [(list 'field-set! target (? symbol? name) rhs) (FieldSetC (parse target) name (parse rhs))]
    [(list 'malloc init) (MallocC (parse init))]
    [(list 'free ptr) (FreeC (parse ptr))]
    [(list fun args ...) (CallC (parse fun) (map parse args))]
    [_ (error 'parse "CLANG: invalid expression: ~e" s)]))

;;;; lookup function
; Purpose: finds the memory location for a variable in the environment.
(: lookup (Symbol Env -> Location))
(define (lookup name env)
  (match env
    ['() (error 'lookup "CLANG: unbound variable: ~e" name)]
    [(cons (Binding bind-name bind-loc) rest-env)
     (if (symbol=? name bind-name)
         bind-loc
         (lookup name rest-env))]))

;;;; extend function
; Purpose: adds one variable binding to an environment.
(: extend (Env Symbol Location -> Env))
(define (extend env name loc)
  (cons (Binding name loc) env))

;;;; allocate function
; Purpose: reserves one fresh memory cell in the store.
(: allocate (Store Value -> AllocResult))
(define (allocate sto val)
  (define loc (Store-next sto))
  (AllocResult loc (Store (add1 loc) (hash-set (Store-cells sto) loc val))))

;;;; allocate-many function
; Purpose: reserves several adjacent cells, used for C-like arrays.
(: allocate-many (Store (Listof Value) -> (Pairof ArrayV Store)))
(define (allocate-many sto vals)
  (define base (Store-next sto))
  (let loop ([items : (Listof Value) vals]
             [next-loc : Location base]
             [cells : (HashTable Location Value) (Store-cells sto)])
    (match items
      ['() (cons (ArrayV base (length vals)) (Store next-loc cells))]
      [(cons first-val rest-vals)
       (loop rest-vals (add1 next-loc) (hash-set cells next-loc first-val))])))

;;;; store-ref* function
; Purpose: reads a value from memory.
(: store-ref* (Store Location -> Value))
(define (store-ref* sto loc)
  (hash-ref (Store-cells sto) loc
            (lambda () (error 'interp "CLANG: invalid memory location: ~e" loc))))

;;;; store-set* function
; Purpose: writes a value into an existing memory cell.
(: store-set* (Store Location Value -> Store))
(define (store-set* sto loc val)
  (if (hash-has-key? (Store-cells sto) loc)
      (Store (Store-next sto) (hash-set (Store-cells sto) loc val))
      (error 'interp "CLANG: invalid memory location: ~e" loc)))

;;;; store-free function
; Purpose: removes a memory cell to model C's free operation.
(: store-free (Store Location -> Store))
(define (store-free sto loc)
  (if (hash-has-key? (Store-cells sto) loc)
      (Store (Store-next sto) (hash-remove (Store-cells sto) loc))
      (error 'interp "CLANG: cannot free invalid pointer: ~e" loc)))

;;;; truthy? function
; Purpose: converts values into C-like boolean behavior.
(: truthy? (Value -> Boolean))
(define (truthy? val)
  (match val
    [(BoolV b) b]
    [(NumV n) (not (zero? n))]
    [(NullV) #f]
    [_ #t]))

;;;; serialize function
; Purpose: turns an interpreted value into a printable string.
(: serialize (Value -> String))
(define (serialize val)
  (match val
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StringV s) (~v s)]
    [(CloV params body env) "#<function>"]
    [(PtrV loc) (format "#<ptr:~a>" loc)]
    [(ArrayV base size) "#<array>"]
    [(StructV fields) "#<struct>"]
    [(NullV) "null"]))

;;;; expect-num function
; Purpose: checks that a value is numeric before arithmetic.
(: expect-num (Value -> Real))
(define (expect-num val)
  (match val
    [(NumV n) n]
    [_ (error 'interp "CLANG: expected number, got ~a" (serialize val))]))

;;;; expect-ptr function
; Purpose: checks that a value is a pointer before pointer operations.
(: expect-ptr (Value -> Location))
(define (expect-ptr val)
  (match val
    [(PtrV loc) loc]
    [_ (error 'interp "CLANG: expected pointer, got ~a" (serialize val))]))

;;;; expect-array function
; Purpose: checks that a value is an array before array operations.
(: expect-array (Value -> ArrayV))
(define (expect-array val)
  (match val
    [(ArrayV base size) val]
    [_ (error 'interp "CLANG: expected array, got ~a" (serialize val))]))

;;;; expect-struct function
; Purpose: checks that a value is a struct before field operations.
(: expect-struct (Value -> StructV))
(define (expect-struct val)
  (match val
    [(StructV fields) val]
    [_ (error 'interp "CLANG: expected struct, got ~a" (serialize val))]))

;;;; valid-index function
; Purpose: validates and converts array indexes.
(: valid-index (Value Natural -> Natural))
(define (valid-index val size)
  (define n (expect-num val))
  (if (and (integer? n) (<= 0 n) (< n size))
      (cast n Natural)
      (error 'interp "CLANG: invalid array index: ~e" n)))

;;;; field-location function
; Purpose: finds the memory location for a field inside a struct.
(: field-location (Fields Symbol -> Location))
(define (field-location fields name)
  (match fields
    ['() (error 'interp "CLANG: unknown struct field: ~e" name)]
    [(cons (FieldLoc field-name loc) rest-fields)
     (if (symbol=? name field-name)
         loc
         (field-location rest-fields name))]))

;;;; value-equal? function
; Purpose: compares simple C-like values for == and !=.
(: value-equal? (Value Value -> Boolean))
(define (value-equal? left right)
  (match* (left right)
    [((NumV a) (NumV b)) (= a b)]
    [((BoolV a) (BoolV b)) (equal? a b)]
    [((StringV a) (StringV b)) (string=? a b)]
    [((PtrV a) (PtrV b)) (= a b)]
    [((ArrayV a-base a-size) (ArrayV b-base b-size))
     (and (= a-base b-base) (= a-size b-size))]
    [((NullV) (NullV)) #t]
    [(_ _) #f]))

;;;; interp-binop function
; Purpose: evaluates C-like binary operators.
(: interp-binop (Symbol Value Value -> Value))
(define (interp-binop op left right)
  (match op
    ['+ (NumV (+ (expect-num left) (expect-num right)))]
    ['- (NumV (- (expect-num left) (expect-num right)))]
    ['* (NumV (* (expect-num left) (expect-num right)))]
    ['/ (if (zero? (expect-num right))
            (error 'interp "CLANG: division by zero")
            (NumV (/ (expect-num left) (expect-num right))))]
    ['< (BoolV (< (expect-num left) (expect-num right)))]
    ['<= (BoolV (<= (expect-num left) (expect-num right)))]
    ['> (BoolV (> (expect-num left) (expect-num right)))]
    ['>= (BoolV (>= (expect-num left) (expect-num right)))]
    ['== (BoolV (value-equal? left right))]
    ['!= (BoolV (not (value-equal? left right)))]
    ['&& (BoolV (and (truthy? left) (truthy? right)))]
    ['|| (BoolV (or (truthy? left) (truthy? right)))]
    [_ (error 'interp "CLANG: unknown binary operator: ~e" op)]))

;;;; interp-list function
; Purpose: evaluates a list of expressions and returns their values.
(: interp-list ((Listof ExprC) Env Store -> (Pairof (Listof Value) Store)))
(define (interp-list exprs env sto)
  (match exprs
    ['() (cons empty sto)]
    [(cons first-expr rest-exprs)
     (define first-result (interp first-expr env sto))
     (define rest-result (interp-list rest-exprs env (Result-store first-result)))
     (cons (cons (Result-value first-result) (car rest-result))
           (cdr rest-result))]))

;;;; interp-block function
; Purpose: evaluates expressions in order and returns the final value.
(: interp-block ((Listof ExprC) Env Store -> Result))
(define (interp-block exprs env sto)
  (match exprs
    ['() (Result (NullV) sto)]
    [(list last-expr) (interp last-expr env sto)]
    [(cons first-expr rest-exprs)
     (define first-result (interp first-expr env sto))
     (interp-block rest-exprs env (Result-store first-result))]))

;;;; bind-params function
; Purpose: binds function parameters to fresh memory locations.
(: bind-params ((Listof Symbol) (Listof Value) Env Store -> (Pairof Env Store)))
(define (bind-params params vals env sto)
  (match* (params vals)
    [('() '()) (cons env sto)]
    [((cons first-param rest-params) (cons first-val rest-vals))
     (define alloc-result (allocate sto first-val))
     (bind-params rest-params
                  rest-vals
                  (extend env first-param (AllocResult-loc alloc-result))
                  (AllocResult-store alloc-result))]
    [(_ _) (error 'interp "CLANG: function arity mismatch")]))

;;;; build-fields function
; Purpose: evaluates struct fields and stores each field in memory.
(: build-fields (FieldExprs Env Store Fields -> (Pairof Fields Store)))
(define (build-fields fields env sto built)
  (match fields
    ['() (cons (reverse built) sto)]
    [(cons (FieldExpr name expr) rest-fields)
     (define expr-result (interp expr env sto))
     (define alloc-result (allocate (Result-store expr-result) (Result-value expr-result)))
     (build-fields rest-fields
                   env
                   (AllocResult-store alloc-result)
                   (cons (FieldLoc name (AllocResult-loc alloc-result)) built))]))

;;;; interp function
; Purpose: evaluates one C-like AST node using an environment and store.
(: interp (ExprC Env Store -> Result))
(define (interp expr env sto)
  (match expr
    [(NumC n) (Result (NumV n) sto)]
    [(BoolC b) (Result (BoolV b) sto)]
    [(StringC s) (Result (StringV s) sto)]
    [(IdC name) (Result (store-ref* sto (lookup name env)) sto)]
    [(IfC test then else)
     (define test-result (interp test env sto))
     (if (truthy? (Result-value test-result))
         (interp then env (Result-store test-result))
         (interp else env (Result-store test-result)))]
    [(WhileC test body)
     (let loop ([current-store : Store sto])
       (define test-result (interp test env current-store))
       (if (truthy? (Result-value test-result))
           (let ([body-result (interp body env (Result-store test-result))])
             (loop (Result-store body-result)))
           (Result (NullV) (Result-store test-result))))]
    [(BlockC exprs) (interp-block exprs env sto)]
    [(VarC name init body)
     (define init-result (interp init env sto))
     (define alloc-result (allocate (Result-store init-result) (Result-value init-result)))
     (interp body
             (extend env name (AllocResult-loc alloc-result))
             (AllocResult-store alloc-result))]
    [(SetC name rhs)
     (define rhs-result (interp rhs env sto))
     (define loc (lookup name env))
     (Result (Result-value rhs-result)
             (store-set* (Result-store rhs-result) loc (Result-value rhs-result)))]
    [(FnC params body) (Result (CloV params body env) sto)]
    [(CallC fun args)
     (define fun-result (interp fun env sto))
     (define arg-result (interp-list args env (Result-store fun-result)))
     (match (Result-value fun-result)
       [(CloV params body saved-env)
        (define bindings (bind-params params (car arg-result) saved-env (cdr arg-result)))
        (interp body (car bindings) (cdr bindings))]
       [_ (error 'interp "CLANG: attempted to call a non-function")])]
    [(BinopC op left right)
     (define left-result (interp left env sto))
     (define right-result (interp right env (Result-store left-result)))
     (Result (interp-binop op (Result-value left-result) (Result-value right-result))
             (Result-store right-result))]
    [(AddrC name) (Result (PtrV (lookup name env)) sto)]
    [(DerefC ptr)
     (define ptr-result (interp ptr env sto))
     (Result (store-ref* (Result-store ptr-result)
                         (expect-ptr (Result-value ptr-result)))
             (Result-store ptr-result))]
    [(PtrSetC ptr rhs)
     (define ptr-result (interp ptr env sto))
     (define rhs-result (interp rhs env (Result-store ptr-result)))
     (define loc (expect-ptr (Result-value ptr-result)))
     (Result (Result-value rhs-result)
             (store-set* (Result-store rhs-result) loc (Result-value rhs-result)))]
    [(ArrayC items)
     (define item-result (interp-list items env sto))
     (define arr-result (allocate-many (cdr item-result) (car item-result)))
     (Result (car arr-result) (cdr arr-result))]
    [(ArefC arr index)
     (define arr-result (interp arr env sto))
     (define index-result (interp index env (Result-store arr-result)))
     (match (expect-array (Result-value arr-result))
       [(ArrayV base size)
        (define index-loc (valid-index (Result-value index-result) size))
        (Result (store-ref* (Result-store index-result) (+ base index-loc))
                (Result-store index-result))])]
    [(AsetC arr index rhs)
     (define arr-result (interp arr env sto))
     (define index-result (interp index env (Result-store arr-result)))
     (define rhs-result (interp rhs env (Result-store index-result)))
     (match (expect-array (Result-value arr-result))
       [(ArrayV base size)
        (define index-loc (valid-index (Result-value index-result) size))
        (Result (Result-value rhs-result)
                (store-set* (Result-store rhs-result) (+ base index-loc) (Result-value rhs-result)))])]
    [(StructC fields)
     (define built (build-fields fields env sto empty))
     (Result (StructV (car built)) (cdr built))]
    [(FieldC target name)
     (define target-result (interp target env sto))
     (match (expect-struct (Result-value target-result))
       [(StructV fields)
        (Result (store-ref* (Result-store target-result) (field-location fields name))
                (Result-store target-result))])]
    [(FieldSetC target name rhs)
     (define target-result (interp target env sto))
     (define rhs-result (interp rhs env (Result-store target-result)))
     (match (expect-struct (Result-value target-result))
       [(StructV fields)
        (define loc (field-location fields name))
        (Result (Result-value rhs-result)
                (store-set* (Result-store rhs-result) loc (Result-value rhs-result)))])]
    [(MallocC init)
     (define init-result (interp init env sto))
     (define alloc-result (allocate (Result-store init-result) (Result-value init-result)))
     (Result (PtrV (AllocResult-loc alloc-result)) (AllocResult-store alloc-result))]
    [(FreeC ptr)
     (define ptr-result (interp ptr env sto))
     (Result (NullV)
             (store-free (Result-store ptr-result)
                         (expect-ptr (Result-value ptr-result))))]))

;;;; top-interp function
; Purpose: parses, interprets, and serializes one C-like program.
(: top-interp (Sexp -> String))
(define (top-interp s)
  (serialize (Result-value (interp (parse s) empty-env empty-store))))

(module+ test
  ;;;; TEST CASES
  (check-equal? (top-interp '(+ 20 22)) "42")
  (check-equal? (top-interp '(var x 5 (+ x 3))) "8")
  (check-equal? (top-interp '(var x 0 (block (set! x 9) x))) "9")
  (check-equal? (top-interp '(if (== 1 1) 10 20)) "10")
  (check-equal? (top-interp '(var i 0 (block (while (< i 3) (set! i (+ i 1))) i))) "3")
  (check-equal? (top-interp '((fn (x) (* x x)) 6)) "36")
  (check-equal? (top-interp '(var x 7 (var p (& x) (block (ptr-set! p 99) (deref p))))) "99")
  (check-equal? (top-interp '(var p (malloc 12) (block (ptr-set! p 13) (deref p)))) "13")
  (check-equal? (top-interp '(var a (array 1 2 3) (block (aset! a 1 50) (aref a 1)))) "50")
  (check-equal? (top-interp '(var point (struct ([x 2] [y 3]))
                                (block (field-set! point x 8) (field point x))))
                "8")
  (check-equal? (top-interp '(var p (malloc 4) (block (free p) null))) "null")
  (check-exn #rx"CLANG" (lambda () (top-interp '(+ 1 "bad"))))
  (check-exn #rx"CLANG" (lambda () (top-interp '(aref (array 1 2) 5)))))
