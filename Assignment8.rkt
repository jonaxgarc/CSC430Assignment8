#lang typed/racket
(require typed/rackunit)

(provide (all-defined-out))

(define-type Location Natural)
(define-type Env (HashTable Symbol Location))
(define-type FieldExprs (Listof (Pairof Symbol ExprC)))
(define-type Fields (HashTable Symbol Location))
(define-type ExprC
  (U NumC idC StringC BoolC ifC fnC CallC BinopC SeqC DeclC AssignC WhileC
     AddrC DerefC PtrSetC StructC FieldC FieldSetC MallocC FreeC))
(define-type Value (U NumV StringV BoolV ClosureV PtrV StructV VoidV))

(struct NumC ([n : Real]) #:transparent)
(struct idC ([name : Symbol]) #:transparent)
(struct StringC ([str : String]) #:transparent)
(struct BoolC ([b : Boolean]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct fnC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct CallC ([fun : ExprC] [exs : (Listof ExprC)]) #:transparent)
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
(struct SeqC ([exs : (Listof ExprC)]) #:transparent)
(struct DeclC ([name : Symbol] [init : ExprC] [body : ExprC]) #:transparent)
(struct AssignC ([name : Symbol] [rhs : ExprC]) #:transparent)
(struct WhileC ([test : ExprC] [body : ExprC]) #:transparent)
(struct AddrC ([name : Symbol]) #:transparent)
(struct DerefC ([ptr : ExprC]) #:transparent)
(struct PtrSetC ([ptr : ExprC] [rhs : ExprC]) #:transparent)
(struct StructC ([fields : FieldExprs]) #:transparent)
(struct FieldC ([target : ExprC] [name : Symbol]) #:transparent)
(struct FieldSetC ([target : ExprC] [name : Symbol] [rhs : ExprC]) #:transparent)
(struct MallocC ([init : ExprC]) #:transparent)
(struct FreeC ([ptr : ExprC]) #:transparent)

(struct NumV ([n : Real]) #:transparent)
(struct StringV ([str : String]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct ClosureV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PtrV ([loc : Location]) #:transparent)
(struct StructV ([fields : Fields]) #:transparent)
(struct VoidV () #:transparent)

(struct Store ([next : Location] [cells : (HashTable Location Value)]) #:transparent)
(struct Result ([value : Value] [store : Store]) #:transparent)
(struct AllocResult ([loc : Location] [store : Store]) #:transparent)

(define empty-env : Env (hash))
(define empty-store : Store (Store 0 (hash)))

;; Looks up the memory location for a variable name.
(: env-ref (Env Symbol -> Location))
(define (env-ref env name)
  (hash-ref env name (lambda () (error 'env-ref "unbound variable: ~a" name))))

;; Extends an environment with one variable-to-location binding.
(: env-set (Env Symbol Location -> Env))
(define (env-set env name loc)
  (hash-set env name loc))

;; Allocates a fresh store location for a value.
(: store-alloc (Store Value -> AllocResult))
(define (store-alloc store value)
  (define loc (Store-next store))
  (AllocResult loc (Store (add1 loc) (hash-set (Store-cells store) loc value))))

;; Reads a value from the store.
(: store-ref (Store Location -> Value))
(define (store-ref store loc)
  (hash-ref (Store-cells store) loc (lambda () (error 'store-ref "invalid location: ~a" loc))))

;; Replaces the value at an existing store location.
(: store-set (Store Location Value -> Store))
(define (store-set store loc value)
  (if (hash-has-key? (Store-cells store) loc)
      (Store (Store-next store) (hash-set (Store-cells store) loc value))
      (error 'store-set "invalid location: ~a" loc)))

;; Removes a store location to model freeing allocated memory.
(: store-free (Store Location -> Store))
(define (store-free store loc)
  (if (hash-has-key? (Store-cells store) loc)
      (Store (Store-next store) (hash-remove (Store-cells store) loc))
      (error 'store-free "invalid location: ~a" loc)))

;; Converts language values into C-like truth values.
(: truthy? (Value -> Boolean))
(define (truthy? value)
  (match value
    [(BoolV b) b]
    [(NumV n) (not (zero? n))]
    [(VoidV) #f]
    [_ #t]))

;; Extracts a number from a numeric value.
(: expect-num (Value Symbol -> Real))
(define (expect-num value who)
  (match value
    [(NumV n) n]
    [_ (error who "expected a number, got: ~v" value)]))

;; Extracts a pointer location from a pointer value.
(: expect-ptr (Value Symbol -> Location))
(define (expect-ptr value who)
  (match value
    [(PtrV loc) loc]
    [_ (error who "expected a pointer, got: ~v" value)]))

;; Extracts field locations from a struct value.
(: expect-struct (Value Symbol -> Fields))
(define (expect-struct value who)
  (match value
    [(StructV fields) fields]
    [_ (error who "expected a struct, got: ~v" value)]))

;; Looks up the location for a named struct field.
(: field-ref (Fields Symbol -> Location))
(define (field-ref fields name)
  (hash-ref fields name (lambda () (error 'field-ref "unknown field: ~a" name))))

;; Compares simple first-order values for the equality operators.
(: value-equal? (Value Value -> Boolean))
(define (value-equal? left right)
  (match* (left right)
    [((NumV a) (NumV b)) (= a b)]
    [((StringV a) (StringV b)) (string=? a b)]
    [((BoolV a) (BoolV b)) (equal? a b)]
    [((PtrV a) (PtrV b)) (= a b)]
    [((VoidV) (VoidV)) #t]
    [(_ _) #f]))

;; Applies a binary operator to already-interpreted operand values.
(: interp-binop (Symbol Value Value -> Value))
(define (interp-binop op left right)
  (case op
    [(+) (NumV (+ (expect-num left 'binop) (expect-num right 'binop)))]
    [(-) (NumV (- (expect-num left 'binop) (expect-num right 'binop)))]
    [(*) (NumV (* (expect-num left 'binop) (expect-num right 'binop)))]
    [(/) (let ([denom (expect-num right 'binop)])
           (if (zero? denom)
               (error 'binop "division by zero")
               (NumV (/ (expect-num left 'binop) denom))))]
    [(<) (BoolV (< (expect-num left 'binop) (expect-num right 'binop)))]
    [(<=) (BoolV (<= (expect-num left 'binop) (expect-num right 'binop)))]
    [(>) (BoolV (> (expect-num left 'binop) (expect-num right 'binop)))]
    [(>=) (BoolV (>= (expect-num left 'binop) (expect-num right 'binop)))]
    [(==) (BoolV (value-equal? left right))]
    [(!=) (BoolV (not (value-equal? left right)))]
    [(&&) (BoolV (and (truthy? left) (truthy? right)))]
    [(||) (BoolV (or (truthy? left) (truthy? right)))]
    [else (error 'binop "unknown operator: ~a" op)]))

;; Interprets a list of expressions from left to right and returns the last value.
(: interp-seq ((Listof ExprC) Env Store -> Result))
(define (interp-seq exs env store)
  (match exs
    ['() (Result (VoidV) store)]
    [(list last-expr) (interp last-expr env store)]
    [(cons first-expr rest-exprs)
     (define first-result (interp first-expr env store))
     (interp-seq rest-exprs env (Result-store first-result))]))

;; Evaluates actual arguments, allocating one store location per function parameter.
(: bind-params ((Listof Symbol) (Listof ExprC) Env Env Store -> (Pairof Env Store)))
(define (bind-params params args closure-env caller-env store)
  (match* (params args)
    [('() '()) (cons closure-env store)]
    [((cons param rest-params) (cons arg rest-args))
     (define arg-result (interp arg caller-env store))
     (define alloc-result (store-alloc (Result-store arg-result) (Result-value arg-result)))
     (bind-params rest-params
                  rest-args
                  (env-set closure-env param (AllocResult-loc alloc-result))
                  caller-env
                  (AllocResult-store alloc-result))]
    [(_ _) (error 'call "wrong number of arguments")]))

;; Evaluates struct field initializers and allocates storage for each field.
(: build-struct-fields (FieldExprs Env Store Fields -> (Pairof Fields Store)))
(define (build-struct-fields fields env store built)
  (match fields
    ['() (cons built store)]
    [(cons field rest-fields)
     (define name (car field))
     (define expr (cdr field))
     (define expr-result (interp expr env store))
     (define alloc-result (store-alloc (Result-store expr-result) (Result-value expr-result)))
     (build-struct-fields rest-fields
                          env
                          (AllocResult-store alloc-result)
                          (hash-set built name (AllocResult-loc alloc-result)))]))

;; Interprets one expression with an environment and explicit mutable store.
(: interp (ExprC Env Store -> Result))
(define (interp expr env store)
  (match expr
    [(NumC n) (Result (NumV n) store)]
    [(idC name) (Result (store-ref store (env-ref env name)) store)]
    [(StringC str) (Result (StringV str) store)]
    [(BoolC b) (Result (BoolV b) store)]
    [(ifC test then-branch else-branch)
     (define test-result (interp test env store))
     (if (truthy? (Result-value test-result))
         (interp then-branch env (Result-store test-result))
         (interp else-branch env (Result-store test-result)))]
    [(fnC params body) (Result (ClosureV params body env) store)]
    [(CallC fun args)
     (define fun-result (interp fun env store))
     (match (Result-value fun-result)
       [(ClosureV params body closure-env)
        (define bindings (bind-params params args closure-env env (Result-store fun-result)))
        (interp body (car bindings) (cdr bindings))]
       [_ (error 'call "expected a function")])]
    [(BinopC op left right)
     (define left-result (interp left env store))
     (define right-result (interp right env (Result-store left-result)))
     (Result (interp-binop op (Result-value left-result) (Result-value right-result))
             (Result-store right-result))]
    [(SeqC exs) (interp-seq exs env store)]
    [(DeclC name init body)
     (define init-result (interp init env store))
     (define alloc-result (store-alloc (Result-store init-result) (Result-value init-result)))
     (interp body
             (env-set env name (AllocResult-loc alloc-result))
             (AllocResult-store alloc-result))]
    [(AssignC name rhs)
     (define rhs-result (interp rhs env store))
     (define loc (env-ref env name))
     (Result (Result-value rhs-result)
             (store-set (Result-store rhs-result) loc (Result-value rhs-result)))]
    [(WhileC test body)
     (let loop ([current-store : Store store])
       (define test-result (interp test env current-store))
       (if (truthy? (Result-value test-result))
           (let ([body-result (interp body env (Result-store test-result))])
             (loop (Result-store body-result)))
           (Result (VoidV) (Result-store test-result))))]
    [(AddrC name) (Result (PtrV (env-ref env name)) store)]
    [(DerefC ptr)
     (define ptr-result (interp ptr env store))
     (Result (store-ref (Result-store ptr-result) (expect-ptr (Result-value ptr-result) 'deref))
             (Result-store ptr-result))]
    [(PtrSetC ptr rhs)
     (define ptr-result (interp ptr env store))
     (define rhs-result (interp rhs env (Result-store ptr-result)))
     (define loc (expect-ptr (Result-value ptr-result) 'ptr-set!))
     (Result (Result-value rhs-result)
             (store-set (Result-store rhs-result) loc (Result-value rhs-result)))]
    [(StructC fields)
     (define built (build-struct-fields fields env store (hash)))
     (Result (StructV (car built)) (cdr built))]
    [(FieldC target name)
     (define target-result (interp target env store))
     (define fields (expect-struct (Result-value target-result) 'field))
     (Result (store-ref (Result-store target-result) (field-ref fields name))
             (Result-store target-result))]
    [(FieldSetC target name rhs)
     (define target-result (interp target env store))
     (define fields (expect-struct (Result-value target-result) 'field-set!))
     (define rhs-result (interp rhs env (Result-store target-result)))
     (define loc (field-ref fields name))
     (Result (Result-value rhs-result)
             (store-set (Result-store rhs-result) loc (Result-value rhs-result)))]
    [(MallocC init)
     (define init-result (interp init env store))
     (define alloc-result (store-alloc (Result-store init-result) (Result-value init-result)))
     (Result (PtrV (AllocResult-loc alloc-result)) (AllocResult-store alloc-result))]
    [(FreeC ptr)
     (define ptr-result (interp ptr env store))
     (Result (VoidV)
             (store-free (Result-store ptr-result)
                         (expect-ptr (Result-value ptr-result) 'free)))]))

;; Runs a program from an empty environment and empty store.
(: run (ExprC -> Value))
(define (run expr)
  (Result-value (interp expr empty-env empty-store)))

(module+ test
  (check-equal? (run (BinopC '+ (NumC 10) (NumC 32))) (NumV 42))
  (check-equal? (run (DeclC 'x (NumC 1)
                           (SeqC (list (AssignC 'x (BinopC '+ (idC 'x) (NumC 4)))
                                       (idC 'x)))))
                (NumV 5))
  (check-equal? (run (CallC (fnC (list 'x)
                                 (BinopC '* (idC 'x) (NumC 2)))
                           (list (NumC 9))))
                (NumV 18))
  (check-equal? (run (DeclC 'x (NumC 7)
                           (DeclC 'p (AddrC 'x)
                             (SeqC (list (PtrSetC (idC 'p) (NumC 11))
                                         (DerefC (idC 'p)))))))
                (NumV 11))
  (check-equal? (run (DeclC 'point
                           (StructC (list (cons 'x (NumC 2))
                                          (cons 'y (NumC 3))))
                           (SeqC (list (FieldSetC (idC 'point) 'x (NumC 8))
                                       (FieldC (idC 'point) 'x)))))
                (NumV 8))
  (check-equal? (run (DeclC 'i (NumC 0)
                           (SeqC (list (WhileC (BinopC '< (idC 'i) (NumC 3))
                                                (AssignC 'i (BinopC '+ (idC 'i) (NumC 1))))
                                       (idC 'i)))))
                (NumV 3)))
