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