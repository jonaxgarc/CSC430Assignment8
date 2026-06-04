#lang typed/racket
(require typed/rackunit)

;; Full project implemented.

(define-type ExprC (U NumC idC StringC ifC fnC CallC mutC))
(struct NumC ([n : Real]) #:transparent)
(struct idC ([name : Symbol]) #:transparent)
(struct StringC ([str : String]) #:transparent)
(struct ifC ([test : ExprC][then : ExprC][else : ExprC]) #:transparent)
(struct fnC ([params : (Listof Symbol)][body : ExprC]) #:transparent)
(struct CallC ([fun : ExprC][exs : (Listof ExprC)]) #:transparent)
(struct mutC ([name : Symbol][rhs : ExprC]) #:transparent)

(define-type Value (U NumV BoolV StringV CloV PrimV ArrayV NullV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StringV ([str : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)
(struct ArrayV ([ptr : Natural][size : Natural]) #:transparent)
(struct NullV () #:transparent)

(struct Binding ([name : Symbol][loc : Natural]) #:transparent)
(define-type Env (Listof Binding))

(: top-env Env)
(define top-env
  (list (Binding '+ 0)
        (Binding '- 1)
        (Binding '* 2)
        (Binding '/ 3)
        (Binding '<= 4)
        (Binding 'equal? 5)
        (Binding 'substring 6)
        (Binding 'strlen 7)
        (Binding 'error 8)
        (Binding 'true 9)
        (Binding 'false 10)
        (Binding 'println 11)
        (Binding 'read-num 12)
        (Binding 'read-str 13)
        (Binding 'chain 14)
        (Binding '++ 15)
        (Binding 'make-array 16)
        (Binding 'array 17)
        (Binding 'aref 18)
        (Binding 'aset! 19)))

(: initial-env Env)
(define initial-env top-env)

(: initial-values (Listof Value))
(define initial-values
  (list (PrimV '+)
        (PrimV '-)
        (PrimV '*)
        (PrimV '/)
        (PrimV '<=)
        (PrimV 'equal?)
        (PrimV 'substring)
        (PrimV 'strlen)
        (PrimV 'error)
        (BoolV #t)
        (BoolV #f)
        (PrimV 'println)
        (PrimV 'read-num)
        (PrimV 'read-str)
        (PrimV 'chain)
        (PrimV '++)
        (PrimV 'make-array)
        (PrimV 'array)
        (PrimV 'aref)
        (PrimV 'aset!)))

(struct Store ([mem : (Vectorof Value)]
               [next : Natural]) #:mutable)

(: MEMORY Store)
(define MEMORY
  (Store (ann (make-vector 100 (BoolV #f))
              (Vectorof Value))
         0))

;;;; Expression Parser function for VEBG
; Purpose: parses VEBG expressions functions into readable interpretable expressions
(: parse (Sexp -> ExprC))
(define (parse s)
  (match s
    [(? real? n) (NumC n)]
    [(? string? str) (StringC str)]
    [(? symbol? sym)
     (if (reserved? sym)
         (error 'parse "VEBG: invalid identifier: ~e" s)
         (idC sym))]
    [(list 'if t th el)
     (ifC (parse t) (parse th) (parse el))]
    [(list (? symbol? name) ':= rhs)
     (if (reserved? name)
         (error 'parse "VEBG: invalid mutation name: ~e" s)
         (mutC name (parse rhs)))]
    [(list 'fn (list (? symbol? params) ...) '-> body)
     (define ps (cast params (Listof Symbol)))
     (if (has-duplicates? ps)
         (error 'parse "VEBG: duplicate parameter: ~e" s)
         (fnC ps (parse body)))]
    [(list 'given (list bindings ...) 'do body)
     (define names (map parse-binding-name bindings))
     (define vals (map parse-binding-val bindings))
     (if (has-duplicates? names)
         (error 'parse "VEBG: duplicate given binding: ~e" s)
         (CallC (fnC names (parse body)) vals))]
    [(list fun args ...)
     (CallC (parse fun) (map parse args))]
    [_ (error 'parse "VEBG: invalid VEBG expression: ~e" s)]))

(: reserved? (Symbol -> Boolean))
(define (reserved? sym)
  (or (symbol=? sym 'if)
      (symbol=? sym '=)
      (symbol=? sym 'given)
      (symbol=? sym 'fn)
      (symbol=? sym '->)
      (symbol=? sym 'do)
      (symbol=? sym ':=)))

(: has-duplicates? ((Listof Symbol) -> Boolean))
(define (has-duplicates? xs)
  (cond
    [(empty? xs) #f]
    [(member (first xs) (rest xs)) #t]
    [else (has-duplicates? (rest xs))]))

(: parse-binding-name (Sexp -> Symbol))
(define (parse-binding-name b)
  (match b
    [(list (? symbol? name) '= rhs)
     (if (reserved? name)
         (error 'parse "VEBG: invalid binding name: ~e" b)
         name)]
    [_ (error 'parse "VEBG: invalid given binding: ~e" b)]))

(: parse-binding-val (Sexp -> ExprC))
(define (parse-binding-val b)
  (match b
    [(list (? symbol? name) '= rhs)
     (parse rhs)]
    [_ (error 'parse "VEBG: invalid given binding: ~e" b)]))

;;;; lookup function
; Purpose: finds the store location of an identifier in the environment
(: lookup (Symbol Env -> Natural))
(define (lookup name env)
  (match env
    ['() (error 'lookup "VEBG: unbound identifier: ~e" name)]
    [(cons (Binding bind-name bind-loc) rest-env)
     (if (symbol=? name bind-name)
         bind-loc
         (lookup name rest-env))]))

(: allocate (Store Natural -> Natural))
(define (allocate sto n)
  (define base-loc (Store-next sto))
  (define mem-cap (vector-length (Store-mem sto)))
  (if (> (+ base-loc n) mem-cap)
      (error 'interp "VEBG: out of memory while allocating ~e cells" n)
      (begin
        (set-Store-next! sto (+ base-loc n))
        base-loc)))

;;;; extend function
; Purpose: adds parameters and their values to an environment by allocating store locations
(: extend-with-alloc (Env (Listof Symbol) (Listof Value) Store -> Env))
(define (extend-with-alloc env params vals sto)
  (match* (params vals)
    [('() '()) env]
    [((cons p ps) (cons v vs))
     (define loc (allocate sto 1))
     (vector-set! (Store-mem sto) loc v)
     (extend-with-alloc
      (cons (Binding p loc) env)
      ps
      vs
      sto)]
    [(_ _) (error 'interp "VEBG: extend-with-alloc arity mismatch")]))

;;;; serialize function
; Purpose: turns a VEBG value into a string
(: serialize (Value -> String))
(define (serialize v)
  (match v
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StringV str) (~v str)]
    [(CloV params body env) "#<procedure>"]
    [(PrimV op) "#<primop>"]
    [(ArrayV ptr size) "#<array>"]
    [(NullV) "null"]))

;;;; apply-primitive
; Purpose: helper function for interpreting primitive expressions
(: numV-n* (Value -> Real))
(define (numV-n* v)
  (match v
    [(NumV n) n]
    [_ (error 'interp "VEBG: expected number, got ~a" (serialize v))]))

(: stringV-str* (Value -> String))
(define (stringV-str* v)
  (match v
    [(StringV s) s]
    [_ (error 'interp "VEBG: expected string, got ~a" (serialize v))]))

(: arrayV* (Value -> ArrayV))
(define (arrayV* v)
  (match v
    [(ArrayV ptr size) v]
    [_ (error 'interp "VEBG: expected array, got ~a" (serialize v))]))

(: check-args ((Listof Value) Natural Symbol -> Void))
(define (check-args args n op)
  (unless (= (length args) n)
    (error 'interp "VEBG: wrong number of arguments for ~e" op)))

(: value-equal? (Value Value -> Boolean))
(define (value-equal? a b)
  (match* (a b)
    [((NumV x) (NumV y)) (= x y)]
    [((BoolV x) (BoolV y)) (equal? x y)]
    [((StringV x) (StringV y)) (equal? x y)]
    [((ArrayV p1 s1) (ArrayV p2 s2)) (and (= p1 p2) (= s1 s2))]
    [((NullV) (NullV)) #t]
    [(_ _) #f]))

(: value->string (Value -> String))
(define (value->string v)
  (match v
    [(StringV s) s]
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(NullV) "null"]
    [else (error 'interp "VEBG: value->string cannot convert value to string: ~a" (serialize v))]))

(: valid-index (Real Natural Symbol -> Natural))
(define (valid-index i size op)
  (if (and (integer? i) (<= 0 i) (< i size))
      (cast i Natural)
      (error 'interp "VEBG: invalid array index for ~e: ~e" op i)))

(: fill-array! (Store Natural Natural Value -> Void))
(define (fill-array! sto base size val)
  (let loop ([i : Natural 0])
    (if (< i size)
        (begin
          (vector-set! (Store-mem sto) (+ base i) val)
          (loop (add1 i)))
        (void))))

(: copy-array-values! (Store Natural (Listof Value) -> Void))
(define (copy-array-values! sto base vals)
  (let loop ([i : Natural 0]
             [vs : (Listof Value) vals])
    (match vs
      ['() (void)]
      [(cons v rest)
       (begin
         (vector-set! (Store-mem sto) (+ base i) v)
         (loop (add1 i) rest))])))

(: last-value ((Listof Value) -> Value))
(define (last-value vals)
  (match vals
    ['() (error 'interp "VEBG: chain needs at least one expression")]
    [(list v) v]
    [(cons first-val rest-vals) (last-value rest-vals)]))

;;;; make-initial-store function with copy-values-to-store helper
; Purpose: creates the initial store with top-level bindings loaded into memory
(: copy-values-to-store ((Listof Value) (Vectorof Value) Natural -> Void))
(define (copy-values-to-store vals mem i)
  (match vals
    ['() (void)]
    [(cons first-val rest-vals)
     (begin
       (vector-set! mem i first-val)
       (copy-values-to-store rest-vals mem (add1 i)))]))

(: make-initial-store (Natural -> Store))
(define (make-initial-store memsize)
  (if (< memsize (length initial-values))
      (error 'interp "VEBG: memory size too small for initial environment: ~e" memsize)
      (let ([mem (ann (make-vector memsize (BoolV #f))
                      (Vectorof Value))])
        (copy-values-to-store initial-values mem 0)
        (Store mem (length initial-values)))))

(: apply-primitive (Symbol (Listof Value) Store -> Value))
(define (apply-primitive op args sto)
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
     (if (= (numV-n* (second args)) 0)
         (error 'interp "VEBG: division by zero in /: ~e" args)
         (NumV (/ (numV-n* (first args)) (numV-n* (second args)))))]
    ['<=
     (check-args args 2 op)
     (BoolV (<= (numV-n* (first args)) (numV-n* (second args))))]
    ['equal?
     (check-args args 2 op)
     (BoolV (value-equal? (first args) (second args)))]
    ['strlen
     (check-args args 1 op)
     (NumV (string-length (stringV-str* (first args))))]
    ['substring
     (check-args args 3 op)
     (define str (stringV-str* (first args)))
     (define start (numV-n* (second args)))
     (define stop (numV-n* (third args)))
     (if (and (integer? start) (integer? stop)
              (<= 0 start) (<= start stop) (<= stop (string-length str)))
         (StringV (substring str
                             (cast start Exact-Nonnegative-Integer)
                             (cast stop Exact-Nonnegative-Integer)))
         (error 'interp "VEBG: invalid substring indexes: ~e" args))]
    ['println
     (check-args args 1 op)
     (match (first args)
       [(StringV s)
        (displayln s)
        (BoolV #t)]
       [else (error 'interp "VEBG: println expected a string: ~a" (serialize (first args)))])]
    ['read-num
     (check-args args 0 op)
     (define input (read-line))
     (if (string? input)
         (let ([num (string->number input)])
           (if (and num (real? num))
               (NumV num)
               (error 'interp "VEBG: read-num expected a real number, got ~e" input)))
         (error 'interp "VEBG: read-num expected input"))]
    ['read-str
     (check-args args 0 op)
     (define input (read-line))
     (if (string? input)
         (StringV input)
         (error 'interp "VEBG: read-str expected input"))]
    ['chain
     (last-value args)]
    ['++
     (if (empty? args)
         (error 'interp "VEBG: ++ expected at least one argument")
         (StringV
          (apply string-append
                 (map value->string args))))]
    ['make-array
     (check-args args 2 op)
     (define size-val (numV-n* (first args)))
     (if (and (integer? size-val) (>= size-val 1))
         (let* ([size (cast size-val Natural)]
                [base (allocate sto size)])
           (fill-array! sto base size (second args))
           (ArrayV base size))
         (error 'interp "VEBG: make-array expected positive integer size: ~e" args))]
    ['array
     (if (empty? args)
         (error 'interp "VEBG: array expected at least one value")
         (let* ([size (length args)]
                [base (allocate sto size)])
           (copy-array-values! sto base args)
           (ArrayV base size)))]
    ['aref
     (check-args args 2 op)
     (match (arrayV* (first args))
       [(ArrayV ptr size)
        (define index (valid-index (numV-n* (second args)) size op))
        (vector-ref (Store-mem sto) (+ ptr index))])]
    ['aset!
     (check-args args 3 op)
     (match (arrayV* (first args))
       [(ArrayV ptr size)
        (define index (valid-index (numV-n* (second args)) size op))
        (vector-set! (Store-mem sto) (+ ptr index) (third args))
        (NullV)])]
    ['error
     (check-args args 1 op)
     (error 'interp "VEBG: user-error: ~a" (serialize (first args)))]
    [_ (error 'interp "VEBG: unknown primitive: ~e" op)]))

(: interp-args ((Listof ExprC) Env Store -> (Listof Value)))
(define (interp-args args env sto)
  (match args
    ['() '()]
    [(cons first-arg rest-args)
     (cons (interp first-arg env sto)
           (interp-args rest-args env sto))]))

;;;; interp function
; Purpose: Interprets expressions for evaluation
(: interp (ExprC Env Store -> Value))
(define (interp exp env sto)
  (match exp
    [(NumC n) (NumV n)]
    [(StringC str) (StringV str)]
    [(idC name)
     (vector-ref (Store-mem sto)
                 (lookup name env))]
    [(ifC test then else)
     (match (interp test env sto)
       [(BoolV #t) (interp then env sto)]
       [(BoolV #f) (interp else env sto)]
       [_ (error 'interp "VEBG: if test must be boolean in ~e" exp)])]
    [(fnC params body)
     (CloV params body env)]
    [(mutC name rhs)
     (define loc (lookup name env))
     (define new-val (interp rhs env sto))
     (vector-set! (Store-mem sto) loc new-val)
     (NullV)]
    [(CallC fun args)
     (define fun-val (interp fun env sto))
     (define arg-vals (interp-args args env sto))
     (match fun-val
       [(PrimV op)
        (apply-primitive op arg-vals sto)]
       [(CloV params body saved-env)
        (if (= (length params) (length arg-vals))
            (interp body
                    (extend-with-alloc saved-env params arg-vals sto)
                    sto)
            (error 'interp "VEBG: function arity mismatch in ~e" exp))]
       [_ (error 'interp "VEBG: not a function in ~e" exp)])]))

;;;; top-interp function
; Purpose: Parses and interprets any function
(: top-interp (Sexp Natural -> String))
(define (top-interp s memsize)
  (serialize (interp (parse s) initial-env (make-initial-store memsize))))

(: while Sexp)
(define while
  '(fn (guard body) ->
       (given ([loop = "bogus"] [done = 0])
         do
         (chain
          (loop := (fn () ->
                       (if (guard)
                           (chain (body) (loop))
                           (done := done))))
          (loop)))))

(: in-order Sexp)
(define in-order
  '(fn (arr size) ->
       (given ([i = 0] [ok = true])
         do
         (chain
          (while
           (fn () -> (if ok (<= (+ i 1) (- size 1)) false))
           (fn () ->
               (if (<= (aref arr (+ i 1)) (aref arr i))
                   (ok := false)
                   (i := (+ i 1)))))
          ok))))

;;;; TEST CASES
;; parse
(check-equal? (parse '5) (NumC 5))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse '"hello") (StringC "hello"))
(check-equal? (parse '(if true 1 2))
              (ifC (idC 'true) (NumC 1) (NumC 2)))
(check-equal? (parse '(+ 1 2))
              (CallC (idC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '(x := 10))
              (mutC 'x (NumC 10)))
(check-exn #rx"VEBG"
           (lambda () (parse #t)))
(check-exn #rx"VEBG"
           (lambda () (parse '(if true 1))))
(check-exn #rx"VEBG: duplicate parameter"
           (lambda () (parse '(fn (x x) -> x))))
(check-exn #rx"VEBG: duplicate given binding"
           (lambda () (parse '(given ([x = 1] [x = 2]) do x))))
(check-equal? (parse-binding-name '[x = 1]) 'x)
(check-exn #rx"VEBG: invalid binding name"
           (lambda () (parse-binding-name '[if = 1])))
(check-exn #rx"VEBG: invalid given binding"
           (lambda () (parse-binding-name '[x 1])))
(check-equal? (parse-binding-val '[x = 1]) (NumC 1))
(check-exn #rx"VEBG: invalid given binding"
           (lambda () (parse-binding-val '[x 1])))
;; serialize
(check-equal? (serialize (NumV 5)) "5")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StringV "hi")) "\"hi\"")
(check-equal? (serialize (PrimV '+)) "#<primop>")
(check-equal? (serialize (CloV (list 'x) (idC 'x) top-env)) "#<procedure>")
(check-equal? (serialize (ArrayV 0 3)) "#<array>")
(check-equal? (serialize (NullV)) "null")
;; interp
(check-equal? (top-interp '(+ 1 2) 100) "3")
(check-equal? (top-interp '(- 5 2) 100) "3")
(check-equal? (top-interp '(* 3 4) 100) "12")
(check-equal? (top-interp '(/ 8 2) 100) "4")
(check-equal? (top-interp '(<= 1 2) 100) "true")
(check-equal? (top-interp '(equal? 3 3) 100) "true")
(check-equal? (top-interp '(equal? "a" "a") 100) "true")
(check-equal? (top-interp '(if true 1 2) 100) "1")
(check-equal? (top-interp '(if false 1 2) 100) "2")
(check-equal? (top-interp '((fn (x) -> (+ x 1)) 5) 100) "6")
(check-equal? (top-interp '((fn (x y) -> (+ x y)) 3 4) 100) "7")
(check-equal? (top-interp '(given ([x = 3] [y = 4]) do (+ x y)) 100) "7")
(check-equal? (top-interp '(((fn (x) -> (fn (y) -> (+ x y))) 10) 5) 100) "15")
(check-equal? (top-interp '(strlen "hello") 100) "5")
(check-equal? (top-interp '(substring "hello" 1 4) 100) "\"ell\"")
(check-equal? (top-interp '(given ([x = 3]) do (chain (x := 9) x)) 100) "9")
(check-equal? (top-interp '(given ([x = 3]) do (x := 9)) 100) "null")
(define test-store2 (make-initial-store 100))
(check-equal?
 (interp (parse '(make-array 2 0)) top-env test-store2)
 (ArrayV 20 2))
(check-equal?
 (interp (parse '(make-array 3 9)) top-env test-store2)
 (ArrayV 22 3))
(define equal-order-store (make-initial-store 100))
(check-equal?
 (interp
  (parse '(equal? (make-array 2 0) (make-array 3 0)))
  top-env
  equal-order-store)
 (BoolV #f))
(define test-store3 (make-initial-store 100))
(check-equal?
 (interp
  (parse '(equal? (make-array 2 0) (make-array 3 0)))
  top-env
  test-store3)
 (BoolV #f))
(check-equal? (Store-next test-store3) 25)
(check-equal? (top-interp '(aref (array 10 20 30) 1) 100) "20")
(check-equal? (top-interp '(given ([a = (array 1 2 3)]) do (chain (aset! a 1 99) (aref a 1))) 100) "99")
(check-equal? (top-interp '(given ([a = (array 1 2)]) do (equal? a a)) 100) "true")
(check-equal? (top-interp '(equal? (array 1) (array 1)) 100) "false")
(check-equal? (top-interp '(given ([x = 0]) do (+ (chain (x := 10) x) x)) 100) "20")
(check-exn #rx"VEBG"
           (lambda () (top-interp '(+ 1 "bad") 100)))
(check-exn #rx"VEBG"
           (lambda () (top-interp '(/ 1 0) 100)))
(check-exn #rx"VEBG"
           (lambda () (top-interp '(if 1 2 3) 100)))
(check-exn #rx"VEBG"
           (lambda () (top-interp '(1 2) 100)))
(check-exn #rx"VEBG: invalid substring indexes"
           (lambda () (top-interp '(substring "hello" 4 1) 100)))
(check-exn #rx"VEBG: user-error"
           (lambda () (top-interp '(error "bad") 100)))
(check-exn #rx"VEBG: function arity mismatch"
           (lambda () (top-interp '((fn (x) -> x) 1 2) 100)))
(check-exn #rx"VEBG: make-array expected positive integer size"
           (lambda () (top-interp '(make-array 0 1) 100)))
(check-exn #rx"VEBG: array expected at least one value"
           (lambda () (top-interp '(array) 100)))
(check-exn #rx"VEBG: invalid array index"
           (lambda () (top-interp '(aref (array 1 2) 2) 100)))
(check-exn #rx"VEBG: invalid array index"
           (lambda () (top-interp '(aset! (array 1 2) -1 9) 100)))
(check-exn #rx"VEBG: out of memory"
           (lambda () (top-interp '(make-array 100 0) 25)))
;; misc
(check-equal? (lookup 'x (list (Binding 'x 5)))
              5)
(check-exn #rx"VEBG: unbound identifier"
           (lambda () (lookup 'x empty)))
(check-exn #rx"VEBG: expected string"
           (lambda () (stringV-str* (NumV 5))))
(check-exn #rx"VEBG: wrong number of arguments"
           (lambda () (check-args (list (NumV 1)) 2 '+)))
(check-equal? (value-equal? (BoolV #t) (BoolV #t)) #t)
(check-equal? (value-equal? (StringV "a") (StringV "a")) #t)
(check-equal? (value-equal? (PrimV '+) (PrimV '+)) #f)
(check-equal? (value-equal? (NullV) (NullV)) #t)
(check-exn #rx"VEBG: unknown primitive"
           (lambda () (apply-primitive 'bad empty (make-initial-store 100))))
(check-equal?
 (top-interp
  `(given ([while = "bogus"])
     do
     (chain
      (while := ,while)
      (given ([fact = "bogus"])
        do
        (chain
         (fact := (fn (x) -> (if (equal? x 0) 1 (* x (fact (- x 1))))))
         (fact 5)))))
  300)
 "120")
(check-exn #rx"VEBG: invalid mutation name"
           (lambda () (parse '(if := 10))))
(check-exn #rx"VEBG: unbound identifier"
           (lambda () (top-interp '(x := 10) 100)))
(check-exn #rx"VEBG: extend-with-alloc arity mismatch"
           (lambda ()
             (extend-with-alloc top-env
                                (list 'x 'y)
                                (list (NumV 1))
                                (make-initial-store 100))))
(check-exn #rx"VEBG: expected array"
           (lambda () (top-interp '(aref 5 0) 100)))
(check-exn #rx"VEBG: expected array"
           (lambda () (top-interp '(aset! 5 0 99) 100)))
(check-equal? (value->string (StringV "hello")) "hello")
(check-equal? (value->string (NumV 42)) "42")
(check-equal? (value->string (BoolV #t)) "true")
(check-equal? (value->string (BoolV #f)) "false")
(check-equal? (value->string (NullV)) "null")
(check-exn #rx"VEBG: value->string cannot convert value to string"
           (lambda () (value->string (PrimV '+))))
(check-exn #rx"VEBG: value->string cannot convert value to string"
           (lambda () (value->string (ArrayV 20 3))))
(check-exn #rx"VEBG: chain needs at least one expression"
           (lambda ()
             (apply-primitive 'chain empty (make-initial-store 100))))
(check-exn #rx"VEBG: \\+\\+ expected at least one argument"
           (lambda ()
             (apply-primitive '++ empty (make-initial-store 100))))
(check-exn #rx"VEBG: memory size too small for initial environment"
           (lambda () (make-initial-store 5)))
(check-equal?
 (apply-primitive 'println
                  (list (StringV "hi"))
                  (make-initial-store 100))
 (BoolV #t))
(check-exn #rx"VEBG: println expected a string"
           (lambda ()
             (apply-primitive 'println
                              (list (NumV 5))
                              (make-initial-store 100))))
(check-exn #rx"VEBG: wrong number of arguments"
           (lambda ()
             (apply-primitive 'read-num
                              (list (NumV 1))
                              (make-initial-store 100))))
(check-exn #rx"VEBG: wrong number of arguments"
           (lambda ()
             (apply-primitive 'read-str
                              (list (StringV "x"))
                              (make-initial-store 100))))
(check-equal?
 (apply-primitive '++
                  (list (StringV "a") (NumV 3) (BoolV #t) (NullV))
                  (make-initial-store 100))
 (StringV "a3truenull"))
(check-equal?
 (apply-primitive 'chain
                  (list (NumV 1) (StringV "last"))
                  (make-initial-store 100))
 (StringV "last"))
(check-equal?
 (top-interp
  `(given ([while = "bogus"] [in-order = "bogus"])
     do
     (chain
      (while := ,while)
      (in-order := ,in-order)
      (in-order (array 1 2 3 4) 4)))
  500)
 "true")
(check-equal?
 (top-interp
  `(given ([while = "bogus"] [in-order = "bogus"])
     do
     (chain
      (while := ,while)
      (in-order := ,in-order)
      (in-order (array 1 3 2 4) 4)))
  500)
 "false")
(check-equal?
 (top-interp
  `(given ([while = ,while])
     do
     (given ([in-order = ,in-order])
       do
       (in-order (array 1 2 3 4) 4)))
  500)
 "true")
(check-equal?
 (top-interp
  `(given ([while = ,while])
     do
     (given ([in-order = ,in-order])
       do
       (in-order (array 1 3 2 4) 4)))
  500)
 "false")
(check-equal?
 (parameterize ([current-input-port (open-input-string "42\n")])
   (apply-primitive 'read-num empty (make-initial-store 100)))
 (NumV 42))
(check-exn #rx"VEBG: read-num expected a real number"
           (lambda ()
             (parameterize ([current-input-port (open-input-string "bad\n")])
               (apply-primitive 'read-num empty (make-initial-store 100)))))
(check-exn #rx"VEBG: read-num expected input"
           (lambda ()
             (parameterize ([current-input-port (open-input-string "")])
               (apply-primitive 'read-num empty (make-initial-store 100)))))
(check-equal?
 (parameterize ([current-input-port (open-input-string "hello\n")])
   (apply-primitive 'read-str empty (make-initial-store 100)))
 (StringV "hello"))
(check-exn #rx"VEBG: read-str expected input"
           (lambda ()
             (parameterize ([current-input-port (open-input-string "")])
               (apply-primitive 'read-str empty (make-initial-store 100)))))
