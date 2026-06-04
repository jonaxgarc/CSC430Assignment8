#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC idC StringC ifC fnC CallC))
(struct NumC ([n : Real]) #:transparent)
(struct idC ([name : Symbol]) #:transparent)
(struct StringC ([str : String]) #:transparent)
(struct ifC ([test : ExprC][then : ExprC][else : ExprC]) #:transparent)
(struct fnC ([params : (Listof Symbol)][body : ExprC]) #:transparent)
(struct CallC ([fun : ExprC][exs : (Listof ExprC)]) #:transparent)