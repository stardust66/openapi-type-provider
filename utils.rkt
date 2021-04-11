#lang typed/racket/base

(provide json-ref
         json-refs)

(require typed/json)

(: json-ref (-> JSExpr Symbol JSExpr))
(define (json-ref object key)
  (hash-ref (assert object hash?) key))

(: json-refs (-> JSExpr (Listof Symbol) JSExpr))
(define (json-refs object keys)
  (foldl
   (λ ([key : Symbol] [obj : JSExpr]) (json-ref obj key))
   object
   keys))
