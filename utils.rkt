#lang typed/racket/base

(provide (all-defined-out))

(require typed/json)

(: json-ref : (case->
               [JSExpr Symbol -> JSExpr]
               [JSExpr Symbol JSExpr -> JSExpr]))
(define json-ref
  (case-lambda
    [([object : JSExpr]
      [key : Symbol])
     (hash-ref (assert object hash?) key)]
    [([object : JSExpr]
      [key : Symbol]
      [failure-result : JSExpr])
     (hash-ref (assert object hash?) key (λ () failure-result))]))

(define (json-refs [object : JSExpr] [keys : (Listof Symbol)]) : JSExpr
  (foldl
   (λ ([key : Symbol] [obj : JSExpr]) (json-ref obj key))
   object
   keys))

(define (json-has-key? [object : JSExpr] [key : Symbol]) : Boolean
  (hash-has-key? (assert object hash?) key))

(define (json-has-keys? [object : JSExpr] [keys : (Listof Symbol)]) : Boolean
  (cond
    [(null? keys) #t]
    [else
     (and (json-has-key? object (car keys))
          (json-has-keys? (json-ref object (car keys)) (cdr keys)))]))


(define (upper-name [name : Symbol]) : Symbol
  (string->symbol (string-titlecase (symbol->string name))))
