#lang typed/racket/base

(require typed/json)

(provide (struct-out Schema)
         (struct-out Schema-Object)
         (struct-out Schema-String)
         (struct-out Schema-Integer)
         (struct-out Schema-Number)
         file->json-schema
         make-schema-with-name)

(struct Schema ([name : Symbol]) #:transparent)
(struct Schema-Object Schema ([properties : (Listof Schema)]))
(struct Schema-String Schema ())
(struct Schema-Integer Schema ())
(struct Schema-Number Schema ())

(define (make-properties [properties : JSExpr])
  (assert properties hash?)
  (hash-map
   properties
   make-schema-with-name))

(: make-schema-with-name (-> Symbol JSExpr Schema))
(define (make-schema-with-name name contents)
  (assert contents hash?)
  (case (hash-ref contents 'type)
    [("object")
     (Schema-Object name (make-properties (hash-ref contents 'properties)))]
    [("string") (Schema-String name)]
    [("integer") (Schema-Integer name)]
    [("number") (Schema-Number name)]
    [else (error "Type not supported")]))

(define (parse-json-schema [contents : JSExpr])
  (cond
    [(hash? contents)
     (define title-expr (hash-ref contents 'title))
     (assert title-expr string?)
     (make-schema-with-name (string->symbol title-expr) contents)]
    [else (error "contents must be hashtable")]))

(: file->json-schema (->* (String) (Symbol) Schema))
(define (file->json-schema filename [name #f])
  (with-input-from-file filename
    (λ ()
      (define contents (read-json))
      (if name
          (make-schema-with-name name (cast contents JSExpr))
          (parse-json-schema (cast contents JSExpr))))))
