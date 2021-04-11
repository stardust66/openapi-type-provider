#lang typed/racket/base

(require typed/json
         racket/string
         "utils.rkt")

(provide (struct-out Schema)
         (struct-out Schema-Object)
         (struct-out Schema-String)
         (struct-out Schema-Integer)
         (struct-out Schema-Number)
         (struct-out Schema-Boolean)
         (struct-out Schema-Array)
         file->json-schema
         make-schema-with-name)

(struct Schema ([name : Symbol]) #:transparent)
(struct Schema-Object Schema ([properties : (Listof Schema)]))
(struct Schema-String Schema ())
(struct Schema-Integer Schema ())
(struct Schema-Number Schema ())
(struct Schema-Boolean Schema ())
(struct Schema-Array Schema ([items : Schema]))

(define (make-properties [properties : JSExpr] [top : JSExpr])
  (hash-map
   (assert properties hash?)
   (λ ([name : Symbol] [contents : JSExpr])
     (make-schema-with-name name contents top))))

(: trace-ref (-> String JSExpr JSExpr))
(define (trace-ref [ref : String] [top : JSExpr])
  (define path (map string->symbol (string-split (string-trim ref "#") "/")))
  (json-refs top path))

;; Version of make-schema-with-name where name is not known. We have to deduce
;; the name from the "title" field in the case of an object, or just use a
;; gensym symbol as the name because it doesn't matter what the name of the
;; schema is.
(: make-schema (-> JSExpr JSExpr Schema))
(define (make-schema contents top)
  (assert contents hash?)
  (cond
    [(hash-has-key? contents '$ref)
     (make-schema
      (trace-ref (assert (hash-ref contents '$ref) string?) top)
      top)]
    [else
     (define type (assert (hash-ref contents 'type) string?))
     (case type
       [("object")
        (define name
          (string->symbol (assert (json-ref contents 'title) string?)))
        (make-schema-with-name name contents top)]
       [else
        (make-schema-with-name (gensym) contents top)])]))

(: make-schema-with-name (-> Symbol JSExpr JSExpr Schema))
(define (make-schema-with-name name contents top)
  (assert contents hash?)
  (cond
    [(hash-has-key? contents '$ref)
     (make-schema-with-name
      name
      (trace-ref (assert (hash-ref contents '$ref) string?) top)
      top)]
    [else
     (define type (assert (hash-ref contents 'type) string?))
     (case type
       [("object")
        (Schema-Object
         name
         (make-properties (hash-ref contents 'properties) top))]
       [("string") (Schema-String name)]
       [("integer") (Schema-Integer name)]
       [("number") (Schema-Number name)]
       [("boolean") (Schema-Boolean name)]
       [("array")
        (Schema-Array
         name
         (make-schema (hash-ref contents 'items) top))]
       [else (error (format "Type not supported: ~a" type))])]))

(define (parse-json-schema [contents : JSExpr])
  (cond
    [(hash? contents)
     (define title-expr (assert (hash-ref contents 'title) string?))
     (make-schema-with-name (string->symbol title-expr) contents contents)]
    [else (error "contents must be hashtable")]))

(: file->json-schema (->* (String) (Symbol) Schema))
(define (file->json-schema filename [name #f])
  (with-input-from-file filename
    (λ ()
      (define contents (cast (read-json) JSExpr))
      (if name
          (make-schema-with-name name contents contents)
          (parse-json-schema contents)))))
