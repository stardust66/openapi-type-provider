#lang typed/racket/base

(require typed/json
         racket/string
         racket/list
         "utils.rkt")

(provide (struct-out Schema)
         (struct-out Schema-Object)
         (struct-out Schema-String)
         (struct-out Schema-Integer)
         (struct-out Schema-Number)
         (struct-out Schema-Boolean)
         (struct-out Schema-Array)
         (struct-out Property)
         file->json-schema
         make-schema-with-name
         make-properties
         ref-string->list
         trace-ref)

(struct Schema () #:transparent)
(struct Schema-Object Schema ([name : Symbol] [properties : (Listof Property)]))
(struct Schema-String Schema ())
(struct Schema-Integer Schema ())
(struct Schema-Number Schema ())
(struct Schema-Boolean Schema ())
(struct Schema-Array Schema ([items : Schema]))
(struct Property ([name : Symbol] [schema : Schema]) #:transparent)

(define (make-properties [properties : JSExpr] [top : JSExpr])
  (hash-map
   (assert properties hash?)
   (λ ([name : Symbol] [contents : JSExpr])
     (Property name
               (make-schema-with-name name contents top)))))

(define (ref-string->list [ref : String]) : (Listof Symbol)
  (map string->symbol (string-split (string-trim ref "#") "/")))

(define (trace-ref [ref : String] [top : JSExpr]) : JSExpr
  (json-refs top (ref-string->list ref)))

(define (make-schema-with-name [name : Symbol]
                               [contents : JSExpr]
                               [top : JSExpr]) : Schema
  (with-asserts ([contents hash?])
    (cond
      [(hash-has-key? contents '$ref)
       (define ref-string (assert (hash-ref contents '$ref) string?))
       (define schema-content (trace-ref ref-string top))
       (make-schema-with-name name schema-content top)]
      [else
       (define type
         (with-handlers
           ([exn:fail?
             (λ (e)
               (error
                (format "Error when creating schema '~a': no type found.\nGiven ~a"
                        name
                        contents)))])
           (assert (hash-ref contents 'type) string?)))
       (case type
         [("object")
          (Schema-Object
           name
           (make-properties
            (hash-ref contents
                      'properties
                      (λ () (cast (hash) JSExpr)))
            top))]
         [("string") (Schema-String)]
         [("integer") (Schema-Integer)]
         [("number") (Schema-Number)]
         [("boolean") (Schema-Boolean)]
         [("array")
          (Schema-Array
           (make-schema-with-name (gensym) (hash-ref contents 'items) top))]
         [else (error (format "Type not supported: ~a" type))])])))

(define (parse-json-schema [contents : JSExpr])
  (cond
    [(hash? contents)
     (define title-expr (assert (hash-ref contents 'title) string?))
     (make-schema-with-name (string->symbol title-expr) contents contents)]
    [else (error "Schema must be a JSON object.")]))

(: file->json-schema (->* (String) (Symbol) Schema))
(define (file->json-schema filename [name #f])
  (with-input-from-file filename
    (λ ()
      (define contents (cast (read-json) JSExpr))
      (if name
          (make-schema-with-name name contents contents)
          (parse-json-schema contents)))))
