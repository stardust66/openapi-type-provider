#lang typed/racket/base

(provide schema-type-provider)

(require (for-syntax racket/base
                     racket/syntax
                     "json-schema.rkt"))

(begin-for-syntax
  (define (schema->field-definition schema)
    (define name (Schema-name schema))
    (define typename (schema->type schema))
    `[,name : ,typename])

  (define (schema->type schema)
    (cond
      [(Schema-String? schema) `String]
      [(Schema-Integer? schema) `Integer]
      [(Schema-Number? schema) `Number]
      [(Schema-Object? schema) (Schema-name schema)]))

  (define ((schema->hashref hashtable) schema)
    (define name (Schema-name schema))
    (define typename (schema->type schema))
    `(cast (hash-ref ,hashtable (quote ,name)) ,typename))

  (define (schema->read schema)
    (define name (Schema-name schema))
    (define read-name (format-symbol "read-~a" name))
    (define property-names (map Schema-name (Schema-Object-properties schema)))
    (define properties (Schema-Object-properties schema))
    `(begin
       (: ,read-name (-> String ,name))
       (define (,read-name jsonstr)
         (define contents (cast (string->jsexpr jsonstr) (HashTable Symbol JSExpr)))
         (,name ,@(map (schema->hashref 'contents) properties)))))

  (define (schema->struct schema)
    (define name (Schema-name schema))
    (define field-defs
      (map schema->field-definition (Schema-Object-properties schema)))
    `(struct ,name (,@field-defs) #:transparent)))

(define-syntax (schema-type-provider stx)
  (syntax-case stx ()
    [(_ filename)
     (begin
       (define schema (file->json-schema (syntax-e #'filename)))
       (define definitions
         `(begin
            ,(schema->struct schema)
            ,(schema->read schema)))
       (displayln definitions)
       (datum->syntax stx definitions stx stx))]))
