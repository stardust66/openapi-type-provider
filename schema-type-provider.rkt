#lang typed/racket/base

(provide schema-type-provider)

(require (for-syntax racket/base))

(module for-syntax-mod typed/racket/base
  (provide file->json-schema
           schema->struct)
  
  (require/typed "json-schema.rkt"
    [#:struct Schema ([name : Symbol])]
    [#:struct (Schema-String Schema) ()]
    [#:struct (Schema-Object Schema) ([properties : (Listof Schema)])]
    [#:struct (Schema-Integer Schema) ()]
    [#:struct (Schema-Number Schema) ()]
    [file->json-schema (->* (String) (String) Schema)])

  (define (schema->type [schema : Schema])
    (define name (Schema-name schema))
    (cond
      [(Schema-String? schema) `[,name : String]]
      [(Schema-Integer? schema) `[,name : Integer]]
      [(Schema-Number? schema) `[,name : Number]]
      [(Schema-Object? schema) `[,name : ,name]]))

  (define (schema->struct [schema : Schema-Object])
    (define name (Schema-name schema))
    `(struct ,name (,@(map schema->type (Schema-Object-properties schema))))))

(require (for-syntax 'for-syntax-mod))

(define-syntax (schema-type-provider stx)
  (syntax-case stx ()
    [(_ filename)
     (begin
       (define schema (file->json-schema (syntax->datum #'filename)))
       (define definitions (schema->struct schema))
       (displayln definitions)
       (datum->syntax stx definitions))]
    [(_ filename name)
     (begin
       (define schema (file->json-schema (syntax->datum #'filename)
                                         (syntax->datum #'name)))
       (define definitions (schema->struct schema))
       (datum->syntax stx definitions))]))
