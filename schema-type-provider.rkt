#lang typed/racket/base

(provide schema-type-provider)

(require (for-syntax racket/base))

(module for-syntax-mod typed/racket/base
  (provide file->json-schema
           schema->struct
           schema->read)

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

  (module untyped-mod racket/base
    (require racket/syntax
             "json-schema.rkt")
    
    (provide schema->read)

    (define (schema->read schema)
      (define name (Schema-name schema))
      (define read-name (format-symbol "read-~a" name))
      (define property-names (map Schema-name (Schema-Object-properties schema)))
      `(define (,read-name jsonstr)
         (define contents (string->jsexpr jsonstr))
         (define properties
           (for/list ([p (in-list (quote ,property-names))])
             (hash-ref contents p)))
         (apply ,name properties))))

  (require/typed 'untyped-mod
    [schema->read (-> Schema-Object Any)])

  (define (schema->struct [schema : Schema-Object])
    (define name (Schema-name schema))
    `(struct ,name (,@(map schema->type (Schema-Object-properties schema))))))

(require (for-syntax 'for-syntax-mod))

(define-syntax (schema-type-provider stx)
  (syntax-case stx ()
    [(_ filename)
     (begin
       (define schema (file->json-schema (syntax->datum #'filename)))
       (define definitions
         `(begin
            ,(schema->struct schema)
            ,(schema->read schema)))
       (displayln definitions)
       (datum->syntax stx definitions))]
    [(_ filename name)
     (begin
       (define schema (file->json-schema (syntax->datum #'filename)
                                         (syntax->datum #'name)))
       (define definitions (schema->struct schema))
       (datum->syntax stx definitions))]))
