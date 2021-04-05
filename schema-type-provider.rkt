#lang typed/racket/base

(provide schema-type-provider
         (for-syntax schema->typedef
                     schema->field-definition
                     schema->writer
                     schema->read
                     schema->jsexpr)
         JSExpr
         string->jsexpr
         jsexpr->string
         read-json)

(require (for-syntax racket/base
                     racket/syntax
                     "json-schema.rkt")
         typed/json)

(begin-for-syntax
  (define (upper-name name)
    (string->symbol (string-titlecase (symbol->string name))))

  (define (schema->field-definition schema)
    (define name (Schema-name schema))
    (define typename (schema->typename schema))
    `[,name : ,typename])

  (define (schema->typename schema)
    (cond
      [(Schema-String? schema) `String]
      [(Schema-Integer? schema) `Integer]
      [(Schema-Number? schema) `Number]
      [(Schema-Object? schema) (upper-name (Schema-name schema))]))

  (define ((schema->hashref hashtable) schema)
    (define name (Schema-name schema))
    (define typename (schema->typename schema))
    `(cast (hash-ref ,hashtable (quote ,name)) ,typename))

  (define (schema->read schema)
    (define name (Schema-name schema))
    (define read-name (format-symbol "read-~a" name))
    (define properties (Schema-Object-properties schema))
    `(begin
       (: ,read-name (-> Input-Port ,name))
       (define (,read-name in)
         (define contents (cast (read-json in) (HashTable Symbol JSExpr)))
         (,name ,@(map (schema->hashref 'contents) properties)))))

  (define (schema->writer schema)
    (cond
      [(Schema-String? schema) 'values]
      [(Schema-Number? schema) 'number->string]
      [(Schema-Integer? schema) 'number->string]
      [(Schema-Object? schema) (format-symbol "write-~a" (Schema-name schema))]))

  (define (schema->jsexpr schema)
    (define properties (Schema-Object-properties schema))
    (define name (Schema-name schema))
    (define write-name (format-symbol "write-~a" name))
    `(begin
       (: ,write-name (-> ,name String))
       (define (,write-name obj)
         (jsexpr->string
          (make-hasheq
           (list
            ,@(map
               (λ (property)
                 (define property-name (Schema-name property))
                 (define getter (format-symbol "~a-~a" name property-name))
                 `(cons (quote ,property-name)
                        (,(schema->writer property) (,getter obj))))
               properties)))))))

  (define (schema->typedef schema)
    (define name (upper-name (Schema-name schema)))
    (cond
      [(Schema-Object? schema)
       (define field-defs
         (map schema->field-definition (Schema-Object-properties schema)))
       `(struct ,name (,@field-defs) #:transparent)])))

(define-syntax (schema-type-provider stx)
  (syntax-case stx ()
    [(_ filename)
     (begin
       (define schema (file->json-schema (syntax-e #'filename)))
       (define definitions
         `(begin
            ,(schema->typedef schema)
            ,(schema->read schema)
            ,(schema->jsexpr schema)))
       (datum->syntax stx definitions stx stx))]))
