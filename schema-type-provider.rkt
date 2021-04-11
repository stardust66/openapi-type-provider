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
      [(Schema-Number? schema) `Inexact-Real]
      [(Schema-Boolean? schema) `Boolean]
      [(Schema-Object? schema) (upper-name (Schema-name schema))]
      [(Schema-Array? schema)
       `(Listof ,(schema->typename (Schema-Array-items schema)))]))

  (define ((schema->hashref hashtable) schema)
    (define name (Schema-name schema))
    (define content `(hash-ref ,hashtable (quote ,name)))
    (cond
      [(Schema-Object? schema)
       (define reader (format-symbol "read-~a" (schema->typename schema)))
       `(,reader (cast ,content (HashTable Symbol JSExpr)))]
      [(Schema-Array? schema)
       (define item (Schema-Array-items schema))
       (define typename (schema->typename item))
       (define reader
         (if (Schema-Object? item)
             (format-symbol "read-~a" typename)
             `(λ (c) (cast c typename))))
       `(map ,reader (cast ,content (Listof (HashTable Symbol JSExpr))))]
      [else
       (define typename (schema->typename schema))
       `(cast ,content ,typename)]))

  (define (schema->read schema)
    (cond
      [(Schema-Object? schema)
       (define name (upper-name (Schema-name schema)))
       (define read-name (format-symbol "read-~a" name))
       (define properties (Schema-Object-properties schema))
       `(begin
          ,@(map schema->read properties)
          (: ,read-name (-> (HashTable Symbol JSExpr) ,name))
          (define (,read-name contents)
            (,name ,@(map (schema->hashref 'contents) properties))))]
      [(Schema-Array? schema)
       (schema->read (Schema-Array-items schema))]))

  (define (schema->writer schema)
    (cond
      [(Schema-Array? schema)
       (define items (Schema-Array-items schema))
       (define item-writer (schema->writer items))
       `(λ (l)
          (map ,item-writer l))]
      [(Schema-Object? schema)
       (format-symbol "write-~a" (schema->typename schema))]
      [else 'values]))

  (define (schema->jsexpr schema)
    (cond
      [(Schema-Object? schema)
       (define properties (Schema-Object-properties schema))
       (define name (schema->typename schema))
       (define write-name (format-symbol "write-~a" name))
       `(begin
          ,@(map schema->jsexpr properties)
          (: ,write-name (-> ,name JSExpr))
          (define (,write-name obj)
            (make-hasheq
             (list
              ,@(map
                 (λ (property)
                   (define property-name (Schema-name property))
                   (define getter (format-symbol "~a-~a" name property-name))
                   `(cast
                     (cons (quote ,property-name)
                           (,(schema->writer property) (,getter obj)))
                     (Pairof Symbol JSExpr)))
                 properties)))))]
      [(Schema-Array? schema)
       (schema->jsexpr (Schema-Array-items schema))]))

  (define (schema->typedef schema)
    (define name (upper-name (Schema-name schema)))
    (cond
      [(Schema-Object? schema)
       (define properties (Schema-Object-properties schema))
       (define field-defs
         (map schema->field-definition properties))
       `(begin
          ,@(map schema->typedef properties)
          (struct ,name (,@field-defs) #:transparent))]
      [(Schema-Array? schema)
       (schema->typedef (Schema-Array-items schema))])))

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
