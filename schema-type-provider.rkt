#lang typed/racket/base

(provide schema-type-provider
         (for-syntax schema->typedef
                     property->field-definition
                     schema->writer
                     schema->read
                     schema->jsexpr
                     schema->typename)
         JSExpr
         string->jsexpr
         jsexpr->string
         read-json)

(require (for-syntax racket/base
                     racket/syntax
                     racket/set
                     racket/pretty
                     "json-schema.rkt")
         typed/json)

(begin-for-syntax
  (define (property->field-definition property)
    (define name (Property-name property))
    (define typename (schema->typename (Property-schema property)))
    `[,name : ,typename])

  (define (schema->typename schema)
    (cond
      [(Schema-String? schema) `String]
      [(Schema-Integer? schema) `Integer]
      [(Schema-Number? schema) `Inexact-Real]
      [(Schema-Boolean? schema) `Boolean]
      [(Schema-Object? schema) (Schema-Object-name schema)]
      [(Schema-Array? schema)
       `(Listof ,(schema->typename (Schema-Array-items schema)))]))

  (define ((property->hashref hashtable) property)
    (define name (Property-name property))
    (define schema (Property-schema property))
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
             `(λ (c) (cast c ,typename))))
       `(map ,reader (cast ,content (Listof (HashTable Symbol JSExpr))))]
      [else
       (define typename (schema->typename schema))
       `(cast ,content ,typename)]))

  (define (schema->read schema seen-names)
    (cond
      [(Schema-Object? schema)
       (define name (Schema-Object-name schema))
       (when (not (set-member? seen-names name))
         (set-add! seen-names name)
         (define read-name (format-symbol "read-~a" name))
         (define properties (Schema-Object-properties schema))
         `(begin
            ,@(map
               (λ (p) (schema->read (Property-schema p) seen-names))
               properties)
            (: ,read-name (-> (HashTable Symbol JSExpr) ,name))
            (define (,read-name contents)
              (,name ,@(map (property->hashref 'contents) properties)))))]
      [(Schema-Array? schema)
       (schema->read (Schema-Array-items schema) seen-names)]))

  ;; Given a Schema, return the function (as a Symbol) that turns an instance
  ;; of that Schema into a JSExpr value.
  (define (schema->writer schema)
    (cond
      [(Schema-Array? schema)
       (define items (Schema-Array-items schema))
       (define item-writer (schema->writer items))
       (if (equal? item-writer 'values)
           'values
           `(λ (l)
              (map ,item-writer l)))]
      [(Schema-Object? schema)
       (format-symbol "write-~a" (schema->typename schema))]
      [else 'values]))

  (define (schema->jsexpr schema seen-names)
    (cond
      [(Schema-Object? schema)
       (define name (schema->typename schema))
       (when (not (set-member? seen-names name))
         (set-add! seen-names name)
         (define properties (Schema-Object-properties schema))
         (define write-name (format-symbol "write-~a" name))
         `(begin
            ,@(map
               (λ (p) (schema->jsexpr (Property-schema p) seen-names))
               properties)
            (: ,write-name (-> ,name JSExpr))
            (define (,write-name obj)
              (make-hasheq
               (list
                ,@(map
                   (λ (property)
                     (define property-name (Property-name property))
                     (define property-schema (Property-schema property))
                     (define getter (format-symbol "~a-~a" name property-name))
                     `(cast
                       (cons (quote ,property-name)
                             (,(schema->writer property-schema) (,getter obj)))
                       (Pairof Symbol JSExpr)))
                   properties))))))]
      [(Schema-Array? schema)
       (schema->jsexpr (Schema-Array-items schema) seen-names)]))

  (define (schema->typedef schema seen-names)
    (cond
      [(Schema-Object? schema)
       (define name (Schema-Object-name schema))
       (when (not (set-member? seen-names name))
         (set-add! seen-names name)
         (define properties (Schema-Object-properties schema))
         (define field-defs
           (map property->field-definition properties))

         ;; Also generate all type definitions that the current one refers to
         `(begin
            ,@(map
               (λ (p) (schema->typedef (Property-schema p) seen-names))
               properties)
            (struct ,name (,@field-defs) #:transparent)))]
      [(Schema-Array? schema)
       (schema->typedef (Schema-Array-items schema) seen-names)])))

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
