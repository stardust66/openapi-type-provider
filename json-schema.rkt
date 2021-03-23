#lang racket/base

(require json)

(provide (struct-out Schema)
         (struct-out Schema-Object)
         (struct-out Schema-String)
         (struct-out Schema-Integer)
         (struct-out Schema-Number)
         file->json-schema)

(struct Schema (name) #:transparent)
(struct Schema-Object Schema (properties))
(struct Schema-String Schema ())
(struct Schema-Integer Schema ())
(struct Schema-Number Schema ())

(define (make-properties properties)
  (hash-map
   properties
   make-schema-with-name))

(define (make-schema-with-name name contents)
  (case (hash-ref contents 'type)
    [("object")
     (Schema-Object name (make-properties (hash-ref contents 'properties)))]
    [("string") (Schema-String name)]
    [("integer") (Schema-Integer name)]
    [("number") (Schema-Number name)]))

(define (parse-json-schema contents)
  (make-schema-with-name (string->symbol (hash-ref contents 'title)) contents))

(define (file->json-schema filename [name #f])
  (with-input-from-file filename
    (λ ()
      (define contents (read-json))
      (if name
          (make-schema-with-name name contents)
          (parse-json-schema contents)))))
