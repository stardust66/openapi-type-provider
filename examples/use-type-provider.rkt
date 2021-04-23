#lang typed/racket

(require "../schema-type-provider.rkt")

(schema-type-provider "../schemas/person-schema.json")
(Person-lastName (Person 10 "hello" "world"))

; Reading
(define input-data
  (read-json (open-input-string "{ \"age\": 10, \"firstName\": \"hello\", \"lastName\": \"world\" }")))
(when (not (eof-object? input-data))
  (read-Person input-data))

; Writing
(jsexpr->string (write-Person (Person 10 "hello" "world")))
