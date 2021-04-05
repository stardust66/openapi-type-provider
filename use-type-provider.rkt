#lang typed/racket

(require "schema-type-provider.rkt")

(schema-type-provider "test-schema.txt")
(Person-lastName (Person 10 "hello" "world"))

; Reading
(read-Person (open-input-string (string-replace "{ 'age': 10, 'firstName': 'hello', 'lastName': 'world' }" "'" "\"")))

; Writing
(write-Person (Person 10 "hello" "world"))
