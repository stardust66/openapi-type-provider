#lang typed/racket

(require "schema-type-provider.rkt"
         typed/json)

(schema-type-provider "test-schema.txt")
(Person-lastName (Person 10 "hello" "world"))
(read-Person (string-replace "{ 'age': 10, 'firstName': 'hello', 'lastName': 'world' }" "'" "\""))
