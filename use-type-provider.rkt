#lang typed/racket

(require "schema-type-provider.rkt"
         typed/json)

(provide (struct-out Person))

(schema-type-provider "test-schema.txt")

(Person-lastName (Person 10 "Jason" "hello"))
