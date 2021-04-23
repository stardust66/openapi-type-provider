#lang info

(define version "0.1")

(define collection "openapi-type-provider")

(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"))

(define pkg-desc "Generate well typed client library from OpenAPI schema.")

(define pkg-authors '("kbtpodifo@gmail.com"))

(define scribblings '(("scribblings/openapi-type-provider.scrbl" ())))
