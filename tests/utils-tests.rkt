#lang racket/base

(require rackunit
         typed/json
         "../utils.rkt")

(test-case
    "simple json-refs"
  (check-equal?
   (json-refs (string->jsexpr "{\"one\": {\"two\": { \"three\": 10 }}}")
              '(one two three))
   10))
