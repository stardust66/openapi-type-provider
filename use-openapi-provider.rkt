#lang typed/racket/base

(require "openapi-type-provider.rkt")

(openapi-type-provider "open-weather-map-api.json")

(define result
  (/weather "c29ade87c3bc9b61d4b0737c11f640ac" "London,uk" "" "" "" "" "" "" ""))

result

(write-|200| result)
