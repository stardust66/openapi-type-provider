#lang typed/racket/base

(require "../openapi-type-provider.rkt"
         racket/port)

(openapi-type-provider "../schemas/open-weather-map-api-original.json")

;; Right now, since I've removed the hard coded authentication which only works
;; for Open Weather Map, this requests results in a 401 response, which is not
;; documented in the OpenAPI schema. So, this throws a custom error.
(define result
  (/weather "London,uk" "" "" "" "" "" "" ""))

result
