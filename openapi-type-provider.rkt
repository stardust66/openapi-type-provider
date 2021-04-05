#lang typed/racket/base

(provide openapi-type-provider
         string->jsexpr
         http-sendrecv
         alist->form-urlencoded
         read-json
         JSExpr
         jsexpr->string)

(require "schema-type-provider.rkt"
         (for-syntax "openapi.rkt"
                     "json-schema.rkt"
                     racket/base
                     racket/list)
         typed/json
         typed/net/http-client
         typed/net/uri-codec)

(begin-for-syntax
  (define (api->types api)
    (append-map (λ (route)
                  (append
                   (list (schema->typedef (Route-response route))
                         (schema->read (Route-response route))
                         (schema->jsexpr (Route-response route)))
                   (map (λ (param)
                          (schema->typedef param))
                        (Route-parameters route))))
                (API-routes api)))

  ;; Generate methods for sending requests defined in API struct
  (define (api->methods api)
    (map
     (λ (route)
       (define name (string->symbol (Route-path route)))
       (define parameters (Route-parameters route))
       (define fields (map schema->field-definition parameters))
       `(define (,name [appid : String] ,@fields)
          (define params
            (list
             (cons 'appid appid)
             ,@(map
                (λ (property)
                  (define writer (schema->writer property))
                  (define property-name (Schema-name property))
                  `(cons (quote ,property-name) (,writer ,property-name)))
                parameters)))
          (define query (alist->form-urlencoded params))
          (define-values (status headers in)
            (http-sendrecv
             ,(API-hostname api)
             (string-append ,(API-path-prefix api) ,(Route-path route) "?" query)))
          (read-|200| in)))
     (API-routes api))))

(define-syntax (openapi-type-provider stx)
  (syntax-case stx ()
    [(_ filename)
     (begin
       (define api (file->openapi (syntax-e #'filename)))
       (define defs (api->types api))
       (define methods (api->methods api))
       (define final `(begin ,@defs ,@methods))
       #;(displayln final)
       (datum->syntax stx final))]))
