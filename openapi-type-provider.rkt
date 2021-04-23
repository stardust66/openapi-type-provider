#lang typed/racket/base

(provide openapi-type-provider
         string->jsexpr
         http-sendrecv
         alist->form-urlencoded
         read-json
         JSExpr
         jsexpr->string
         string-join
         string-split)

(require "schema-type-provider.rkt"
         (for-syntax "openapi.rkt"
                     "json-schema.rkt"
                     racket/base
                     racket/list
                     racket/syntax
                     racket/pretty
                     racket/set)
         racket/string
         typed/json
         typed/net/http-client
         typed/net/uri-codec)

(begin-for-syntax
  (define (param->field-definition param)
    (define name (Param-name param))
    (define typename (schema->typename (Param-schema param)))
    `[,name : ,typename])

  (define (api->types api)
    (define typedef-seen (mutable-set))
    (define read-seen (mutable-set))
    (define write-seen (mutable-set))
    (append-map (λ (route)
                  (append
                   (append-map
                    (λ (response)
                      (define schema (Response-schema response))
                      (list
                       (schema->typedef schema typedef-seen)
                       (schema->read schema read-seen)
                       (schema->jsexpr schema write-seen)))
                    (Route-responses route))
                   (map (λ (param)
                          (schema->typedef param typedef-seen))
                        (Route-parameters route))))
                (API-routes api)))

  ;; Generate methods for sending requests defined in API struct
  (define (api->methods api)
    (map
     (λ (route)
       (define name (string->symbol (Route-path route)))
       (define parameters (Route-parameters route))
       (define fields (map param->field-definition parameters))
       `(define (,name [appid : String] ,@fields)
          (define params
            (list
             (cons 'appid appid)
             ,@(map
                (λ (p)
                  (define writer (schema->writer (Param-schema p)))
                  (define param-name (Param-name p))
                  `(cons (quote ,param-name)
                         (jsexpr->string (,writer ,param-name))))
                parameters)))

          (define query (alist->form-urlencoded params))

          (define-values (status headers in)
            (http-sendrecv
             ,(API-hostname api)
             (string-append ,(API-path-prefix api)
                            ,(Route-path route)
                            "?"
                            query)))

          ;; Status line looks like "#HTTP1.1 200" where 200 is the status code
          (define status-code
            (string->symbol (car (cdr (string-split (bytes->string/utf-8 status))))))

          (define response-expr (cast (read-json in) (HashTable Symbol JSExpr)))

          (case status-code
            ,@(map
               (λ (response)
                 `[(,(Response-code response))
                   (,(format-symbol
                      "read-~a"
                      (Schema-Object-name (Response-schema response)))
                    response-expr)])
               (Route-responses route))
            [else (error (format "Unexpected status code ~a" status-code))])))
     (API-routes api))))

(define-syntax (openapi-type-provider stx)
  (syntax-case stx ()
    [(_ filename)
     (begin
       (define api (file->openapi (syntax-e #'filename)))
       (define defs (api->types api))
       (define methods (api->methods api))
       (define final `(begin ,@defs ,@methods))
       #;(pretty-print final)
       (datum->syntax stx final))]))
