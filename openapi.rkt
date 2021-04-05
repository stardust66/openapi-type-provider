#lang typed/racket/base

(provide file->openapi
         (struct-out API)
         (struct-out Route))

(require typed/json
         racket/list
         typed/net/url
         "json-schema.rkt")

(define-type Method (U 'get 'post))
(define-type JsonHash (HashTable Symbol JSExpr))

(: method? (-> Any Boolean : Method))
(define (method? m)
  (or (eq? m 'get) (eq? m 'post)))

(struct API
  ([name : String]
   [routes : (Listof Route)]
   [hostname : String]
   [path-prefix : String]) #:transparent)
(struct Route
  ([path : String]
   [method : Method]
   [parameters : (Listof Schema)]
   [response : Schema])
  #:transparent)

#;(define-type ParamIn (U 'query 'path 'header 'cookie))
#;
(struct Param
  ([schema : Schema]
   [in : ParamIn])
  #:transparent)

(define (make-param [param : JsonHash])
  (define name (assert (hash-ref param 'name) string?))
  (define schema (hash-ref param 'schema))
  (make-schema-with-name (string->symbol name) schema))

(define (make-response [code : Symbol] [response : JsonHash])
  (define schema
    (hash-ref
     (assert
      (hash-ref
       (assert
        (hash-ref response 'content)
        hash?)
       'application/json)
      hash?)
     'schema))
  (make-schema-with-name code schema))

(define (get-routes [paths : JsonHash])
  (: routes (Listof (Listof Route)))
  (define routes
    (for/list ([(path contents) (in-hash paths)])
      (hash-map
       (assert contents hash?)
       (λ (method body)
         (with-asserts ([method method?] [body hash?])
           (define parameters
             (map
              make-param
              (cast (hash-ref body 'parameters) (Listof JsonHash))))
           (define response
             (make-response
              '|200|
              (cast
               (hash-ref
                (cast (hash-ref body 'responses) JsonHash)
                '|200|)
               JsonHash)))
           (Route (symbol->string path) method parameters response))))))
  (append* routes))

(define (parse-openapi [contents : JSExpr])
  (with-asserts ([contents hash?])
    (define paths (assert (hash-ref contents 'paths) hash?))
    (define info (assert (hash-ref contents 'info) hash?))
    (define name (assert (hash-ref info 'title) string?))
    (define routes (get-routes (cast paths JsonHash)))
    (define url
      (string->url
       (assert
        (hash-ref
         (assert
          (car (assert (hash-ref contents 'servers) list?))
          hash?) 'url)
        string?)))
    (define path-prefix
      (foldl
       (λ ([e : path/param] [acc : String])
         (string-append acc "/" (assert (path/param-path e) string?)))
       ""
       (url-path url)))
    (API
     name
     routes
     (assert (url-host url) string?)
     path-prefix)))

(define (file->openapi [filename : String])
  (with-input-from-file filename
    (λ ()
      (define contents (cast (read-json) JSExpr))
      (parse-openapi contents))))
