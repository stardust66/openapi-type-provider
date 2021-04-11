#lang typed/racket/base

(provide file->openapi
         (struct-out API)
         (struct-out Route))

(require typed/json
         racket/list
         typed/net/url
         "json-schema.rkt"
         "utils.rkt")

(define-type Method (U 'get 'head 'post 'put 'delete))
(define-type JsonHash (HashTable Symbol JSExpr))

(: method? (-> Any Boolean : Method))
(define (method? m)
  (and (member m '(get head post put delete)) #t))

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

(define (make-param [param : JSExpr] [top : JSExpr])
  (define name (assert (json-ref param 'name) string?))
  (define schema (json-ref param 'schema))
  (make-schema-with-name (string->symbol name) schema top))

(define (make-response [code : Symbol] [response : JSExpr] [top : JSExpr])
  (define schema (json-refs response '(content application/json schema)))
  (make-schema-with-name code schema top))

(define (get-routes [paths : JSExpr] [top : JSExpr])
  (: routes (Listof (Listof Route)))
  (define routes
    (for/list ([(path contents) (in-hash (assert paths hash?))])
      (hash-map
       (assert contents hash?)
       (λ ([method : Symbol] [body : JSExpr])
         (with-asserts ([method method?])
           (define parameters
             (map
              (λ ([p : JSExpr]) (make-param p top))
              (assert (json-ref body 'parameters) list?)))
           (define response
             (make-response
              '|200|
              (json-refs body '(responses |200|))
              top))
           (Route (symbol->string path) method parameters response))))))
  (append* routes))

(define (parse-openapi [contents : JSExpr])
  (define name (assert (json-refs contents '(info title)) string?))
  (define routes (get-routes (json-ref contents 'paths) contents))
  (define url
    (string->url
     (assert
      (json-ref
       (car (assert (json-ref contents 'servers) list?))
       'url)
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
   path-prefix))

(define (file->openapi [filename : String])
  (with-input-from-file filename
    (λ ()
      (define contents (cast (read-json) JSExpr))
      (parse-openapi contents))))
