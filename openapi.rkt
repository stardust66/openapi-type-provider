#lang typed/racket/base

(provide file->openapi
         (struct-out API)
         (struct-out Route)
         (struct-out Response)
         (struct-out Param))

(require typed/json
         racket/list
         typed/net/url
         "json-schema.rkt"
         "utils.rkt")

(define-type Method (U 'get 'head 'post 'put 'delete))

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
   [parameters : (Listof Param)]
   [responses : (Listof Response)])
  #:transparent)

(define-type ParamIn (U 'query 'path 'header 'cookie))

(struct Param
  ([name : Symbol]
   [schema : Schema]
   [in : ParamIn])
  #:transparent)

(struct Response
  ([schema : Schema-Object]
   [code : Symbol]))

(define (string->ParamIn [in : String]) : ParamIn
  (case in
    [("path") 'path]
    [("header") 'header]
    [("query") 'query]
    [("cookie") 'cookie]
    [else (error (format "Expected one of 'query', 'path', 'header', 'cookie. Given ~a"
                         in))]))

(define (make-param [param : JSExpr] [top : JSExpr]) : Param
  (cond
    [(json-has-key? param '$ref)
     (make-param (trace-ref (assert (json-ref param '$ref) string?) top) top)]
    [else
     (define name (string->symbol (assert (json-ref param 'name) string?)))
     (define schema-json (json-ref param 'schema))
     (define schema (make-schema-with-name name schema-json top))
     (define param-in (string->ParamIn (assert (json-ref param 'in) string?)))
     (Param name schema param-in)]))

(define (make-response [code : Symbol]
                       [method : Method]
                       [path : Symbol]
                       [response : JSExpr]
                       [top : JSExpr]) : Response
  (define schema-keys '(content application/json schema))
  (define schema-name (string->symbol (format "~a-~a-~a" method path code)))
  (define schema
    (if (json-has-keys? response schema-keys)
        (make-schema-with-name schema-name (json-refs response schema-keys) top)
        (Schema-Object schema-name '())))
  (Response (assert schema Schema-Object?) code))

(define (make-routes-for-path [path : Symbol]
                              [contents : JSExpr]
                              [top : JSExpr]) : (Listof Route)
  (hash-map
   (assert contents hash?)
   (λ ([method : Symbol] [body : JSExpr])
     (with-asserts ([method method?])
       (define parameters
         (map
          (λ ([p : JSExpr]) (make-param p top))
          (assert (json-ref body 'parameters '()) list?)))

       (define responses : (Listof Response)
         (for/list ([(code response-body)
                     (in-hash (assert (json-ref body 'responses) hash?))])
           (make-response code method path response-body top)))
       (Route (symbol->string path) method parameters responses)))))

(define (make-routes [paths : JSExpr] [top : JSExpr])
  (define routes : (Listof (Listof Route))
    (for/list ([(path contents) (in-hash (assert paths hash?))])
      (make-routes-for-path path contents top)))
  (append* routes))

(define (parse-openapi [contents : JSExpr])
  (define name (assert (json-refs contents '(info title)) string?))
  (define routes (make-routes (json-ref contents 'paths) contents))
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
