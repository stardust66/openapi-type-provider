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
   [ssl? : Boolean]
   [path-prefix : String])
  #:transparent)

(struct Route
  ([path : String]
   [method : Method]
   [security-requirement : (Option Security-Scheme)]
   [body : (Listof Property)]
   [parameters : (Listof Param)]
   [responses : (Listof Response)])
  #:transparent)

(define-type Param-In (U 'query 'path 'header 'cookie))
(struct Param
  ([name : Symbol]
   [schema : Schema]
   [in : Param-In])
  #:transparent)

(struct Response
  ([schema : Schema-Object]
   [code : Symbol])
  #:transparent)

(define-type Security-Type (U 'apiKey 'http 'oauth2 'openIdConnect))
(define-type Security-In (U 'query 'header 'cookie))
(struct Security-Scheme
  ([type : Security-Type]
   [in : Security-In])
  #:transparent)

;; TODO: Write macro for the following functions
(define (string->Param-In [in : String]) : Param-In
  (case in
    [("path") 'path]
    [("header") 'header]
    [("query") 'query]
    [("cookie") 'cookie]
    [else (error (format "Expected one of 'query', 'path', 'header', 'cookie. Given ~a"
                         in))]))

(define (string->Security-Type [in : String]) : Security-Type
  (case in
    [("apiKey") 'apiKey]
    [("http") 'http]
    [("oauth2") 'oauth2]
    [("openIdConnect") 'openIdConnect]
    [else (error (format "Expected one of 'apiKey', 'http', 'oauth2', 'openIdConnect'. Given ~a"
                         in))]))

(define (string->Security-In [in : String]) : Security-In
  (case in
    [("query") 'query]
    [("header") 'header]
    [("cookie") 'cookie]
    [else (error (format "Expected one of 'query', 'header', 'cookie'. Given ~a"
                         in))]))

(define (make-param [param : JSExpr] [top : JSExpr]) : Param
  (cond
    [(json-has-key? param '$ref)
     (make-param (trace-ref (assert (json-ref param '$ref) string?) top) top)]
    [else
     (define name (string->symbol (assert (json-ref param 'name) string?)))
     (define schema-json (json-ref param 'schema))
     (define schema (make-schema-with-name name schema-json top))
     (define param-in (string->Param-In (assert (json-ref param 'in) string?)))
     (Param name schema param-in)]))

(define schema-keys '(content application/json schema))

(define (make-request-body [request : JSExpr]
                           [top : JSExpr]) : (Listof Property)
  (cond
    [(json-has-keys? request schema-keys)
     (make-properties (json-ref (json-refs request schema-keys) 'properties) top)]
    [else '()]))

(define (make-response [code : Symbol]
                       [method : Method]
                       [path : Symbol]
                       [response : JSExpr]
                       [top : JSExpr]) : Response
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

       (define request-body
         (make-request-body (json-ref body 'requestBody (hash)) top))

       (Route
        (symbol->string path)
        method
        #f  ; Security not implemented
        request-body
        parameters
        responses)))))

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
  (define ssl? (string=? (assert (url-scheme url) string?) "https"))
  (API
   name
   routes
   (assert (url-host url) string?)
   ssl?
   path-prefix))

(define (file->openapi [filename : String])
  (with-input-from-file filename
    (λ ()
      (define contents (cast (read-json) JSExpr))
      (parse-openapi contents))))
