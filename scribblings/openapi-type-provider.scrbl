#lang scribble/manual

@(require (for-label (only-meta-in 0 typed/racket)))

@title{OpenAPI Type Provider}

@defmodule[openapi-type-provider]

OpenAPI Type Provider is a library that generates well-typed OpenAPI client
libraries, inspired by type providers in F#.

@section[#:tag "overview"]{Overview}

@defform[(openapi-type-provider path)
         #:contracts ([path path-string?])]{
  Reads OpenAPI 3.0 schema from the file spcified by @racket[path]. Then,
  expands to the following types and functions.

  For each route defined by the schema, a function with the name
  @racketvarfont{method-path} will be generated. For example, for a GET request
  to "/weather", @racketvarfont{get-/weather} will be generated. The first
  argument of the generated function will be a credentials struct, and the rest
  are parameters of the request.

  For each parameter that is of object type, a @racket[struct] of the same name
  will be defined, using schema information from the OpenAPI specification. For
  example, a parameter

  @verbatim|{
  "Weather": {
    "title": "Weather",
    "type": "object",
    "properties": {
      "id": {
        "type": "integer"
      },
      "main": {
        "type": "string"
      },
      "description": {
        "type": "string"
      },
      "icon": {
        "type": "string"
      }
    }
  }
  }|

  will expand to the following definition

  @racketmod[
  typed/racket
  (struct Weather ([id : Integer]
                   [main : String]
                   [description : String]
                   [icon : String]))
  ]

  In addition, functions of the form @racketvarfont{read-} and
  @racketvarfont{write-} will be generated for each of the structs that read the
  struct from JSON and write instances in JSON. The example above will generate

  @defproc[(read-Weather [in Input-Port])
           Weather]
  @defproc[(write-Weather [weather Weather]
                          [out Output-Port (current-output-port)])
           Void]
}

@section[#:tag "Authentication"]{Authentication}

For authentication, different credential structs are generated based on the
security scheme in the specification.
