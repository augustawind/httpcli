# TODO

## Greater Tasks

## Lesser Tasks

- use InsOrdHashMap to preserve ordering of api/env
- support other body types (currently only JSON is supported)

## Refactoring

- use Char8 ByteStrings uniformly everywhere (replace reg ByteStrings)

##

- API docs & Env docs
    - [x] YAML (de)serialization
    - [x] Template rendering
    - [ ] post-request scripting (embedded language?)
- Requests
    - [x] Massage Request into data suitable for Req library
    - [ ] Execute post-request scripts on local Env.
- Command-line interface
    - Commands:
        run: execute request from API doc
        view: print API group/request or Env
        env: view or update Env
    - Options:
        --api: API yaml file
        --env: Env yaml file
        --save: persist variables to env
    - Modifiers: DSL for just-in-time modification of API requests
        - Actions:
            assign: Set a request field value
            update: Update an existing request field value
            delete: Delete a request field value
        - Targets: method, url, query, headers, json body
