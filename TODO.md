# TODO

- [ ] support other body types (currently only JSON is supported)
- [ ] add test suite

## Improvements

- [ ] colored output
- [ ] Add flexibility to API and Env docs:
    - API request groups to accept:
        - [x] a list of singleton objects (ordered)
        - [ ] an object (unordered)
    - Env to accept:
        - [x] a list of singleton objects (ordered)
        - [ ] an object (unordered)
    - `headers` to accept:
        - [ ] a list of singleton objects (ordered)
        - [ ] an object (unordered)
        - [x] a string (http syntax)
    - `query` to accept:
        - [x] a list of singleton objects (ordered)
        - [ ] an object (unordered)
        - [ ] a string (http syntax)

## Scripting engine

- [ ] group-level scripts (run for each request in the group)
- [ ] add support for pre-request scripts
- [ ] allow importing lua libraries
- [ ] provide small contrib library with various utilities:
    - [ ] functions for common patterns (e.g. checking status codes, setting env variables)
    - [ ] assertions & benchmarking tools

## Refactoring


## Overview

- API docs & Env docs
    - [x] YAML (de)serialization
    - [x] Template rendering
    - [x] post-request scripting (embedded language?)
- Requests
    - [x] Massage HttpRequest into data suitable for Req library
    - [x] Execute post-request scripts on local Env.
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
