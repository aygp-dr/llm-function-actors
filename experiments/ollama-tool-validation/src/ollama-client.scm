#!/usr/bin/env guile3
!#

;;; Ollama client for tool calling with comprehensive logging
;;; Provides interface to Ollama API with tool support

(define-module (ollama-client)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (srfi srfi-19)
  #:export (make-ollama-client
            ollama-chat
            ollama-generate
            register-tool!
            set-log-level!))

;; Logging levels
(define *log-level* 'info) ; 'debug 'info 'warn 'error

(define (set-log-level! level)
  (set! *log-level* level))

(define (log-message level tag message)
  "Enhanced logging with levels and tags"
  (when (or (eq? level 'error)
            (and (eq? level 'warn) (memq *log-level* '(debug info warn)))
            (and (eq? level 'info) (memq *log-level* '(debug info)))
            (eq? *log-level* 'debug))
    (format #t "~a [OLLAMA-TOOLS] [~a] ~a: ~a~%"
            (date->string (current-date) "~Y-~m-~d ~H:~M:~S")
            (string-upcase (symbol->string level))
            tag
            message)))

(define (make-ollama-client #:key (base-url "http://localhost:11434"))
  "Create an Ollama client with specified base URL"
  (let ((tools (make-hash-table)))
    
    (log-message 'info 'SETUP (format #f "Initialized client with base URL: ~a" base-url))
    
    (define (register-tool! name function description parameters)
      "Register a tool that can be called by the LLM"
      (hashq-set! tools name
                  `((function . ,function)
                    (description . ,description)
                    (parameters . ,parameters)))
      (log-message 'debug 'SETUP (format #f "Registered tool: ~a" name)))
    
    (define (format-tools)
      "Format registered tools for Ollama API"
      (let ((tool-list (hash-map->list
                        (lambda (name tool)
                          `((type . "function")
                            (function . ((name . ,(symbol->string name))
                                        (description . ,(assq-ref tool 'description))
                                        (parameters . ,(assq-ref tool 'parameters))))))
                        tools)))
        (log-message 'debug 'SETUP 
                     (format #f "Formatted ~a tools for API" (length tool-list)))
        tool-list))
    
    (define (call-api endpoint data)
      "Make API call to Ollama with logging"
      (log-message 'debug 'API-CALL 
                   (format #f "Calling ~a with ~a bytes of data" 
                           endpoint (string-length (scm->json-string data))))
      (let* ((uri (string->uri (string-append base-url endpoint)))
             (response (http-post uri
                                 #:body (scm->json-string data)
                                 #:headers '((content-type . "application/json"))))
             (body (get-string-all (cadr response))))
        (log-message 'debug 'API-RESPONSE 
                     (format #f "Received ~a bytes response" (string-length body)))
        (json-string->scm body)))
    
    (define (chat model messages #:key (temperature 0.7))
      "Chat completion with tool support"
      (log-message 'info 'LLM-REQUEST "Sending prompt with available tools to LLM")
      (let ((request `((model . ,model)
                      (messages . ,messages)
                      (tools . ,(format-tools))
                      (temperature . ,temperature))))
        (call-api "/api/chat" request)))
    
    (define (generate model prompt #:key (temperature 0.7))
      "Simple text generation"
      (log-message 'info 'LLM-REQUEST "Sending generation request")
      (let ((request `((model . ,model)
                      (prompt . ,prompt)
                      (temperature . ,temperature))))
        (call-api "/api/generate" request)))
    
    (define (get-tool name)
      "Get a tool function by name"
      (let ((tool (hashq-ref tools name)))
        (if tool
            (begin
              (log-message 'info 'EXECUTE 
                           (format #f "Found function ~a, executing..." name))
              (assq-ref tool 'function))
            (begin
              (log-message 'error 'EXECUTE 
                           (format #f "Tool ~a not found!" name))
              #f))))
    
    ;; Return client interface
    (lambda (method . args)
      (case method
        ((register-tool!) (apply register-tool! args))
        ((chat) (apply chat args))
        ((generate) (apply generate args))
        ((get-tools) (format-tools))
        ((get-tool) (apply get-tool args))))))

;; Convenience procedures
(define (ollama-chat client model messages . args)
  (apply (client 'chat) model messages args))

(define (ollama-generate client model prompt . args)
  (apply (client 'generate) model prompt args))

(define (register-tool! client . args)
  (apply (client 'register-tool!) args))
