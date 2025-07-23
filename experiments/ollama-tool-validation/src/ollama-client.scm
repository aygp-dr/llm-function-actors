#!/usr/bin/env guile3
!#

;;; Ollama client for tool calling
;;; Provides interface to Ollama API with tool support

(define-module (ollama-client)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (json)
  #:export (make-ollama-client
            ollama-chat
            ollama-generate
            register-tool!))

(define (make-ollama-client #:key (base-url "http://localhost:11434"))
  "Create an Ollama client with specified base URL"
  (let ((tools (make-hash-table)))
    
    (define (register-tool! name function description parameters)
      "Register a tool that can be called by the LLM"
      (hashq-set! tools name
                  `((function . ,function)
                    (description . ,description)
                    (parameters . ,parameters))))
    
    (define (format-tools)
      "Format registered tools for Ollama API"
      (hash-map->list
       (lambda (name tool)
         `((type . "function")
           (function . ((name . ,(symbol->string name))
                       (description . ,(assq-ref tool 'description))
                       (parameters . ,(assq-ref tool 'parameters))))))
       tools))
    
    (define (call-api endpoint data)
      "Make API call to Ollama"
      (let* ((uri (string->uri (string-append base-url endpoint)))
             (response (http-post uri
                                 #:body (scm->json-string data)
                                 #:headers '((content-type . "application/json"))))
             (body (get-string-all (cadr response))))
        (json-string->scm body)))
    
    (define (chat model messages #:key (temperature 0.7))
      "Chat completion with tool support"
      (let ((request `((model . ,model)
                      (messages . ,messages)
                      (tools . ,(format-tools))
                      (temperature . ,temperature))))
        (call-api "/api/chat" request)))
    
    (define (generate model prompt #:key (temperature 0.7))
      "Simple text generation"
      (let ((request `((model . ,model)
                      (prompt . ,prompt)
                      (temperature . ,temperature))))
        (call-api "/api/generate" request)))
    
    ;; Return client interface
    (lambda (method . args)
      (case method
        ((register-tool!) (apply register-tool! args))
        ((chat) (apply chat args))
        ((generate) (apply generate args))
        ((get-tools) (format-tools))))))

;; Convenience procedures
(define (ollama-chat client model messages . args)
  (apply (client 'chat) model messages args))

(define (ollama-generate client model prompt . args)
  (apply (client 'generate) model prompt args))

(define (register-tool! client . args)
  (apply (client 'register-tool!) args))
