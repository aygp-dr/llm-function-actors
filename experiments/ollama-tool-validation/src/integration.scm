#!/usr/bin/env guile3
!#

;;; Integration of Ollama client with file tools
;;; Main entry point for experiments

(add-to-load-path (dirname (current-filename)))

(use-modules (ollama-client)
             (file-tools)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             (srfi srfi-19))

(define (log-event event-type message)
  "Log events with timestamps for sequence diagram validation"
  (format #t "[~a] ~a: ~a~%"
          (date->string (current-date) "~H:~M:~S")
          event-type
          message))

(define (process-tool-calls client response)
  "Process tool calls from LLM response"
  (let ((message (assq-ref response 'message)))
    (when message
      (let ((tool-calls (assq-ref message 'tool_calls)))
        (when tool-calls
          (map (lambda (tool-call)
                 (let* ((function (assq-ref tool-call 'function))
                        (name (string->symbol (assq-ref function 'name)))
                        (args (assq-ref function 'arguments)))
                   (log-event 'TOOL-CALL (format #f "Calling ~a with ~a" name args))
                   ;; Execute the tool
                   (let ((result (apply (client 'get-tool name) 
                                       (map cdr (json-string->scm args)))))
                     (log-event 'TOOL-RESULT (format #f "~a returned: ~a" name result))
                     result)))
               tool-calls))))))

(define (run-conversation client model prompt)
  "Run a complete conversation with tool calling"
  (log-event 'START (format #f "Model: ~a, Prompt: ~a" model prompt))
  
  (let* ((messages `(((role . "user") (content . ,prompt))))
         (response (ollama-chat client model messages)))
    
    (log-event 'LLM-RESPONSE "Initial response received")
    (pretty-print response)
    
    ;; Process any tool calls
    (let ((tool-results (process-tool-calls client response)))
      (when tool-results
        ;; Send tool results back to LLM
        (let* ((updated-messages 
                (append messages 
                        (list (assq-ref response 'message))
                        (map (lambda (result)
                               `((role . "tool")
                                 (content . ,result)))
                             tool-results)))
               (final-response (ollama-chat client model updated-messages)))
          
          (log-event 'FINAL-RESPONSE "Final response after tool execution")
          (pretty-print final-response))))
    
    (log-event 'END "Conversation complete")))

(define (main args)
  (format #t "Ollama Tool Calling Validation~%")
  (format #t "==============================~%~%")
  
  ;; Create client and register tools
  (let ((client (make-ollama-client)))
    (register-file-tools! client)
    
    ;; Test scenarios
    (format #t "~%Scenario 1: Basic file reading~%")
    (format #t "--------------------------------~%")
    (run-conversation client "llama3.2:3b" 
                     "Can you read the file README.md and summarize it?")
    
    (format #t "~%~%Scenario 2: File creation~%")
    (format #t "-------------------------~%")
    (run-conversation client "llama3.2:3b"
                     "Create a file called test.txt with the content 'Hello from Ollama!'")
    
    (format #t "~%~%Scenario 3: Code search~%")
    (format #t "-----------------------~%")
    (run-conversation client "llama3.2:3b"
                     "Search for functions that contain 'tool' in the src directory")))

(main (command-line))
