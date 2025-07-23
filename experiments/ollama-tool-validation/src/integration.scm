#!/usr/bin/env guile3
!#

;;; Integration of Ollama client with file tools
;;; Main entry point for experiments with comprehensive flow tracing

(add-to-load-path (dirname (current-filename)))

(use-modules (ollama-client)
             (file-tools)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             (srfi srfi-19))

;; Set debug logging for detailed trace
(set-log-level! 'debug)

(define (print-banner)
  "Print visual banner"
  (format #t "~%╔════════════════════════════════════════════╗~%")
  (format #t "║        FLOW TRACE VISUALIZATION            ║~%")
  (format #t "╚════════════════════════════════════════════╝~%~%"))

(define (log-event tag message)
  "Log events with timestamps for sequence diagram validation"
  (format #t "~a [OLLAMA-TOOLS] [INFO] ~a: ~a~%"
          (date->string (current-date) "~Y-~m-~d ~H:~M:~S")
          tag
          message))

(define (process-tool-calls client response)
  "Process tool calls from LLM response with detailed logging"
  (let ((message (assq-ref response 'message)))
    (when message
      (let ((tool-calls (assq-ref message 'tool_calls)))
        (when tool-calls
          (log-event 'DECISION 
                     (format #f "LLM requested ~a tool call(s)" (length tool-calls)))
          (log-event 'TOOL-LOOP 
                     (format #f "Processing ~a tool call(s)" (length tool-calls)))
          
          (let loop ((calls tool-calls)
                     (index 1)
                     (results '()))
            (if (null? calls)
                (begin
                  (log-event 'TOOL-LOOP "All tools executed, sending results back to LLM")
                  (reverse results))
                (let* ((tool-call (car calls))
                       (function (assq-ref tool-call 'function))
                       (name (string->symbol (assq-ref function 'name)))
                       (args (assq-ref function 'arguments)))
                  
                  (log-event 'TOOL-LOOP 
                             (format #f "Processing tool call ~a/~a" index (length tool-calls)))
                  (log-event 'EXECUTE 
                             (format #f "Looking up function: ~a" name))
                  (format #t "~a [OLLAMA-TOOLS] [DEBUG] EXECUTE: Arguments: ~a~%"
                          (date->string (current-date) "~Y-~m-~d ~H:~M:~S")
                          args)
                  
                  ;; Execute the tool
                  (let* ((tool-fn (client 'get-tool name))
                         (parsed-args (json-string->scm args))
                         (result (if tool-fn
                                    (apply tool-fn (map cdr parsed-args))
                                    `((success . #f) 
                                      (error . ,(format #f "Tool ~a not found" name))))))
                    
                    (log-event 'EXECUTE 
                               (format #f "Function ~a returned: ~a" name result))
                    
                    (loop (cdr calls)
                          (+ index 1)
                          (cons result results)))))))))))

(define (run-conversation client model prompt)
  "Run a complete conversation with tool calling and visual flow"
  (log-event 'INTERACTION "===== NEW CHAT SESSION =====")
  (log-event 'USER (format #f "Prompt: ~a" prompt))
  (log-event 'SETUP (format #f "Initialized client with model: ~a" model))
  
  (let* ((tools (client 'get-tools)))
    (log-event 'SETUP (format #f "Registered ~a tools" (length tools)))
    (format #t "~a [OLLAMA-TOOLS] [DEBUG] SETUP: Tools: ~a~%"
            (date->string (current-date) "~Y-~m-~d ~H:~M:~S")
            (map (lambda (t) (string->symbol (assq-ref (assq-ref t 'function) 'name)))
                 tools)))
  
  (let* ((messages `(((role . "user") (content . ,prompt))))
         (response (ollama-chat client model messages)))
    
    (log-event 'LLM-RESPONSE "Initial response received")
    
    ;; Process any tool calls
    (let ((tool-results (process-tool-calls client response)))
      (if tool-results
          ;; Send tool results back to LLM
          (let* ((updated-messages 
                  (append messages 
                          (list (assq-ref response 'message))
                          (map (lambda (result)
                                 `((role . "tool")
                                   (content . ,(scm->json-string result))))
                               tool-results)))
                 (final-response (begin
                                  (log-event 'LLM-REQUEST 
                                             "Requesting final answer with tool results")
                                  (ollama-chat client model updated-messages))))
            
            (log-event 'RESPONSE 
                       (format #f "Final answer: ~a" 
                               (let ((msg (assq-ref final-response 'message)))
                                 (if msg
                                     (let ((content (assq-ref msg 'content)))
                                       (if (> (string-length content) 60)
                                           (string-append (substring content 0 60) "...")
                                           content))
                                     "No response")))))
          
          ;; No tool calls, direct answer
          (log-event 'RESPONSE "Direct answer provided (no tools needed)")))
    
    (log-event 'INTERACTION "===== SESSION COMPLETE =====")))

(define (print-flow-summary)
  "Print a flow summary"
  (format #t "~%Flow Summary:~%")
  (format #t "1. USER → APP: Send prompt~%")
  (format #t "2. APP → REGISTRY: Register 4 file tools~%")
  (format #t "3. APP → LLM: Send prompt + tool definitions~%")
  (format #t "4. LLM → APP: Request tool call (read_file)~%")
  (format #t "5. APP → REGISTRY: Lookup read_file function~%")
  (format #t "6. APP → APP: Execute read_file('example.txt')~%")
  (format #t "7. APP → LLM: Return file contents~%")
  (format #t "8. LLM → APP: Final summarized answer~%")
  (format #t "9. APP → USER: Display response~%"))

(define (main args)
  (print-banner)
  
  ;; Create client and register tools
  (let ((client (make-ollama-client)))
    (register-file-tools! client)
    
    ;; Test scenarios
    (format #t "~%Scenario 1: Basic file reading~%")
    (format #t "--------------------------------~%")
    (run-conversation client "llama3.2:3b" 
                     "Can you read the file README.org and summarize it?")
    
    (format #t "~%~%Scenario 2: File creation~%")
    (format #t "-------------------------~%")
    (run-conversation client "llama3.2:3b"
                     "Create a file called test.txt with the content 'Hello from Ollama!'")
    
    (format #t "~%~%Scenario 3: Code search~%")
    (format #t "-----------------------~%")
    (run-conversation client "llama3.2:3b"
                     "Search for functions that contain 'tool' in the src directory")
    
    (print-flow-summary)))

(main (command-line))
