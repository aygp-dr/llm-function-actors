#!/usr/bin/env guile3
!#

;;; LLM Actor Mock - Demonstrates control flow from sequence diagram
;;; This mock simulates an LLM that can:
;;; 1. Analyze user prompts
;;; 2. Decide whether to call functions
;;; 3. Chain multiple function calls
;;; 4. Generate final answers

(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 threads)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-19))

;; Channel implementation (simple thread-safe queue)
(define (make-channel)
  (let ((queue '())
        (mutex (make-mutex))
        (condvar (make-condition-variable)))
    (lambda (op . args)
      (case op
        ((put)
         (with-mutex mutex
           (set! queue (append queue (list (car args))))
           (signal-condition-variable condvar)))
        ((get)
         (with-mutex mutex
           (while (null? queue)
             (wait-condition-variable condvar mutex))
           (let ((item (car queue)))
             (set! queue (cdr queue))
             item)))))))

(define (channel-put channel item)
  (channel 'put item))

(define (channel-get channel)
  (channel 'get))

;; Message types matching the sequence diagram
(define-record-type <message>
  (make-message type content from to timestamp)
  message?
  (type message-type)
  (content message-content)
  (from message-from)
  (to message-to)
  (timestamp message-timestamp))

;; Function registry
(define function-registry (make-hash-table))

(define (register-function! name func description)
  (hashq-set! function-registry name 
              `((function . ,func)
                (description . ,description))))

;; Register mock functions
(register-function! 'calculate
  (lambda (expression)
    (format #f "Result: ~a" (eval (with-input-from-string expression read)
                                   (interaction-environment))))
  "Evaluates mathematical expressions")

(register-function! 'get_current_time
  (lambda ()
    (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
  "Returns the current date and time")

(register-function! 'read_file
  (lambda (path)
    (if (file-exists? path)
        (call-with-input-file path get-string-all)
        (format #f "Error: File ~a not found" path)))
  "Reads content from a file")

(register-function! 'write_file
  (lambda (path content)
    (call-with-output-file path
      (lambda (port)
        (display content port)))
    (format #f "Successfully wrote to ~a" path))
  "Writes content to a file")

;; Mock LLM decision engine
(define (analyze-prompt prompt)
  "Analyzes prompt and returns decision: 'function-call or 'direct-answer"
  (cond
   ((string-contains-ci prompt "calculate")
    `(function-call . ((name . calculate)
                      (args . ("(+ 2 2)")))))
   ((string-contains-ci prompt "what time")
    `(function-call . ((name . get_current_time)
                      (args . ()))))
   ((string-contains-ci prompt "read")
    `(function-call . ((name . read_file)
                      (args . ("test.txt")))))
   ((string-contains-ci prompt "write")
    `(function-call . ((name . write_file)
                      (args . ("test.txt" "Hello from mock LLM!")))))
   (else
    `(direct-answer . "I understand your question. This is a mock response."))))

;; LLM Actor implementation
(define (llm-actor app-channel llm-channel)
  "Mock LLM actor that processes messages according to sequence diagram flow"
  (define (log-flow step)
    (format #t "[LLM Actor] Flow: ~a at ~a~%" step 
            (date->string (current-date) "~H:~M:~S")))
  
  (let loop ()
    (log-flow "Waiting for prompt")
    (let ((msg (channel-get llm-channel)))
      (when msg
        (log-flow "Prompt received")
        (match msg
          (($ <message> type content from to timestamp)
           (case type
             ((prompt)
              (log-flow "Analyzing intent")
              (let ((decision (analyze-prompt content)))
                (match decision
                  (('function-call . params)
                   (log-flow "Function call detected")
                   ;; Send function request to app actor
                   (channel-put app-channel
                     (make-message 'function-request params 'llm 'app (current-time)))
                   
                   ;; Wait for function result
                   (log-flow "Waiting for function result")
                   (let ((result-msg (channel-get llm-channel)))
                     (match result-msg
                       (($ <message> 'function-result result _ _ _)
                        (log-flow "Function result received")
                        ;; Generate final answer incorporating result
                        (let ((final-answer 
                               (format #f "Based on the function result: ~a" result)))
                          (log-flow "Sending final answer")
                          (channel-put app-channel
                            (make-message 'final-answer final-answer 'llm 'app (current-time))))))))
                  
                  (('direct-answer . answer)
                   (log-flow "Direct answer - no function needed")
                   (channel-put app-channel
                     (make-message 'final-answer answer 'llm 'app (current-time)))))))
             
             (else
              (format #t "[LLM Actor] Unknown message type: ~a~%" type)))))
        (loop)))))

;; Application Actor implementation
(define (app-actor app-channel llm-channel)
  "Application actor that coordinates with LLM actor"
  (define (log-flow step)
    (format #t "[App Actor] Flow: ~a at ~a~%" step 
            (date->string (current-date) "~H:~M:~S")))
  
  ;; Start by sending a prompt
  (log-flow "Sending initial prompt")
  (channel-put llm-channel
    (make-message 'prompt "Can you calculate 2 + 2?" 'app 'llm (current-time)))
  
  ;; Process responses
  (let loop ()
    (let ((msg (channel-get app-channel)))
      (when msg
        (match msg
          (($ <message> type content from to timestamp)
           (case type
             ((function-request)
              (log-flow "Function request received")
              (let* ((func-name (assq-ref content 'name))
                     (args (assq-ref content 'args))
                     (func-entry (hashq-ref function-registry func-name)))
                
                (if func-entry
                    (let* ((func (assq-ref func-entry 'function))
                           (result (apply func args)))
                      (log-flow (format #f "Executed function '~a' with result" func-name))
                      ;; Send result back to LLM
                      (channel-put llm-channel
                        (make-message 'function-result result 'app 'llm (current-time))))
                    (begin
                      (log-flow (format #f "Function '~a' not found!" func-name))
                      (channel-put llm-channel
                        (make-message 'function-error 
                                      (format #f "Function ~a not found" func-name)
                                      'app 'llm (current-time)))))))
             
             ((final-answer)
              (log-flow "Final answer received")
              (format #t "~%=== Final Answer ===~%~a~%==================~%~%" content))
             
             (else
              (format #t "[App Actor] Unknown message type: ~a~%" type)))))
        (loop)))))

;; Helper for string search
(define (string-contains-ci str search)
  "Case-insensitive string contains"
  (let ((str-lower (string-downcase str))
        (search-lower (string-downcase search)))
    (string-contains str-lower search-lower)))

;; Demo scenarios
(define (run-scenario prompt)
  "Runs a complete scenario with the given prompt"
  (format #t "~%=== Scenario: ~a ===~%" prompt)
  
  (let* ((app-channel (make-channel))
         (llm-channel (make-channel)))
    
    ;; Override the initial prompt in app-actor
    (set! app-actor
      (lambda (app-ch llm-ch)
        (define (log-flow step)
          (format #t "[App Actor] Flow: ~a at ~a~%" step 
                  (date->string (current-date) "~H:~M:~S")))
        
        (log-flow "Sending prompt")
        (channel-put llm-ch
          (make-message 'prompt prompt 'app 'llm (current-time)))
        
        (let loop ((timeout 5))
          (if (> timeout 0)
              (let ((msg (channel-get app-ch)))
                (when msg
                  (match msg
                    (($ <message> type content from to timestamp)
                     (case type
                       ((function-request)
                        (log-flow "Function request received")
                        (let* ((func-name (assq-ref content 'name))
                               (args (assq-ref content 'args))
                               (func-entry (hashq-ref function-registry func-name)))
                          
                          (if func-entry
                              (let* ((func (assq-ref func-entry 'function))
                                     (result (apply func args)))
                                (log-flow (format #f "Executed '~a'" func-name))
                                (channel-put llm-ch
                                  (make-message 'function-result result 'app 'llm (current-time))))
                              (channel-put llm-ch
                                (make-message 'function-error 
                                              (format #f "Function ~a not found" func-name)
                                              'app 'llm (current-time))))))
                       
                       ((final-answer)
                        (log-flow "Final answer received")
                        (format #t "~%=== Final Answer ===~%~a~%==================~%~%" content)
                        (set! timeout 0))
                       
                       (else
                        (format #t "[App Actor] Unknown message type: ~a~%" type)))))
                  (when (> timeout 0)
                    (loop (- timeout 1))))))))) ; Added missing parens for lambda and set!
    
    ;; Start threads
    (call-with-new-thread
      (lambda () (app-actor app-channel llm-channel)))
    (call-with-new-thread
      (lambda () (llm-actor app-channel llm-channel)))
    
    ;; Wait for completion
    (usleep 1000000))) ; 1 second

;; Main execution
(define (main args)
  (format #t "LLM Actor Mock Experiment~%")
  (format #t "========================~%")
  
  ;; Run different scenarios
  (run-scenario "Can you calculate 2 + 2?")
  (run-scenario "What time is it?")
  (run-scenario "Please read the file test.txt")
  (run-scenario "What is the meaning of life?")
  
  (format #t "~%Experiment complete!~%"))

(main (command-line))