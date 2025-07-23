#!/usr/bin/env guile3
!#

;;; Multi-step LLM Actor Mock
;;; Demonstrates complex control flow with multiple function calls

(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 threads)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-19)
             (ice-9 rdelim))

;; Load base definitions
(add-to-load-path (dirname (current-filename)))

;; Enhanced message type with conversation history
(define-record-type <conversation>
  (make-conversation id messages functions-called)
  conversation?
  (id conversation-id)
  (messages conversation-messages set-conversation-messages!)
  (functions-called conversation-functions set-conversation-functions!))

;; Mock LLM with conversation awareness
(define (mock-llm-with-state)
  "Creates a stateful mock LLM that tracks conversation"
  (let ((conversations (make-hash-table)))
    
    (define (analyze-with-context prompt conv-id)
      "Analyzes prompt with conversation context"
      (let* ((conv (hashq-ref conversations conv-id 
                              (make-conversation conv-id '() '())))
             (history (conversation-messages conv))
             (functions-used (conversation-functions conv)))
        
        ;; Multi-step scenarios
        (cond
         ;; Scenario: Create and read back a file
         ((string-contains-ci prompt "create a file") 
          (if (null? functions-used)
              ;; First step: write file
              `(function-call . ((name . write_file)
                                (args . ("demo.txt" "This is a test file created by the mock LLM"))))
              ;; Second step: read it back
              `(function-call . ((name . read_file)
                                (args . ("demo.txt"))))))
         
         ;; Scenario: Complex calculation with memory
         ((string-contains-ci prompt "calculate and store")
          (cond
           ((null? functions-used)
            ;; First: calculate
            `(function-call . ((name . calculate)
                              (args . ("(* 42 10)")))))
           ((= (length functions-used) 1)
            ;; Second: store result
            `(function-call . ((name . write_file)
                              (args . ("calculation.txt" "The answer is 420")))))
           (else
            ;; Final: confirm
            `(direct-answer . "I've calculated 42 * 10 = 420 and stored it in calculation.txt"))))
         
         ;; Scenario: Time-based file creation
         ((string-contains-ci prompt "log the time")
          (cond
           ((null? functions-used)
            ;; First: get time
            `(function-call . ((name . get_current_time)
                              (args . ()))))
           (else
            ;; Second: write to log
            (let ((timestamp (caar history))) ; Get last result
              `(function-call . ((name . write_file)
                                (args . ("time.log" ,timestamp))))))))
         
         (else
          `(direct-answer . "I've completed the requested task.")))))
    
    (define (update-conversation! conv-id message function-name)
      "Updates conversation state"
      (let ((conv (hashq-ref conversations conv-id 
                            (make-conversation conv-id '() '()))))
        (set-conversation-messages! conv 
          (cons message (conversation-messages conv)))
        (when function-name
          (set-conversation-functions! conv
            (cons function-name (conversation-functions conv))))
        (hashq-set! conversations conv-id conv)))
    
    ;; Return the analyzer function
    (lambda (prompt conv-id)
      (let ((decision (analyze-with-context prompt conv-id)))
        (match decision
          (('function-call . params)
           (update-conversation! conv-id prompt (assq-ref params 'name)))
          (('direct-answer . _)
           (update-conversation! conv-id prompt #f)))
        decision))))

;; Enhanced LLM actor with state management
(define (stateful-llm-actor app-channel llm-channel analyzer)
  "LLM actor that maintains conversation state"
  (define (log-flow step details)
    (format #t "[LLM Actor] ~a: ~a~%" step details))
  
  (let ((conv-counter 0))
    (let loop ()
      (let ((msg (channel-get llm-channel)))
        (when msg
          (match msg
            ((type content from)
             (case type
               ((prompt)
                (set! conv-counter (+ conv-counter 1))
                (let ((conv-id conv-counter))
                  (log-flow "New conversation" (format #f "ID: ~a" conv-id))
                  
                  ;; Process potentially multiple steps
                  (let step-loop ((prompt content)
                                  (step-count 0))
                    (log-flow "Step" (format #f "~a - Analyzing: ~a" step-count prompt))
                    (let ((decision (analyzer prompt conv-id)))
                      (match decision
                        (('function-call . params)
                         (log-flow "Decision" "Function call needed")
                         (channel-put app-channel `(function-request ,params ,conv-id))
                         
                         ;; Wait for result and potentially continue
                         (let ((result-msg (channel-get llm-channel)))
                           (match result-msg
                             ((result-type result _)
                              (when (eq? result-type 'function-result)
                                (log-flow "Result" (format #f "Got: ~a" result))
                                ;; Check if more steps needed
                                (let ((next-decision (analyzer prompt conv-id)))
                                  (match next-decision
                                    (('function-call . _)
                                     ;; Continue with next step
                                     (step-loop prompt (+ step-count 1)))
                                    (('direct-answer . answer)
                                     ;; Send final answer
                                     (log-flow "Complete" "Sending final answer")
                                     (channel-put app-channel 
                                       `(final-answer ,answer ,conv-id))))))))))
                        
                        (('direct-answer . answer)
                         (log-flow "Decision" "Direct answer")
                         (channel-put app-channel `(final-answer ,answer ,conv-id))))))))))
          (loop))))))

;; Test runner
(define (run-multi-step-scenario description prompt)
  "Runs a multi-step scenario"
  (format #t "~%~%=== Multi-Step Scenario: ~a ===~%" description)
  (format #t "Prompt: ~a~%" prompt)
  (format #t "~%")
  
  (let* ((app-channel (make-channel))
         (llm-channel (make-channel))
         (analyzer (mock-llm-with-state))
         (completed #f))
    
    ;; Simple function implementations
    (define functions
      `((calculate . ,(lambda (expr) 
                       (format #f "~a" (eval (with-input-from-string expr read)
                                            (interaction-environment)))))
        (get_current_time . ,(lambda () 
                              (date->string (current-date) "~Y-~m-~d ~H:~M:~S")))
        (write_file . ,(lambda (path content)
                        (format #t "  [Function] Writing '~a' to ~a~%" content path)
                        "Write successful"))
        (read_file . ,(lambda (path)
                       (format #t "  [Function] Reading from ~a~%" path)
                       "This is a test file created by the mock LLM"))))
    
    ;; App actor
    (define app-thread
      (make-thread
        (lambda ()
          ;; Send initial prompt
          (channel-put llm-channel `(prompt ,prompt app))
          
          ;; Process messages
          (let loop ()
            (let ((msg (channel-get app-channel)))
              (when msg
                (match msg
                  (('function-request params conv-id)
                   (let* ((func-name (assq-ref params 'name))
                          (args (assq-ref params 'args))
                          (func (assq-ref functions func-name)))
                     (if func
                         (let ((result (apply func args)))
                           (channel-put llm-channel `(function-result ,result ,conv-id)))
                         (channel-put llm-channel `(function-error "Not found" ,conv-id)))))
                  
                  (('final-answer answer conv-id)
                   (format #t "~%Final Answer: ~a~%" answer)
                   (set! completed #t)))
                
                (when (not completed)
                  (loop))))))))
    
    ;; LLM actor
    (define llm-thread
      (make-thread
        (lambda ()
          (stateful-llm-actor app-channel llm-channel analyzer))))
    
    (thread-start! app-thread)
    (thread-start! llm-thread)
    
    ;; Wait for completion
    (let wait ((count 0))
      (when (and (not completed) (< count 30))
        (usleep 100000) ; 100ms
        (wait (+ count 1))))
    
    (thread-cancel app-thread)
    (thread-cancel llm-thread)))

;; Main
(define (main args)
  (format #t "Multi-Step LLM Actor Mock~%")
  (format #t "=========================~%")
  
  (run-multi-step-scenario 
    "File Creation and Verification"
    "Create a file with some content and then read it back")
  
  (run-multi-step-scenario
    "Complex Calculation with Storage"
    "Calculate and store the result of 42 times 10")
  
  (run-multi-step-scenario
    "Time Logging"
    "Log the current time to a file")
  
  (format #t "~%~%Experiment complete!~%"))

(main (command-line))