#!/usr/bin/env guile3
!#

;;; Flow Visualizer - Generates Mermaid diagrams from actual execution

(use-modules (ice-9 format)
             (ice-9 match)
             (srfi srfi-19))

;; Flow event recording
(define flow-events '())

(define (record-flow-event! actor action details)
  "Records a flow event for visualization"
  (set! flow-events 
    (cons (list (current-time) actor action details) flow-events)))

(define (clear-flow-events!)
  "Clears recorded events"
  (set! flow-events '()))

(define (generate-mermaid-sequence)
  "Generates a Mermaid sequence diagram from recorded events"
  (let ((events (reverse flow-events)))
    (format #t "```mermaid~%")
    (format #t "sequenceDiagram~%")
    (format #t "    participant User~%")
    (format #t "    participant App as Application Actor~%") 
    (format #t "    participant LLM as LLM Actor~%")
    (format #t "    participant Func as Function Registry~%~%")
    
    (for-each
      (lambda (event)
        (match event
          ((time actor action details)
           (case action
             ((send-prompt)
              (format #t "    User->>App: ~a~%" details))
             ((forward-prompt)
              (format #t "    App->>LLM: Forward prompt~%"))
             ((analyze-intent)
              (format #t "    Note over LLM: Analyze intent<br/>Decision: ~a~%" details))
             ((request-function)
              (format #t "    LLM->>App: Request function: ~a~%" details))
             ((execute-function)
              (format #t "    App->>Func: Execute ~a~%" details))
             ((function-result)
              (format #t "    Func-->>App: Result: ~a~%" details))
             ((send-result)
              (format #t "    App->>LLM: Function result~%"))
             ((final-answer)
              (format #t "    LLM->>App: Final answer~%"))
             ((return-answer)
              (format #t "    App->>User: ~a~%" details))
             ((direct-answer)
              (format #t "    LLM->>App: Direct answer~%"))
             ((error)
              (format #t "    Note over App,LLM: Error: ~a~%" details))))))
      events)
    
    (format #t "```~%")))

(define (generate-mermaid-flowchart)
  "Generates a Mermaid flowchart from recorded events"
  (format #t "```mermaid~%")
  (format #t "graph TD~%")
  (format #t "    Start([User Input])~%")
  
  (let ((step-num 0))
    (for-each
      (lambda (event)
        (match event
          ((time actor action details)
           (set! step-num (+ step-num 1))
           (case action
             ((analyze-intent)
              (format #t "    A~a[Analyze Intent]~%" step-num)
              (if (string-contains details "function")
                  (format #t "    A~a -->|Function needed| F~a[Function Call]~%" 
                          step-num step-num)
                  (format #t "    A~a -->|Direct answer| D~a[Generate Answer]~%" 
                          step-num step-num)))
             ((execute-function)
              (format #t "    F~a[Execute: ~a]~%" step-num details))
             ((function-result)
              (format #t "    R~a[Result: ~a]~%" step-num 
                      (if (> (string-length details) 20)
                          (string-append (substring details 0 20) "...")
                          details)))))))
      (reverse flow-events)))
  
  (format #t "    End([Final Answer])~%")
  (format #t "```~%"))

;; Example usage within a mock execution
(define (traced-mock-execution prompt)
  "Runs a mock execution with flow tracing"
  (clear-flow-events!)
  
  ;; Record initial prompt
  (record-flow-event! 'user 'send-prompt prompt)
  (record-flow-event! 'app 'forward-prompt prompt)
  
  ;; Analyze intent
  (let ((decision (if (string-contains-ci prompt "calculate")
                     'function-call
                     'direct-answer)))
    (record-flow-event! 'llm 'analyze-intent 
                       (format #f "~a" decision))
    
    (case decision
      ((function-call)
       ;; Function call flow
       (record-flow-event! 'llm 'request-function "calculate")
       (record-flow-event! 'app 'execute-function "calculate(2+2)")
       (record-flow-event! 'func 'function-result "4")
       (record-flow-event! 'app 'send-result "4")
       (record-flow-event! 'llm 'final-answer "The result is 4")
       (record-flow-event! 'app 'return-answer "The result is 4"))
      
      ((direct-answer)
       ;; Direct answer flow
       (record-flow-event! 'llm 'direct-answer "Providing information")
       (record-flow-event! 'app 'return-answer "Here's your answer...")))))

;; Test scenarios
(define (visualize-scenario name prompt)
  (format #t "~%## Scenario: ~a~%" name)
  (format #t "Prompt: \"~a\"~%~%" prompt)
  
  (traced-mock-execution prompt)
  
  (format #t "### Sequence Diagram~%")
  (generate-mermaid-sequence)
  
  (format #t "~%### Flow Chart~%")
  (generate-mermaid-flowchart)
  (format #t "~%"))

;; Main
(define (main args)
  (format #t "# LLM Actor Flow Visualization~%")
  (format #t "Generated on: ~a~%"
          (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
  
  (visualize-scenario 
    "Function Call Flow"
    "Can you calculate 2 + 2?")
  
  (visualize-scenario
    "Direct Answer Flow" 
    "What is the capital of France?")
  
  (format #t "~%---~%")
  (format #t "These diagrams show the actual control flow ")
  (format #t "as specified in the sequence diagram.~%"))

(main (command-line))