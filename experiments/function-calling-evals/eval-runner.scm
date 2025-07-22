#!/usr/bin/env guile
!#

;; Evaluation runner for LLM function calling tests

(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 rdelim)
             (json)
             (srfi srfi-1))

(define (load-json-file filepath)
  "Load and parse a JSON file"
  (call-with-input-file filepath
    (lambda (port)
      (json->scm port))))

(define (evaluate-test-case test-case tools)
  "Evaluate a single test case against expected behavior"
  (let* ((prompt (assoc-ref test-case "prompt"))
         (expected (assoc-ref test-case "expected"))
         (should-call (assoc-ref expected "should_call_function"))
         (expected-calls (assoc-ref expected "function_calls")))
    
    (format #t "~%Test ID: ~a~%" (assoc-ref test-case "id"))
    (format #t "Prompt: ~a~%" prompt)
    (format #t "Should call function: ~a~%" should-call)
    
    (when should-call
      (format #t "Expected calls:~%")
      (for-each 
        (lambda (call)
          (format #t "  - ~a(~a)~%"
                  (assoc-ref call "function")
                  (assoc-ref call "parameters")))
        (vector->list expected-calls)))))

(define (run-evaluation-suite suite-file)
  "Run all test cases in a suite"
  (let* ((suite-data (load-json-file suite-file))
         (suite-name (assoc-ref suite-data "suite_name"))
         (tools (assoc-ref suite-data "tools"))
         (test-cases (assoc-ref suite-data "test_cases")))
    
    (format #t "~%=== Running Suite: ~a ===~%" suite-name)
    (format #t "Available tools: ~a~%"
            (string-join 
              (map (lambda (tool) (assoc-ref tool "name"))
                   (vector->list tools))
              ", "))
    
    (for-each 
      (lambda (test-case)
        (evaluate-test-case test-case tools))
      (vector->list test-cases))))

(define (calculate-metrics results)
  "Calculate evaluation metrics"
  (let* ((total (length results))
         (correct-function-selection 
           (count (lambda (r) (assoc-ref r 'correct-selection)) results))
         (correct-parameters
           (count (lambda (r) (assoc-ref r 'correct-params)) results))
         (correct-chain
           (count (lambda (r) (assoc-ref r 'correct-chain)) results)))
    
    (format #t "~%=== Evaluation Metrics ===~%")
    (format #t "Total test cases: ~a~%" total)
    (format #t "Function selection accuracy: ~,2f%~%"
            (* 100.0 (/ correct-function-selection total)))
    (format #t "Parameter extraction accuracy: ~,2f%~%"
            (* 100.0 (/ correct-parameters total)))
    (format #t "Chain logic accuracy: ~,2f%~%"
            (* 100.0 (/ correct-chain total)))))

;; Main execution
(define (main args)
  (cond
    ((= (length args) 2)
     (let ((suite-file (second args)))
       (if (file-exists? suite-file)
           (run-evaluation-suite suite-file)
           (format #t "Error: File ~a not found~%" suite-file))))
    (else
     (format #t "Usage: ~a <test-suite.json>~%" (first args))
     (format #t "Available suites:~%")
     (format #t "  - test-cases/file-system-tools.json~%")
     (format #t "  - test-cases/calculator-tools.json~%")
     (format #t "  - test-cases/mixed-tools.json~%"))))

(main (command-line))