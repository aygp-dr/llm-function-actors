#!/usr/bin/env guile3
!#

;;; Validation suite for Ollama tool calling

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (src ollama-client)
             (src file-tools)
             (ice-9 format)
             (srfi srfi-64))

(test-runner-factory 
 (lambda () 
   (let ((runner (test-runner-simple)))
     (test-runner-on-test-end! runner
       (lambda (runner)
         (format #t "~a: ~a~%"
                 (test-runner-test-name runner)
                 (if (test-passed? runner) "PASS" "FAIL"))))
     runner)))

(test-begin "ollama-tool-validation")

(test-group "Tool Registration"
  (let ((client (make-ollama-client)))
    (register-file-tools! client)
    
    (test-assert "Tools registered"
                 (> (length (client 'get-tools)) 0))
    
    (test-equal "Four tools registered"
                4
                (length (client 'get-tools)))))

(test-group "File Operations"
  (test-assert "Read existing file"
               (string? (read-file-tool "README.md")))
  
  (test-assert "Write and read file"
               (begin
                 (write-file-tool "test-output.txt" "Test content")
                 (string=? "Test content" 
                          (read-file-tool "test-output.txt"))))
  
  (test-assert "List files"
               (list? (list-files-tool ".")))
  
  (test-assert "Search code"
               (list? (search-code-tool "define" "."))))

(test-end "ollama-tool-validation")
