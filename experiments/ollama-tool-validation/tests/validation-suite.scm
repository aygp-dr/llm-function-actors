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
               (let ((result (read-file-tool "README.org")))
                 (assq-ref result 'success)))
  
  (test-assert "Write and read file"
               (begin
                 (write-file-tool "test-output.txt" "Test content")
                 (let ((result (read-file-tool "test-output.txt")))
                   (and (assq-ref result 'success)
                        (string=? "Test content" 
                                 (assq-ref result 'content))))))
  
  (test-assert "List files returns alist"
               (let ((result (list-files-tool ".")))
                 (assq-ref result 'success)))
  
  (test-assert "Search code returns alist"
               (let ((result (search-code-tool "define" ".")))
                 (assq-ref result 'success))))

(test-end "ollama-tool-validation")
