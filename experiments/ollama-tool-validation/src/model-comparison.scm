#!/usr/bin/env guile3
!#

;;; Compare tool calling support across different models

(add-to-load-path (dirname (current-filename)))

(use-modules (ollama-client)
             (file-tools)
             (ice-9 format))

(define (test-model-tool-support)
  "Test tool support across different models"
  (format #t "Testing tool support across models:~%~%")
  
  (let ((client (make-ollama-client))
        (models '("llama3.2:3b" "llama3.2:1b" "llama3.1:8b")))
    
    (register-file-tools! client)
    
    (for-each
     (lambda (model)
       (format #t "Testing ~a:~%" model)
       (catch #t
         (lambda ()
           (let* ((messages `(((role . "user") 
                              (content . "List all .txt files in the current directory"))))
                  (response (ollama-chat client model messages)))
             (if (assq-ref response 'message)
                 (format #t "✓ ~a supports tools!~%" model)
                 (format #t "✗ ~a tool support unclear~%" model))))
         (lambda (key . args)
           (format #t "✗ ~a failed: ~a~%" model (car args))))
       (format #t "~%"))
     models)))

(test-model-tool-support)
