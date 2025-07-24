#!/usr/bin/env guile
!#

(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 threads)
             (ice-9 ftw)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

;; Define message types
(define-record-type <message>
  (make-message type content sender)
  message?
  (type message-type)
  (content message-content)
  (sender message-sender))

;; Define function registry
(define function-registry '())

(define (register-function! name fn)
  "Register a function that can be called by the LLM"
  (set! function-registry 
        (assoc-set! function-registry name fn)))

;; File operation tools
(define (read-file-tool path)
  "Read file contents"
  (catch 'system-error
    (lambda ()
      (call-with-input-file path get-string-all))
    (lambda (key . args)
      (format #f "Error reading file ~a: ~a" path (car args)))))

(define (write-file-tool path content)
  "Create or update file"
  (catch 'system-error
    (lambda ()
      (call-with-output-file path
        (lambda (port)
          (display content port)))
      (format #f "Successfully wrote ~a bytes to ~a" 
              (string-length content) path))
    (lambda (key . args)
      (format #f "Error writing file ~a: ~a" path (car args)))))

(define (list-files-tool path)
  "List directory contents"
  (catch 'system-error
    (lambda ()
      (let ((files '()))
        (ftw path
             (lambda (filename statinfo flag)
               (when (eq? flag 'regular)
                 (set! files (cons filename files)))
               #t)
             'depth-first)
        (reverse files)))
    (lambda (key . args)
      (format #f "Error listing directory ~a: ~a" path (car args)))))

;; Register file operation functions
(register-function! 'read_file
                    (lambda (path)
                      (read-file-tool path)))

(register-function! 'write_file
                    (lambda (path content)
                      (write-file-tool path content)))

(register-function! 'list_files
                    (lambda (path)
                      (list-files-tool path)))

;; Actor: Application
(define (application-actor llm-channel)
  "Application actor that executes file operations"
  (let loop ()
    (let ((msg (receive-message llm-channel)))
      (match msg
        [($ <message> 'function-call content 'llm)
         ;; Execute requested function
         (let* ((name (car content))
                (args (cdr content))
                (fn (assoc-ref function-registry name))
                (result (if fn
                            (apply fn args)
                            `(error . ,(format #f "Unknown function: ~a" name)))))
           (send-message llm-channel
                         (make-message 'function-result
                                       `((function . ,name)
                                         (result . ,result))
                                       'application))
           (loop))]
        
        [($ <message> 'final-answer answer 'llm)
         (format #t "Final Answer: ~a~%" answer)
         ;; Continue listening for more requests
         (loop)]
        
        [($ <message> 'shutdown _ _)
         #t]
        
        [($ <message> 'function-result content sender)
         ;; Log function result (this might be echoed back from channel)
         (format #t "Application: Function ~a returned ~a~%" 
                 (assoc-ref content 'function)
                 (assoc-ref content 'result))
         (loop)]
        
        [msg
         (format #t "Application: Unexpected message ~a~%" msg)
         (loop)]))))

;; Actor: LLM Provider
(define (llm-actor app-channel prompt)
  "LLM actor that demonstrates file operations"
  ;; Simulate different file operations based on prompt
  (cond
    [(string-contains prompt "list")
     ;; List files in current directory
     (send-message app-channel
                   (make-message 'function-call
                                 '(list_files . ("."))
                                 'llm))
     ;; Wait for result
     (let ((result-msg (receive-message app-channel)))
       (match result-msg
         [($ <message> 'function-result content 'application)
          (let ((files (assoc-ref content 'result)))
            (send-message app-channel
                          (make-message 'final-answer
                                        (format #f "Found ~a files:\n~a"
                                                (length files)
                                                (string-join files "\n"))
                                        'llm)))]))]
    
    [(string-contains prompt "read")
     ;; Extract filename from prompt (simple parsing)
     (let ((words (string-split prompt #\space))
           (filename (find (lambda (w) (string-contains w ".")) words)))
       (if filename
           (begin
             (send-message app-channel
                           (make-message 'function-call
                                         `(read_file . (,filename))
                                         'llm))
             ;; Wait for result
             (let ((result-msg (receive-message app-channel)))
               (match result-msg
                 [($ <message> 'function-result content 'application)
                  (let ((file-content (assoc-ref content 'result)))
                    (send-message app-channel
                                  (make-message 'final-answer
                                                (format #f "File contents:\n~a" 
                                                        file-content)
                                                'llm)))])))
           (send-message app-channel
                         (make-message 'final-answer
                                       "Please specify a filename to read"
                                       'llm))))]
    
    [(string-contains prompt "write")
     ;; Demo: Create a test file
     (send-message app-channel
                   (make-message 'function-call
                                 '(write_file . ("test-output.txt" "Hello from file tools simulator!\nThis is a test file."))
                                 'llm))
     ;; Wait for result
     (let ((result-msg (receive-message app-channel)))
       (match result-msg
         [($ <message> 'function-result content 'application)
          (let ((result (assoc-ref content 'result)))
            (send-message app-channel
                          (make-message 'final-answer
                                        result
                                        'llm)))]))]
    
    [else
     ;; Direct response
     (send-message app-channel
                   (make-message 'final-answer
                                 "I can help you with file operations. Try: 'list files', 'read <filename>', or 'write a file'."
                                 'llm))]))

;; Message passing infrastructure
(define (make-channel)
  "Create a thread-safe channel"
  (let ((queue (make-queue))
        (mutex (make-mutex))
        (condvar (make-condition-variable)))
    (list queue mutex condvar)))

(define (send-message channel msg)
  "Send a message to a channel"
  (match channel
    [(queue mutex condvar)
     (with-mutex mutex
       (enqueue! queue msg)
       (signal-condition-variable condvar))]))

(define (receive-message channel)
  "Blocking receive from a channel"
  (match channel
    [(queue mutex condvar)
     (with-mutex mutex
       (while (queue-empty? queue)
         (wait-condition-variable condvar mutex))
       (dequeue! queue))]))

;; Queue implementation
(define (make-queue)
  (cons '() '()))

(define (enqueue! q item)
  (set-cdr! q (cons item (cdr q))))

(define (dequeue! q)
  (if (null? (car q))
      (begin
        (set-car! q (reverse (cdr q)))
        (set-cdr! q '())))
  (let ((item (caar q)))
    (set-car! q (cdar q))
    item))

(define (queue-empty? q)
  (and (null? (car q)) (null? (cdr q))))

;; Simulation runner
(define (run-file-tools-simulation prompt)
  "Run a file tools simulation"
  (let ((channel (make-channel))
        (timeout-seconds 10))
    
    ;; Start application actor
    (let ((app-thread
           (call-with-new-thread
            (lambda ()
              (application-actor channel)))))
      
      ;; Start LLM actor
      (let ((llm-thread
             (call-with-new-thread
              (lambda ()
                (llm-actor channel prompt)))))
        
        ;; Wait for completion with timeout
        (let ((start-time (current-time)))
          (while (and (not (thread-exited? llm-thread))
                      (< (- (current-time) start-time) timeout-seconds))
            (usleep 100000)) ; Sleep 100ms
          
          ;; Handle timeout or completion
          (if (thread-exited? llm-thread)
              ;; Normal completion - shutdown application
              (begin
                (send-message channel (make-message 'shutdown '() 'system))
                (join-thread app-thread))
              ;; Timeout - cancel threads
              (begin
                (format #t "~%Timeout: Simulation took longer than ~a seconds~%" timeout-seconds)
                (cancel-thread llm-thread)
                (cancel-thread app-thread))))))))

;; Export for use in other modules
(export run-file-tools-simulation
        register-function!
        read-file-tool
        write-file-tool
        list-files-tool)