#!/usr/bin/env guile
!#

(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 threads)
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

;; Actor: Application
(define (application-actor llm-channel)
  "Application actor that sends prompts and executes functions"
  (let loop ((state 'idle))
    (match state
      ['idle
       ;; Wait for startup signal
       (loop 'ready)]
      
      ['ready
       ;; This is triggered by run-simulation
       (loop 'waiting-for-response)]
      
      ['waiting-for-response
       ;; Receive from LLM
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
              (loop 'waiting-for-final))]
           
           [($ <message> 'final-answer answer 'llm)
            (format #t "Final Answer: ~a~%" answer)
            (loop 'complete)]
           
           [msg
            (format #t "Application: Unexpected message ~a~%" msg)
            (loop state)]))]
      
      ['waiting-for-final
       ;; Receive final answer from LLM
       (let ((msg (receive-message llm-channel)))
         (match msg
           [($ <message> 'final-answer answer 'llm)
            (format #t "Final Answer: ~a~%" answer)
            (loop 'complete)]
           
           [msg
            (format #t "Application: Unexpected message ~a~%" msg)
            (loop state)]))]
      
      ['complete
       ;; Done
       #t])))

;; Actor: LLM Provider
(define (llm-actor app-channel prompt)
  "LLM actor that processes prompts and decides on actions"
  ;; Send initial prompt analysis
  (let ((functions (map car function-registry)))
    ;; Simulate decision making
    (cond
      [(string-contains prompt "calculate")
       ;; Decide to call a function
       (send-message app-channel
                     (make-message 'function-call
                                   '(calculate . (5 3))
                                   'llm))
       ;; Wait for function result
       (let ((result-msg (receive-message app-channel)))
         (match result-msg
           [($ <message> 'function-result content 'application)
            (let ((result (assoc-ref content 'result)))
              ;; Incorporate function result and generate final answer
              (send-message app-channel
                            (make-message 'final-answer
                                          (format #f "Based on the calculation, the result is: ~a" result)
                                          'llm)))]
           [msg
            (format #t "LLM: Unexpected message while waiting for result: ~a~%" msg)]))]
      [else
       ;; Direct response
       (send-message app-channel
                     (make-message 'final-answer
                                   "I'll help you with that."
                                   'llm))])))

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

;; Example functions available to LLM
(register-function! 'calculate
                    (lambda (a b)
                      (+ a b)))

(register-function! 'get-time
                    (lambda ()
                      (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))))

;; Simulation runner
(define (run-simulation prompt)
  "Run a complete function calling simulation"
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
          
          ;; Cancel threads if timeout
          (when (not (thread-exited? llm-thread))
            (format #t "~%Timeout: Simulation took longer than ~a seconds~%" timeout-seconds)
            (cancel-thread llm-thread))
          
          (when (not (thread-exited? app-thread))
            (cancel-thread app-thread)))))))

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