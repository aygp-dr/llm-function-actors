#!/usr/bin/env guile
!#

(use-modules (ice-9 match)
             (ice-9 format)
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
(define (application-actor inbox outbox)
  "Application actor that sends prompts and executes functions"
  (let loop ((state 'idle))
    (match (receive-message inbox)
      [($ <message> 'start prompt _)
       ;; Send initial prompt with function definitions
       (send-message outbox 
                     (make-message 'prompt 
                                   `((prompt . ,prompt)
                                     (functions . ,(map car function-registry)))
                                   'application))
       (loop 'waiting-for-response)]
      
      [($ <message> 'function-call (name . args) 'llm)
       ;; Execute requested function
       (let* ((fn (assoc-ref function-registry name))
              (result (if fn
                          (apply fn args)
                          `(error . ,(format #f "Unknown function: ~a" name)))))
         (send-message outbox
                       (make-message 'function-result
                                     `((function . ,name)
                                       (result . ,result))
                                     'application))
         (loop 'waiting-for-final))]
      
      [($ <message> 'final-answer answer 'llm)
       (format #t "Final Answer: ~a~%" answer)
       (loop 'complete)]
      
      [msg
       (format #t "Application: Unexpected message ~a in state ~a~%" msg state)
       (loop state)])))

;; Actor: LLM Provider
(define (llm-actor inbox outbox)
  "LLM actor that processes prompts and decides on actions"
  (let loop ((context '()))
    (match (receive-message inbox)
      [($ <message> 'prompt content 'application)
       (let ((prompt (assoc-ref content 'prompt))
             (functions (assoc-ref content 'functions)))
         ;; Simulate decision making
         (cond
           [(string-contains prompt "calculate")
            ;; Decide to call a function
            (send-message outbox
                          (make-message 'function-call
                                        '(calculate . (5 3))
                                        'llm))]
           [else
            ;; Direct response
            (send-message outbox
                          (make-message 'final-answer
                                        "I'll help you with that."
                                        'llm))])
         (loop `((prompt . ,prompt) . ,context)))]
      
      [($ <message> 'function-result content 'application)
       (let ((result (assoc-ref content 'result)))
         ;; Incorporate function result and generate final answer
         (send-message outbox
                       (make-message 'final-answer
                                     (format #f "Based on the calculation, the result is: ~a" result)
                                     'llm))
         (loop `((last-result . ,result) . ,context)))]
      
      [msg
       (format #t "LLM: Unexpected message ~a~%" msg)
       (loop context)])))

;; Message passing infrastructure
(define (make-channel)
  "Create a bidirectional channel"
  (cons (make-queue) (make-queue)))

(define (send-message channel msg)
  "Send a message to a channel"
  (enqueue! (car channel) msg))

(define (receive-message channel)
  "Blocking receive from a channel"
  (let loop ()
    (if (queue-empty? (cdr channel))
        (begin (usleep 1000) (loop))
        (dequeue! (cdr channel)))))

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
  (let ((app-to-llm (make-channel))
        (llm-to-app (cons (cdr app-to-llm) (car app-to-llm))))
    
    ;; Start actors in separate threads
    (call-with-new-thread
     (lambda ()
       (application-actor llm-to-app app-to-llm)))
    
    (call-with-new-thread
     (lambda ()
       (llm-actor app-to-llm llm-to-app)))
    
    ;; Initiate conversation
    (send-message app-to-llm
                  (make-message 'start prompt 'user))
    
    ;; Wait for completion
    (sleep 2)))

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