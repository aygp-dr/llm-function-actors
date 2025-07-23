#!/usr/bin/env guile
!#

;; Test Ollama tool calling with various models
(add-to-load-path "..")
(use-modules (ice-9 format)
             (ice-9 receive)
             (web client)
             (web response)
             (json))

(define (test-ollama-tools model)
  "Test tool calling with a specific Ollama model"
  (let* ((url "http://localhost:11434/api/chat")
         (weather-tool '((type . "function")
                         (function . ((name . "get_weather")
                                      (description . "Get weather for a location")
                                      (parameters . ((type . "object")
                                                     (properties . ((location . ((type . "string")
                                                                                 (description . "City name")))
                                                                    (unit . ((type . "string")
                                                                             (enum . #("celsius" "fahrenheit"))))))
                                                     (required . #("location"))))))))
         
         (body `((model . ,model)
                 (messages . #(((role . "user")
                                (content . "What's the weather in Tokyo? Please use Celsius."))))
                 (tools . #(,weather-tool))
                 (stream . #f))))
    
    (format #t "~%Testing model: ~a~%" model)
    (format #t "Sending request...~%")
    
    (catch #t
      (lambda ()
        (receive (response body-port)
            (http-post url
                       #:body (scm->json-string body)
                       #:headers '((content-type . "application/json"))
                       #:decode-body? #f)
          
          (let ((status (response-code response)))
            (if (= status 200)
                (let* ((result (json->scm body-port))
                       (message (assoc-ref result 'message))
                       (tool-calls (assoc-ref message 'tool_calls)))
                  
                  (format #t "Response content: ~a~%" (assoc-ref message 'content))
                  
                  (if tool-calls
                      (begin
                        (format #t "~%Tool calls detected:~%")
                        (for-each
                          (lambda (tool-call)
                            (let* ((function (assoc-ref tool-call 'function))
                                   (name (assoc-ref function 'name))
                                   (arguments (assoc-ref function 'arguments)))
                              (format #t "  Function: ~a~%" name)
                              (format #t "  Arguments: ~a~%" arguments)))
                          (vector->list tool-calls)))
                      (format #t "No tool calls in response~%")))
                
                (format #t "Error: HTTP ~a~%" status)))))
      
      (lambda (key . args)
        (format #t "Error: ~a ~a~%" key args)))))

;; Test multiple models
(define models-to-test
  '("llama3.1"      ; Latest with native tool support
    "mistral"       ; Fast and efficient
    "mixtral"       ; More complex reasoning
    "qwen2.5"))     ; Alternative model

;; Check if Ollama is running
(define (check-ollama)
  "Check if Ollama service is accessible"
  (catch #t
    (lambda ()
      (receive (response body)
          (http-get "http://localhost:11434/api/tags")
        (= (response-code response) 200)))
    (lambda _ #f)))

;; Main execution
(if (check-ollama)
    (begin
      (format #t "Ollama service detected. Testing tool support...~%")
      (format #t "================================================~%")
      
      ;; Get list of available models
      (receive (response body-port)
          (http-get "http://localhost:11434/api/tags"
                    #:decode-body? #f)
        (let* ((models-data (json->scm body-port))
               (installed-models (map (lambda (m) 
                                        (assoc-ref m 'name))
                                      (vector->list (assoc-ref models-data 'models)))))
          
          (format #t "Installed models: ~a~%" installed-models)
          (format #t "~%")
          
          ;; Test each available model
          (for-each
            (lambda (model)
              (when (member model installed-models)
                (test-ollama-tools model)
                (format #t "~%---~%")))
            models-to-test))))
    
    (begin
      (format #t "Error: Ollama service not found!~%")
      (format #t "Please ensure Ollama is running:~%")
      (format #t "  1. Install: https://ollama.com~%")
      (format #t "  2. Start service: ollama serve~%")
      (format #t "  3. Pull a model: ollama pull llama3.1~%")))