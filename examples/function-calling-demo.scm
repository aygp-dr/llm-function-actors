#!/usr/bin/env guile
!#

;; Load from relative path when run from project root
(primitive-load (string-append 
                 (dirname (current-filename))
                 "/../src/function-calling-simulator.scm"))

;; Register additional functions
(register-function! 'weather
                    (lambda (city)
                      (format #f "22°C and sunny in ~a" city)))

(register-function! 'search
                    (lambda (query)
                      `((results . (("Title 1" . "Content 1")
                                    ("Title 2" . "Content 2"))))))

;; Run simulations
(display "=== Calculation Request ===\n")
(run-simulation "Please calculate 5 + 3 for me")

(sleep 1)

(display "\n=== Direct Response ===\n")
(run-simulation "Hello, how are you?")