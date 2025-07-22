#!/usr/bin/env guile
!#

;; Load from relative path when run from project root
(primitive-load (string-append 
                 (dirname (current-filename))
                 "/../src/file-tools-simulator.scm"))

;; Run demonstrations
(display "=== List Files Demo ===\n")
(run-file-tools-simulation "list files in current directory")

(sleep 1)

(display "\n=== Write File Demo ===\n") 
(run-file-tools-simulation "write a test file")

(sleep 1)

(display "\n=== Read File Demo ===\n")
(run-file-tools-simulation "read test-output.txt")

(sleep 1)

(display "\n=== Help Demo ===\n")
(run-file-tools-simulation "help me with files")