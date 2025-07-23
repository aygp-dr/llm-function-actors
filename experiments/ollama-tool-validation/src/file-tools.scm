#!/usr/bin/env guile3
!#

;;; File tools for Ollama tool calling
;;; Minimal set of file operations

(define-module (file-tools)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:export (read-file-tool
            write-file-tool
            list-files-tool
            search-code-tool
            register-file-tools!))

(define (read-file-tool path)
  "Read contents of a file"
  (catch #t
    (lambda ()
      (let ((content (call-with-input-file path get-string-all)))
        `((success . #t)
          (content . ,content)
          (path . ,path))))
    (lambda (key . args)
      `((success . #f)
        (error . ,(format #f "Error reading file ~a: ~a" path (car args)))))))

(define (write-file-tool path content)
  "Write content to a file"
  (catch #t
    (lambda ()
      (call-with-output-file path
        (lambda (port)
          (display content port)))
      `((success . #t)
        (message . ,(format #f "Successfully wrote ~a bytes to ~a" 
                           (string-length content) path))
        (path . ,path)))
    (lambda (key . args)
      `((success . #f)
        (error . ,(format #f "Error writing file ~a: ~a" path (car args)))))))

(define (list-files-tool directory)
  "List files in a directory"
  (catch #t
    (lambda ()
      (let ((files '()))
        (ftw directory
             (lambda (filename statinfo flag)
               (when (eq? flag 'regular)
                 (set! files (cons filename files)))
               #t))
        `((success . #t)
          (files . ,(reverse files))
          (count . ,(length files)))))
    (lambda (key . args)
      `((success . #f)
        (error . ,(format #f "Error listing directory ~a: ~a" directory (car args)))))))

(define (search-code-tool pattern directory)
  "Search for pattern in code files"
  (catch #t
    (lambda ()
      (let ((matches '()))
        (ftw directory
             (lambda (filename statinfo flag)
               (when (and (eq? flag 'regular)
                         (or (string-suffix? ".scm" filename)
                             (string-suffix? ".el" filename)
                             (string-suffix? ".py" filename)
                             (string-suffix? ".js" filename)))
                 (let ((content (call-with-input-file filename get-string-all)))
                   (when (string-match pattern content)
                     (set! matches (cons filename matches)))))
               #t))
        `((success . #t)
          (matches . ,(reverse matches))
          (pattern . ,pattern)
          (count . ,(length matches)))))
    (lambda (key . args)
      `((success . #f)
        (error . ,(format #f "Error searching in ~a: ~a" directory (car args)))))))

(define (register-file-tools! client)
  "Register all file tools with the Ollama client"
  (register-tool! client 'read_file
                  read-file-tool
                  "Read the contents of a file"
                  '((type . "object")
                    (properties . ((path . ((type . "string")
                                           (description . "Path to the file to read")))))
                    (required . ("path"))))
  
  (register-tool! client 'write_file
                  write-file-tool
                  "Write content to a file"
                  '((type . "object")
                    (properties . ((path . ((type . "string")
                                           (description . "Path to the file to write")))
                                  (content . ((type . "string")
                                             (description . "Content to write to the file")))))
                    (required . ("path" "content"))))
  
  (register-tool! client 'list_files
                  list-files-tool
                  "List files in a directory"
                  '((type . "object")
                    (properties . ((directory . ((type . "string")
                                                (description . "Directory path to list files from")))))
                    (required . ("directory"))))
  
  (register-tool! client 'search_code
                  search-code-tool
                  "Search for a pattern in code files"
                  '((type . "object")
                    (properties . ((pattern . ((type . "string")
                                              (description . "Regex pattern to search for")))
                                  (directory . ((type . "string")
                                               (description . "Directory to search in")))))
                    (required . ("pattern" "directory")))))
