;;; llm-function-actors.el --- Support for LLM Function Actors project -*- lexical-binding: t; -*-

;; Copyright (C) 2024 aygp-dr

;; Author: aygp-dr
;; Keywords: languages, tools
;; Version: 0.1.0

;;; Commentary:

;; This file provides Emacs support for working with the LLM Function Actors project.
;; It configures Geiser for Guile 3, sets up org-babel for various languages,
;; and provides project-specific utilities.

;;; Code:

(require 'org)
(require 'ob)
(require 'geiser nil t)

;;;; Geiser Configuration

(defun llm-function-actors-setup-geiser ()
  "Configure Geiser for Guile 3."
  (when (fboundp 'geiser-guile-set-implementation)
    (setq geiser-guile-binary "guile3"
          geiser-active-implementations '(guile)
          geiser-default-implementation 'guile))
  
  ;; Add project source directory to Guile load path
  (when (boundp 'geiser-guile-load-path)
    (add-to-list 'geiser-guile-load-path
                 (expand-file-name "src" (file-name-directory load-file-name)))))

;;;; Org-babel Configuration

(defun llm-function-actors-setup-org-babel ()
  "Configure org-babel for project languages."
  ;; Enable languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (scheme . t)
     (shell . t)))
  
  ;; Mermaid support (if mermaid-cli is installed)
  (when (executable-find "mmdc")
    (defun org-babel-execute:mermaid (body params)
      "Execute a mermaid code block."
      (let* ((file (or (cdr (assoc :file params))
                       (error "Mermaid code blocks require a :file parameter")))
             (theme (or (cdr (assoc :theme params)) "default"))
             (background (or (cdr (assoc :background params)) "white"))
             (cmd (format "mmdc -i - -o %s -t %s -b %s"
                          (shell-quote-argument file)
                          theme background)))
        (with-temp-buffer
          (insert body)
          (shell-command-on-region (point-min) (point-max) cmd))
        nil)))
  
  ;; Don't ask for confirmation when executing code blocks
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (member lang '("scheme" "emacs-lisp" "shell" "mermaid"))))))

;;;; Project Utilities

(defun llm-function-actors-run-demo ()
  "Run the function calling demo."
  (interactive)
  (let ((default-directory (file-name-directory load-file-name)))
    (compile "gmake demo")))

(defun llm-function-actors-run-simulator ()
  "Run the main simulator."
  (interactive)
  (let ((default-directory (file-name-directory load-file-name)))
    (compile "gmake run")))

(defun llm-function-actors-tangle-setup ()
  "Tangle the SETUP.org file."
  (interactive)
  (let ((setup-file (expand-file-name "SETUP.org" 
                                      (file-name-directory load-file-name))))
    (when (file-exists-p setup-file)
      (with-current-buffer (find-file-noselect setup-file)
        (org-babel-tangle)
        (message "Tangled SETUP.org")))))

;;;; Key Bindings

(defvar llm-function-actors-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'llm-function-actors-run-demo)
    (define-key map (kbd "C-c C-r") #'llm-function-actors-run-simulator)
    (define-key map (kbd "C-c C-t") #'llm-function-actors-tangle-setup)
    map)
  "Keymap for llm-function-actors minor mode.")

;;;; Minor Mode

;;;###autoload
(define-minor-mode llm-function-actors-mode
  "Minor mode for LLM Function Actors project."
  :lighter " LLM-FA"
  :keymap llm-function-actors-mode-map
  (when llm-function-actors-mode
    (llm-function-actors-setup-geiser)
    (llm-function-actors-setup-org-babel)))

;;;; Auto-enable in project

;;;###autoload
(defun llm-function-actors-enable-maybe ()
  "Enable llm-function-actors-mode if in project directory."
  (when (and (buffer-file-name)
             (string-match-p "llm-function-actors" (buffer-file-name)))
    (llm-function-actors-mode 1)))

(add-hook 'scheme-mode-hook #'llm-function-actors-enable-maybe)
(add-hook 'org-mode-hook #'llm-function-actors-enable-maybe)

(provide 'llm-function-actors)
;;; llm-function-actors.el ends here