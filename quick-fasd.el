;;; quick-fasd.el --- Emacs integration for the command-line productivity booster `fasd' -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti
;; Copyright (C) 2013 steckerhalter

;; Maintainer: James Cherti
;; Original Author: steckerhalter
;; URL: https://github.com/jamescherti/quick-fasd.el
;; Keywords: cli bash zsh autojump

;;; Commentary:
;; An Emacs extension to integrate Fasd.
;;
;; - Hooks into to `find-file-hook' to add all visited files and directories to
;;   `fasd'.
;; - Adds the function `quick-fasd-find-file' to prompt and fuzzy complete
;;   available candidates
;;
;; Requirements:
;; - `fasd' command line tool, see: https://github.com/clvv/fasd
;;
;; Usage:
;; (require 'quick-fasd)
;; (quick-fasd-mode 1)
;;
;; Optionally bind `quick-fasd-find-file' to a key:
;; (global-set-key (kbd "C-h C-/") 'quick-fasd-find-file)

;;; Code:

(defgroup quick-fasd nil
  "Navigate previously-visited files and directories easily."
  :group 'tools
  :group 'convenience)

(defcustom quick-fasd-enable-initial-prompt t
  "Specify whether to enable prompt for the initial query.
When set to nil, all fasd results are returned for completion."
  :type 'boolean)

(defcustom quick-fasd-file-manager 'dired
  "Default file manager used by `quick-fasd-find-file-action'."
  :type '(radio
          (const :tag "Dired, the default Emacs file manager" dired)
          (const :tag "Deer, Ranger's file manager" deer)
          (function :tag "Custom file manager function")))

(defcustom quick-fasd-standard-search "-a"
  "Standard search parameter for `fasd'.
Available options:
- `-a': Match files and directories
- `-d': Match directories only
- `-f': Match files only
- `-r': Match by rank only
- `-t': Match by recent access only
Multiple flags can be specified with spaces, e.g., \"-a -r\"."
  :type 'string)

(defvar quick-fasd--executable-path nil
  "Cached path to the `fasd` executable, or nil if not found.")

(defun quick-fasd--check-fasd-executable ()
  "Return the path to the `fasd` executable or signal an error if not found."
  (unless quick-fasd--executable-path
    (setq quick-fasd--executable-path (executable-find "fasd"))
    (unless quick-fasd--executable-path
      (error "Fasd executable not found; required for `quick-fasd'"))))

(defun quick-fasd-find-file-action (file)
  "Open FILE with appropriate file manager or prompt if unreadable."
  (if (file-readable-p file)
      (if (file-directory-p file)
          (funcall quick-fasd-file-manager file)
        (find-file file))
    (message "Directory or file `%s' doesn't exist" file)))

;;;###autoload
(defun quick-fasd-find-file (&optional query)
  "Use fasd to open a file or directory.
Optionally pass QUERY to avoid prompt."
  (interactive "P")
  (quick-fasd--check-fasd-executable)
  (unless query (setq query (if quick-fasd-enable-initial-prompt
                                (read-from-minibuffer "Fasd query: ")
                              "")))
  (let* ((prompt "Fasd query: ")
         (results
          (split-string
           (shell-command-to-string
            (concat "fasd -l -R"
                    (concat " " quick-fasd-standard-search " ")
                    query))
           "\n" t))
         (file (when results
                 (setq this-command 'quick-fasd-find-file)
                 (completing-read prompt results nil t))))
    (if (not file)
        (message "Fasd found nothing for query `%s'" query)
      (quick-fasd-find-file-action file))))

;;;###autoload
(defun quick-fasd-add-file-to-db ()
  "Add current file or directory to the Fasd database."
  (quick-fasd--check-fasd-executable)
  (let ((file (if (eq major-mode 'dired-mode)
                  dired-directory
                (buffer-file-name (buffer-base-buffer)))))
    (when (and file
               (stringp file)
               (file-readable-p file))
      (start-process "*fasd*" nil "fasd" "--add" file))))

;;;###autoload
(define-minor-mode quick-fasd-mode
  "Toggle fasd mode globally."
  :global t
  :group 'quick-fasd

  (if quick-fasd-mode
      (progn (add-hook 'find-file-hook #'quick-fasd-add-file-to-db)
             (add-hook 'dired-mode-hook #'quick-fasd-add-file-to-db))
    (remove-hook 'find-file-hook #'quick-fasd-add-file-to-db)
    (remove-hook 'dired-mode-hook #'quick-fasd-add-file-to-db)))

(provide 'quick-fasd)
;;; quick-fasd.el ends here
