;;; quick-fasd.el --- Emacs integration for the command-line tool `fasd' -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti
;; Copyright (C) 2013-2021 steckerhalter

;; Maintainer: James Cherti
;; Original Author: steckerhalter
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/quick-fasd.el
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The quick-fasd Emacs package integrates the Fasd tool within the Emacs
;; environment. Fasd, a command-line utility, enhances the productivity of users
;; by providing fast access to frequently used files and directories. Inspired
;; by tools such as autojump, z, and v, Fasd functions by maintaining a dynamic
;; index of files and directories you access, allowing you to reference them
;; quickly from the command line.
;;
;; After installing quick-fasd in Emacs, you can easily navigate your file
;; system directly within Emacs by using Fasd’s fast-access capabilities. For
;; example, you can open recently accessed files or quickly jump to frequently
;; used directories without leaving the Emacs environment.

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

(defcustom quick-fasd-executable-path "fasd"
  "Path to the Fasd executable or its command name (e.g., fasd)."
  :type 'string)

(defun quick-fasd--get-fasd-executable-path ()
  "Return the path to the `fasd` executable or signal an error if not found."
  (let ((result (executable-find quick-fasd-executable-path)))
    (when (or (not result)
              (not (file-regular-p result))
              (not (file-executable-p result)))
      (error "The fasd executable was not found"))

    result))

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
  (let ((fasd-executable (quick-fasd--get-fasd-executable-path)))
    (unless query (setq query (if quick-fasd-enable-initial-prompt
                                  (read-from-minibuffer "Fasd: ")
                                "")))
    (let* ((prompt "Fasd: ")
           (results
            (split-string
             (shell-command-to-string
              (concat fasd-executable
                      " -l -R"
                      (concat " " quick-fasd-standard-search " ")
                      query))
             "\n" t))
           (file (when results
                   (setq this-command 'quick-fasd-find-file)
                   (completing-read prompt results nil t))))
      (if (not file)
          (message "Fasd found nothing for: '%s'" query)
        (quick-fasd-find-file-action file)))))

;;;###autoload
(defun quick-fasd-add-file-to-db ()
  "Add current file or directory to the Fasd database."
  (let ((fasd-executable (quick-fasd--get-fasd-executable-path)))
    (let ((file (if (eq major-mode 'dired-mode)
                    dired-directory
                  (buffer-file-name (buffer-base-buffer)))))
      (when (and file
                 (stringp file)
                 (file-readable-p file))
        (start-process "*fasd*" nil fasd-executable "--add" file)))))

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
