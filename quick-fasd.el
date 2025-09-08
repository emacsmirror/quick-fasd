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
;; system directly within Emacs by using Fasd's fast-access capabilities. For
;; example, you can open recently accessed files or quickly jump to frequently
;; used directories without leaving the Emacs environment.
;;
;; Here's how the package works:
;; - `quick-fasd-mode' adds a hook to `find-file-hook' and `dired-mode-hook' to
;;   automatically add all visited files and directories to Fasd's database.
;; - The user can invoke the `quick-fasd-find-file' function, which prompts for
;;   input and display available candidates from the Fasd index, enabling rapid
;;   and efficient file navigation.

;;; Code:

;;; Defcustom

(defgroup quick-fasd nil
  "Navigate previously-visited files and directories easily."
  :group 'quick-fasd
  :prefix "quick-fasd-")

(defcustom quick-fasd-enable-initial-prompt t
  "Specify whether to enable prompt for the initial query.
When set to nil, all fasd results are returned for completion."
  :type 'boolean
  :group 'quick-fasd)

(defcustom quick-fasd-file-manager 'dired
  "Default file manager used by `quick-fasd-find-file-action'."
  :type '(radio
          (const :tag "Dired, the default Emacs file manager" dired)
          (const :tag "Deer, Ranger's file manager" deer)
          (function :tag "Custom file manager function"))
  :group 'quick-fasd)

(defcustom quick-fasd-command-args '("-a")
  "List of flags passed to the fasd command.
This controls which types of entries are returned.

Available options:
  - `-a': Match files and directories
  - `-d': Match directories only
  - `-f': Match files only
  - `-r': Match by rank only
  - `-t': Match by recent access only

Multiple flags can be combined by adding them to the list. This allows
customizing the default behavior of `quick-fasd' searches."
  :type '(repeat string)
  :group 'quick-fasd)

(defcustom quick-fasd-executable-path "fasd"
  "Path to the Fasd executable or its command name (e.g., fasd)."
  :type 'string
  :group 'quick-fasd)

(defvar quick-fasd-mode-lighter " QFasd"
  "Default lighter string for `quick-fasd-mode'.")

;;; Internal functions

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
    (user-error "[quick-fasd] No such file or directory %s" file)))

(defun quick-fasd--get-file (prefix query)
  "Use fasd to open a file or directory and return the selected path.
Return nil if no results are found.
Optionally pass QUERY to avoid prompt.
PREFIX is the same prefix as `quick-fasd-find-file'."
  (let* ((fasd-executable (quick-fasd--get-fasd-executable-path))
         (prefix-value (prefix-numeric-value prefix))
         (fasd-args (cond
                     ((= prefix-value -1) " -f ")
                     ((= prefix-value 0) " -d ")
                     (t (format " %s "
                                (string-join quick-fasd-command-args " "))))))
    (message "PREFIX: %S / ARGS: %S" prefix-value fasd-args)
    (unless query (setq query (if quick-fasd-enable-initial-prompt
                                  (read-from-minibuffer "Fasd: ")
                                "")))
    (let* ((prompt "Fasd: ")
           (results
            ;; TODO Use an alternative such as process-line
            (split-string
             (shell-command-to-string
              (format "%s -l -R%s %s"
                      (shell-quote-argument fasd-executable)
                      fasd-args
                      query))
             "\n" t))
           (file (when results
                   (setq this-command 'quick-fasd-find-file)
                   (completing-read prompt results nil t))))
      (if file
          file
        (user-error "[quick-fasd] Fasd found nothing for: %S" query)))))

(defun quick-fasd--hook-add-path-to-db ()
  "Add current file or directory to the Fasd database."
  (let ((path (if (derived-mode-p 'dired-mode)
                  dired-directory
                (buffer-file-name (buffer-base-buffer)))))
    (when (and path (stringp path))
      (quick-fasd-add-path-to-db path))))

;;; Functions

;;;###autoload
(defun quick-fasd-find-file (prefix &optional query)
  "Use fasd to open a file or directory.
Optionally pass QUERY to avoid prompt.
If PREFIX is 0 consider only directories.
If PREFIX is -1 consider only files.
Otherwise, use `quick-fasd-command-args', which by default lists both files and
directories."
  (interactive "P")
  (if (minibufferp)
      (let* ((enable-recursive-minibuffers t)
             (file (quick-fasd--get-file prefix query)))
        (when file
          (setq file (file-name-as-directory file))
          (insert file)))
    (let ((file (quick-fasd--get-file prefix query)))
      (when file
        (quick-fasd-find-file-action file)))))

;;;###autoload
(defun quick-fasd-add-path-to-db (path)
  "Add PATH to the Fasd database."
  (let ((fasd-executable (quick-fasd--get-fasd-executable-path)))
    (when (and path
               (stringp path)
               (file-readable-p path))
      (start-process "*fasd*" nil fasd-executable "--add"
                     ;; It is generally a good idea to strip the trailing
                     ;; slash from directories before adding them to Fasd:
                     ;; - fasd stores paths as plain file/directory names.
                     ;; - Including a trailing slash can create
                     ;;   inconsistencies, especially when matching
                     ;;   directories versus files.
                     ;; - Stripping the slash ensures uniform entries and
                     ;;   avoids duplicates like /home/user/dir vs
                     ;;   /home/user/dir/.
                     (directory-file-name path)))))

;;;###autoload
(define-minor-mode quick-fasd-mode
  "Toggle fasd mode globally."
  :global t
  :group 'quick-fasd
  :lighter quick-fasd-mode-lighter
  ;; TODO: add optional defcustom to update database on window or buffer change
  (if quick-fasd-mode
      (progn (add-hook 'find-file-hook #'quick-fasd--hook-add-path-to-db)
             (add-hook 'dired-mode-hook #'quick-fasd--hook-add-path-to-db))
    (remove-hook 'find-file-hook #'quick-fasd--hook-add-path-to-db)
    (remove-hook 'dired-mode-hook #'quick-fasd--hook-add-path-to-db)))

;;; Provide

(provide 'quick-fasd)
;;; quick-fasd.el ends here
