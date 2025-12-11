;;; bash-alias.el --- Use Bash aliases in interactive shell-comands  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Maintainer: Arthur Miller <arthur.miller@live.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/amno1/emacs-bash-alias
;; Keywords: convenience, tools, shell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A shell-command that understands Bash aliases

;;; Code:

(defgroup bash-alias nil
  "Shell-command that understands Bash aliases."
  :group 'tools)

(defvar bash-alias--table nil
  "Table containg names and values for Bash aliases.")

(defun bash-alias-collect-aliases ()
  "Obtain list of bash aliases for the current user."
  (interactive)
  ;; no error checking here, we are parsing a machine generated file
  (unless bash-alias--table
    (setf bash-alias--table (make-hash-table :test #'equal)))
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (let ((shell-command-switch "-ic") beg)
      (shell-command "alias" t)
      (while (search-forward "alias" nil t)
        (setq beg (1+ (point)))
        (search-forward "=")
        (puthash (buffer-substring-no-properties beg (1- (point)))
                 (buffer-substring-no-properties
                  (1+ (point)) (1- (line-end-position)))
                 bash-alias--table)))))

(defun bash-alias-shell-command ()
  "Like `shell-command' but understands Bash aliases."
  (interactive)
  (let* ((args (eval (cadr (interactive-form 'shell-command))))
         (cmd (string-trim (pop args))))
    (apply #'shell-command (or (gethash cmd bash-alias--table) cmd) args)))

(defun bash-alias-async-shell-command ()
  "Like `shell-command' but understands Bash aliases."
  (interactive)
  (let* ((args (eval (cadr (interactive-form 'shell-command))))
         (cmd (string-trim (pop args))))
    (apply #'async-shell-command (or (gethash cmd bash-alias--table) cmd) args)))

(defvar bash-alias-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap shell-command] #'bash-alias-shell-command)
    (define-key map [remap async-shell-command] #'bash-alias-async-shell-command)
    map))

;;;###autoload
(define-minor-mode bash-alias-mode
  "Enable Bash aliases in `shell-command' and `async-shell-command'."
  :global t :lighter " bash-alias"
  (cond
   (bash-alias-mode
    (unless bash-alias--table (bash-alias-collect-aliases))
    (define-key bash-alias-mode-map [remap shell-command]
                #'bash-alias-shell-command)
    (define-key bash-alias-mode-map [remap async-shell-command]
                #'bash-alias-async-shell-command))
   (t
    (setf bash-alias--table nil)
    (define-key bash-alias-mode-map [remap shell-command] nil)
    (define-key bash-alias-mode-map [remap async-shell-command] nil))))

(provide 'bash-alias)
;;; bash-alias.el ends here
