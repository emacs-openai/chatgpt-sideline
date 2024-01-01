;;; chatgpt-sideline.el --- Sideline support for chatgpt  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/chatgpt-sideline
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (chatgpt "0.1.0") (sideline "0.1.0"))
;; Keywords: convenience chatgpt ai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Sideline support for chatgpt.
;;

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'simple)
(require 'subr-x)

(require 'chatgpt)
(require 'sideline)

(defgroup chatgpt-sideline nil
  "Sideline support for chatgpt."
  :prefix "chatgpt-sideline-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/chatgpt-sideline"))

;;;###autoload
(defun chatgpt-sideline (command)
  "Backend sideline for chatgpt.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates (cons :async #'chatgpt-sideline--show))
    (`action #'chatgpt-sideline--on-action)))

(defun chatgpt-sideline--on-action (candidate &rest _)
  "React to action when CANDIDATE is clicked."
  (pcase candidate
    ("📝 Edit" (chatgpt-edit-start chatgpt-instance))
    ("📋 Copy"
     (kill-new (chatgpt-current-content))
     (let ((message-log-max)) (message "Copied!")))))

(defun chatgpt-sideline--editable-p ()
  "Return non-nil when current section is ediatble."
  (string= (chatgpt-user) (chatgpt-current-role)))

(defun chatgpt-sideline--copyable-p ()
  "Return non-nil when current section is copyable."
  (not (string= (chatgpt-user) (chatgpt-current-role))))

(defun chatgpt-sideline--show (callback &rest _)
  "Execute CALLBACK to display with sideline."
  (when (and chatgpt-chat-history
             (eq major-mode #'chatgpt-mode))
    (cond ((chatgpt-sideline--editable-p)
           (funcall callback '("📝 Edit")))
          ((chatgpt-sideline--copyable-p)
           (funcall callback '("📋 Copy"))))))

(provide 'chatgpt-sideline)
;;; chatgpt-sideline.el ends here
