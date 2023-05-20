;;; org-fc-keymap-hint.el --- Key-binding hints for org-fc -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>

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
;;
;; Shows a message with a list of key bindings during review, this
;; replicates the look & feel of previous hydra-based review process.
;;
;;; Code:

(require 'edmacro)

(require 'org-fc)

(defun org-fc-keymap-hint--symbol-name (name)
  "Remove org-fc- prefixes from symbol NAME."
  (setq name (symbol-name name))
  (cond
   ((string-prefix-p "org-fc-review-" name)
    (substring name 14))
   ((string-prefix-p "org-fc-" name)
    (substring name 7))
   (t name)))

(defun org-fc-keymap-hint (keymap)
  "Generate key-binding hints string for KEYMAP."
  (mapconcat
   (lambda (key)
     (if (symbolp (cdr key))
         (format
          "[%s] %s"
          (edmacro-format-keys (list (car key)))
          (if (symbolp (cdr key))
              (org-fc-keymap-hint--symbol-name (cdr key))
            (cdr key)))))
   (reverse (cdr keymap))
   " "))

(add-hook 'org-fc-review-flip-mode-hook
          (lambda () (message (org-fc-keymap-hint org-fc-review-flip-mode-map))))

(add-hook 'org-fc-review-rate-mode-hook
          (lambda () (message (org-fc-keymap-hint org-fc-review-rate-mode-map))))

;; Overwrite message when review ends
(add-hook 'org-fc-after-review-hook (lambda () (message "")))

;;; Footer

(provide 'org-fc-keymap-hint)

;;; org-fc-keymap-hint.el ends here
