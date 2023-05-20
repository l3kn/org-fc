;;; org-fc-type-text-input.el --- Text-input card type -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Leon Rische

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
;;; Code:

(require 'org-fc-core)
(require 'org-fc-diff)

(defun org-fc-text-input-content ()
  "Return the first line of a cards (back) contents.
Returns a pair (pos . string).  If the card has a 'Back' heading,
its content is used, otherwise, the main content is used.  This
function is expected to be called with point on a heading."
  (save-excursion
    ;; Go to main or "Back" heading
    (if-let ((pos (org-fc-back-heading-position)))
        (goto-char pos))

    ;; Skip metadata & drawers
    (forward-line)
    (while (or (looking-at-p org-scheduled-regexp)
               (looking-at-p org-deadline-regexp))
      (forward-line))
    (while (looking-at org-drawer-regexp)
      (if (re-search-forward ":END:" nil t)
          (forward-line)
        (error "No :END: found for drawer")))
    (unless (looking-at-p org-heading-regexp)
      (cons
       (point)
       (buffer-substring-no-properties (point) (point-at-eol))))))

(defun org-fc-type-text-input-init ()
  "Mark headline as card of the text-input type."
  (interactive)
  (unless (org-fc-text-input-content)
    (error "Card contains content"))
  (org-fc--init-card "text-input")
  (org-fc-review-data-update '("front")))

(defun org-fc-type-text-input-setup (_position)
  "Prepare a text-input card for review."
  (interactive)
  ;; Hide answer
  (outline-hide-subtree)
  (when (org-fc-has-back-heading-p)
    (org-show-entry)
    (org-fc-with-point-at-back-heading (org-show-set-visibility 'minimal)))
  ;; Prompt user, create diff overlay
  (let* ((pos-content (org-fc-text-input-content))
         (content (cdr pos-content))
         (start (car pos-content))
         (end (+ start (length content)))
         (deemph (org-fc-deemphasize content))
         (diff (org-fc-diff (read-string "Answer: ") (cdr deemph))))
    ;; Overlay for user input
    (when (car deemph)
      (setq start (1+ start))
      (setq end (1- end)))
    (org-fc-hide-region start end (car diff))
    ;; Overlay for expected answer, using the newline after the answer
    (if (cdr diff)
        (org-fc-hide-region
         end (1+ end)
         (concat
          " (expected: "
          (if (null (car deemph))
              (cdr diff)
            (org-fc-emphasize
             (concat (car deemph) (cdr diff) (car deemph))))
          ")\n"))))
  ;; Reveal answer & diff
  (save-excursion
    (org-show-entry)
    (org-show-children)
    (org-fc-with-point-at-back-heading
     (org-show-entry)
     (org-show-children)
     (org-fc-show-latex))))

(org-fc-register-type
 'text-input
 'org-fc-type-text-input-setup
 nil
 'org-fc-noop)

;;; Footer

(provide 'org-fc-type-text-input)

;;; org-fc-type-text-input.el ends here
