;;; org-fc-type-text-input.el --- Text input card type -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

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
;; Card type prompting the user to enter a string during review.
;; This string is then compared to the back of the card.
;;
;; Not implemented yet!
;;
;;; Code:

(defun org-fc-type-text-input-init ()
  "Initialize a heading as text-input card."
  (interactive)
  (org-fc--init-card "text-input")
  (org-fc-review-data-update '("front")))

(defun org-fc-type-text-input-review (_position)
  "Review a text-input card."
  (org-show-subtree)
  (let ((answer (org-entry-get (point) "ANSWER"))
        (user-answer (read-string "Answer: ")))
    (goto-char (point-max))
    ;; Overlays need to be of at least size 1 to be visible
    (let ((ovl (make-overlay (- (point) 1) (point))))
      (overlay-put ovl 'category 'org-fc-additional-text-overlay)
      (overlay-put ovl 'priority 9999)
      (overlay-put ovl 'face 'default)
      (overlay-put ovl 'display
                   (concat "\n\n\nExpected: " answer
                           "\nGot:      " user-answer)))))

(defun org-fc-type-text-input-update ()
  "Update a text-input card, No-op.")

;; TODO: Implement real handler
(org-fc-register-type
 'text-input
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-type-normal-update)

;;;; Footer

(provide 'org-fc-type-text-input)

;;; org-fc-type-text-input.el ends here
