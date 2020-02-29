;;; org-fc-type-double.el --- Double card type -*- lexical-binding: t; -*-

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
;; Card type where either the heading and the content or the content
;; and a "Back" subheading are reviewed separately.
;;
;;; Code:

(defvar org-fc-type-double-hole-re
  (rx "{{" (group (* (not (any "}")))) "}}"))

(defvar org-fc-type-double--overlay '())

(defun org-fc-type-double-init ()
  "Initialize a double card."
  (interactive)
  (org-fc--init-card "double")
  (org-fc-review-data-update '("front" "back")))

(defun org-fc-type-double-setup (position)
  "Set up POSITION of a double card for review."
  (pcase position
    ("front" (org-fc-type-normal-setup position))
    ("back" (org-fc-type-double-setup-back))
    (_ (error "Invalid double position %s" position))))

(defun org-fc-type-double-setup-back ()
  "Set up the back of a double card for review."
  (org-show-subtree)
  (if (org-fc-has-back-heading-p)
      (setq org-fc-type-double--overlay (org-fc-hide-content "[...]\n"))
    (setq org-fc-type-double--overlay (org-fc-hide-heading "[...]")))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-double-flip ()
  "Flip a double card."
  (if org-fc-type-double--overlay
      (delete-overlay org-fc-type-double--overlay))
  (org-show-subtree)
  (org-fc-review-rate-hydra/body))

(defun org-fc-type-double-update ()
  "Update a double card, No-op.")

(org-fc-register-type
 'double
 'org-fc-type-double-setup
 'org-fc-type-double-flip
 'org-fc-type-double-update)

;;;; Footer

(provide 'org-fc-type-double)

;;; org-fc-type-double.el ends here
