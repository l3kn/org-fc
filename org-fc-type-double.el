;;; org-fc-type-double.el --- Front <-> Back Card Type -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.1.0

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
;; Variant of the normal card type that's also reviewed in the inverse
;; (back -> front) direction.
;;
;;; Code:

(require 'org-fc-core)

(defvar org-fc-type-double--overlay '())

(defun org-fc-type-double-init ()
  "Mark headline as card of the double type."
  (interactive)
  (org-fc--init-card "double")
  (org-fc-review-data-update '("front" "back")))

(defun org-fc-type-double-reset ()
  "Reset review data for the double type."
  (interactive)
  (org-fc-review-data-update '("front" "back") 't))

(defun org-fc-type-double-setup (position)
  "Prepare POSITION of a double card for review."
  (pcase position
    ("front" (org-fc-type-normal-setup position))
    ("back"
     (outline-hide-subtree)
     (if (org-fc-has-back-heading-p)
         (org-fc-with-point-at-back-heading
          (org-fc-show-latex)
          (outline-show-entry))
       (org-show-entry)
       (setq org-fc-type-double--overlay (org-fc-hide-heading "[...]"))))
    (_ (error "Invalid double position %s" position))))

(defun org-fc-type-double-flip ()
  "Flip a double card."
  (if org-fc-type-double--overlay
      (delete-overlay org-fc-type-double--overlay))
  (org-show-entry)
  (org-show-children)
  (org-fc-with-point-at-back-heading
   (org-show-entry)
   (org-show-children)
   (org-fc-show-latex)))

(org-fc-register-type
 'double
 'org-fc-type-double-setup
 'org-fc-type-double-flip
 'org-fc-noop
 'org-fc-type-double-reset)

;;; Footer

(provide 'org-fc-type-double)

;;; org-fc-type-double.el ends here
