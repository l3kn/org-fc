;;; org-fc-type-normal.el --- Front -> Back Card Type -*- lexical-binding: t; -*-

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
;;; Code:

(require 'org-fc-core)

(defun org-fc-type-normal-init ()
  "Mark headline as card of the normal type."
  (interactive)
  (org-fc--init-card "normal")
  (org-fc-review-data-update '("front")))

(defun org-fc-type-normal-reset ()
  "Reset review metadata for this card"
  (interactive)
  (org-fc-review-data-update '("front") 't))

(defun org-fc-type-normal-setup (_position)
  "Prepare a normal card for review."
  (interactive)
  ;; Make sure the card is collapsed
  (outline-hide-subtree)
  (when (org-fc-has-back-heading-p)
    (org-show-entry)
    ;; Make sure the back heading is visible
    (org-fc-with-point-at-back-heading
     (org-show-set-visibility 'minimal))))

(defun org-fc-type-normal-flip ()
  "Flip a normal card."
  (interactive)
  (org-show-entry)
  (org-show-children)
  ;; NOTE: the body only runs if the card has a back heading
  (org-fc-with-point-at-back-heading
   (org-show-entry)
   (org-show-children)
   (org-fc-show-latex)))

(org-fc-register-type
 'normal
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-noop
 'org-fc-type-normal-reset)

;;; Footer

(provide 'org-fc-type-normal)

;;; org-fc-type-normal.el ends here
