;;; org-fc-type-normal.el --- Front -> Back Card Type -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Leon Rische

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
(require 'org-fc-review-data)

(defun org-fc-type-normal-init ()
  "Mark headline as card of the normal type."
  (interactive)
  (org-fc--init-card "normal")
  (org-fc-review-data-update '("front")))

(defun org-fc-type-normal-setup (_position)
  "Prepare a normal card for review."
  (interactive)
  ;; Make sure the card is collapsed
  (outline-hide-subtree)
  (when (org-fc-has-back-heading-p)
    (org-fold-show-entry)
    ;; Make sure the back heading is visible
    (org-fc-with-point-at-back-heading
     (org-fold-show-set-visibility 'minimal))))

(defun org-fc-type-normal-flip ()
  "Flip a normal card."
  (interactive)
  (org-fold-show-entry)
  (org-fold-show-children)
  ;; NOTE: the body only runs if the card has a back heading
  (org-fc-with-point-at-back-heading
   (org-fold-show-entry)
   (org-fold-show-children)
   (org-fc-show-latex)))

(org-fc-register-type
 'normal
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-noop)

;;; Footer

(provide 'org-fc-type-normal)

;;; org-fc-type-normal.el ends here
