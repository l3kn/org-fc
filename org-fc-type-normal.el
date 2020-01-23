;;; org-fc-type-normal.el --- Normal card type -*- lexical-binding: t; -*-

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

(defun org-fc-type-normal-init ()
  (interactive)
  (org-fc--init-card "normal")
  (org-fc-review-data-update '("front")))

(defvar org-fc-type-normal--hidden '())

(defun org-fc-type-normal-setup (_position)
  (interactive)
  (if (org-fc-has-back-heading-p)
      (progn
        (org-show-subtree)
        (setq org-fc-type-normal--hidden (org-fc-hide-subheading "Back"))))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-normal-flip ()
  (interactive)
  (save-excursion
    (org-show-subtree)
    (dolist (pos org-fc-type-normal--hidden)
      (goto-char pos)
      (org-show-subtree)))
  (org-fc-review-rate-hydra/body))

;; No-op
(defun org-fc-type-normal-update ())

(org-fc-register-type
 'normal
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-type-normal-update)

(provide 'org-fc-type-normal)
