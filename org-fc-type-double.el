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

(defvar org-fc-type-double-hole-re
  (rx "{{" (group (* (not (any "}")))) "}}"))

(defvar org-fc-type-double--overlay '())

(defun org-fc-type-double-init ()
  (interactive)
  (org-fc--init-card "double")
  (org-fc-review-data-update '("front" "back")))

(defun org-fc-type-double-setup (position)
  (pcase position
    ("front" (org-fc-type-normal-setup position))
    ("back" (org-fc-type-double-setup-back))
    (_ (error "Invalid double position %s" position))))

(defun org-fc-type-double-setup-back ()
  (org-show-subtree)
  (if (org-fc-has-back-heading-p)
      (setq org-fc-type-double--overlay (org-fc-hide-content "[...]\n"))
      (setq org-fc-type-double--overlay (org-fc-hide-heading "[...]")))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-double-flip ()
  (if org-fc-type-double--overlay
      (delete-overlay org-fc-type-double--overlay))
  (org-show-subtree)
  (org-fc-review-rate-hydra/body))

;; No-op
(defun org-fc-type-double-update ())

(org-fc-register-type
 'double
 'org-fc-type-double-setup
 'org-fc-type-double-flip
 'org-fc-type-double-update)

(provide 'org-fc-type-double)
