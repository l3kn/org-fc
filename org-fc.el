;;; org-fc.el --- Spaced Repetition System for Emacs org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.6"))
;; Version: 0.6.0

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
;; A Spaced repetition system for Emacs org-mode.
;;
;;; Code:

(require 'org-fc-core)
(require 'org-fc-compat)

(require 'org-fc-awk)
(require 'org-fc-algo-noop)
(require 'org-fc-algo-sm2)

(require 'org-fc-review-data)
(require 'org-fc-review)
(require 'org-fc-dashboard)
(require 'org-fc-cache)

(require 'org-fc-type-normal)
(require 'org-fc-type-double)
(require 'org-fc-type-text-input)
(require 'org-fc-type-cloze)

;;; Footer

(provide 'org-fc)

;;; org-fc.el ends here
