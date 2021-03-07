;;; org-fc-compat.el --- Compatibility Code -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.0.1

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
;; Code needed for backward compatibility with previous versions of org-fc.
;;
;;; Code:
;;;; Obsolete Aliases

(define-obsolete-function-alias
  'org-fc-review-rate-card
  'org-fc-review-rate "0.0.1")

(define-obsolete-function-alias
  'org-fc-show-all
  'org-fc-remove-overlays "0.0.1")

(define-obsolete-function-alias
  'org-fc-stats
  'org-fc-dashboard-stats "0.0.1")

(define-obsolete-function-alias
  'org-fc--hashtable-to-alist
  'org-fc-dashboard--hashtable-to-alist "0.0.1")

(define-obsolete-function-alias
  'org-fc-sm2-fuzz
  'org-fc-algo-sm2-fuzz "0.0.1")

(define-obsolete-function-alias
  'org-fc-sm2-next-parameters
  'org-fc-algo-sm2-next-parameters "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-changes
  'org-fc-algo-sm2-changes "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-fixed-intervals
  'org-fc-algo-sm2-intervals "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-ease-min
  'org-fc-algo-sm2-ease-min "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-ease-max
  'org-fc-algo-sm2-ease-max "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-ease-initial
  'org-fc-algo-sm2-ease-initial "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-fuzz-min
  'org-fc-algo-sm2-fuzz-min "0.0.1")

(define-obsolete-variable-alias
  'org-fc-sm2-fuzz-max
  'org-fc-algo-sm2-fuzz-max "0.0.1")

(define-obsolete-variable-alias
  'org-fc-audio-property-after
  'org-fc-audio-after-setup-property "0.1.0")

(define-obsolete-variable-alias
  'org-fc-audio-property-before
  'org-fc-audio-before-setup-property "0.1.0")

(define-obsolete-function-alias
  'org-fc-audio-set-before
  'org-fc-audio-set-before-setup "0.1.0")

(define-obsolete-function-alias
  'org-fc-audio-set-after
  'org-fc-audio-set-after-setup "0.1.0")

;;; Footer

(provide 'org-fc-compat)

;;; org-fc-compat.el ends here
