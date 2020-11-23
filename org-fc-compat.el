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
  'org-fc-algo-sm2-fuzz)

(define-obsolete-function-alias
  'org-fc-sm2-next-parameters
  'org-fc-algo-sm2-next-parameters)

(define-obsolete-variable-alias
  'org-fc-sm2-changes
  'org-fc-algo-sm2-changes)

(define-obsolete-variable-alias
  'org-fc-sm2-fixed-intervals
  'org-fc-algo-sm2-fixed-intervals)

(define-obsolete-variable-alias
  'org-fc-sm2-ease-min
  'org-fc-algo-sm2-ease-min)

(define-obsolete-variable-alias
  'org-fc-sm2-ease-max
  'org-fc-algo-sm2-ease-max)

(define-obsolete-variable-alias
  'org-fc-sm2-ease-initial
  'org-fc-algo-sm2-ease-initial)

(define-obsolete-variable-alias
  'org-fc-sm2-fuzz-min
  'org-fc-algo-sm2-fuzz-min)

(define-obsolete-variable-alias
  'org-fc-sm2-fuzz-max
  'org-fc-algo-sm2-fuzz-max)

;;; Footer

(provide 'org-fc-compat)

;;; org-fc-compat.el ends here
