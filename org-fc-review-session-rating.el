;;; org-fc-review-session-rating.el --- Represents a single review session rating -*- lexical-binding: t; -*-

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

(require 'eieio)

(defclass org-fc-review-session-rating ()
  ((total
    :intiform 0
    :initarg :total
    :type integer)
   (again
    :intiform 0
    :initarg :again
    :type integer)
   (hard
    :intiform 0
    :initarg :hard
    :type integer)
   (good
    :intiform 0
    :initarg :good
    :type integer)
   (easy
    :intiform 0
    :initarg :easy
    :type integer)))

;;; Footer

(provide 'org-fc-review-session-rating)

;;; org-fc-review-session-rating.el ends here
