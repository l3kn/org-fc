;;; org-fc-card.el --- Represent a single card -*- lexical-binding: t; -*-

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
(require 'dash)

(require 'org-fc-position)

(defclass org-fc-card ()
  ((created
    :initform nil
    :initarg :created
    :type list
    :custom (repeat integer)
    :documentation "The time at which this card was created.")
   (filetitle
    :initform ""
    :initarg :filetitle
    :type string
    :documentation "The title of the file which contains this card.")
   (tags
    :initform nil
    :initarg :tags
    :type list
    :custom (repeat string)
    :documentation "The tags on this card.")
   (id
    :initform ""
    :initarg :id
    :type string
    :documentation "The org-id for this card.")
   (inherited-tags
    :initform ""
    :initarg :inherited-tags
    :type string
    :documentation "Inherited tags in the form :taga:tagb:.")
   (local-tags
    :initform ""
    :initarg :local-tags
    :type string
    :documentation "Tags on the card itself in the form :taga:tagb:.")
   (path
    :initform ""
    :initarg :path
    :type string
    :documentation "Full path to the file which contains this card.")
   (positions
    :initform nil
    :initarg :positions
    :type list
    :documentation "The `org-fc-position's for this card.")
   (suspended
    :initform nil
    :initarg :suspended
    :type boolean
    :documentation "t when the card is suspended; nil otherwise.")
   (title
    :initform nil
    :initarg :title
    :type (or null string))
   (type
    :initform nil
    :initarg :type
    :type symbol
    :documentation "The org-fc-* card type (e.g. double or cloze).")))

;;; Footer

(provide 'org-fc-card)

;;; org-fc-card.el ends here
