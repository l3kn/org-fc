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
   (blocked-by
    :initform nil
    :initarg :blocked-by
    :type list
    :custom (repeat string)
    :documentation "The IDs of cards which block this card.")
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

(defun org-fc-card--from-raw (raw-card)
  "Return a `org-fc-card' constructed from RAW-CARD."
  (let ((card (org-fc-card
               :created (plist-get raw-card :created)
               :filetitle (plist-get raw-card :filetitle)
               :tags (plist-get raw-card :tags)
               :id (plist-get raw-card :id)
               :inherited-tags (plist-get raw-card :inherited-tags)
               :local-tags (plist-get raw-card :local-tags)
               :path (plist-get raw-card :path)
               :suspended (plist-get raw-card :suspended)
               :title (plist-get raw-card :title)
               :type (plist-get raw-card :type))))
    (oset card positions (mapcar
                          (lambda (raw-position)
                            "Return a `org-fc-position' constructed from RAW-POSITION."
                            (org-fc-position
                             :box (plist-get raw-position :box)
                             :card card
                             :due (plist-get raw-position :due)
                             :ease (plist-get raw-position :ease)
                             :interval (plist-get raw-position :interval)
                             :pos (plist-get raw-position :position)))
                          (plist-get raw-card :positions)))
    card))

;;; Footer

(provide 'org-fc-card)

;;; org-fc-card.el ends here
