;;; org-fc-card.el --- Representation of a card. -*- lexical-binding: t; -*-

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
;; A single card.
;;
;;; Code:

(require 'eieio)
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
   (priority
    :initform 0
    :initarg :priority
    :type number
    :documentation "How important the card is; higher number is more important.")
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
    :documentation "Inherited tags in the form :taga:tagb:."
    )
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

(cl-defmethod org-fc-card--to-positions ((card org-fc-card))
  "Return list of `org-fc-position' extracted from CARD."
  (mapcar
   (lambda (pos)
     (let ((box (plist-get pos :box))
           (ease (plist-get pos :ease))
           (interval (plist-get pos :interval))
           (pos-pos (plist-get pos :position))
           (due (plist-get pos :due)))
       (org-fc-position
        :box box
        :card card
        :due due
        :ease ease
        :interval interval
        :pos pos-pos)))
   (oref card positions)))

(defun org-fc-cards--to-positions (cards)
  "Convert a list of CARDS (`org-fc-card's) to a list of `org-fc-positions's."
  (cl-loop for card in cards
           append (org-fc-card--to-positions card)))

(cl-defmethod org-fc-card--is-blocked ((card org-fc-card))
  "Return t if the CARD is blocked; nil otherwise.

Blocking cards must be in the same file as the blocked card."
  (when org-fc-cache-mode
    (let ((blocking-ids (oref card blocked-by)))
      (when (> (length blocking-ids) 0 )
        (let* ((box-threshold (length org-fc-algo-sm2-intervals))
               (path (oref card path))
               (cached-index (plist-get (gethash path
                                                 org-fc-cache)
                                        :cards))
               (cached-cards (org-fc-index--to-cards cached-index))
               (blocking-cards (--filter
                                (member (oref it id)
                                        blocking-ids)
                                cached-cards))
               (blocking-positions (org-fc-cards--to-positions blocking-cards))
               (blocking-positions-below-threshold (--filter
                                                    (< (oref it box) box-threshold)
                                                    blocking-positions)))
          (not (= (length blocking-positions-below-threshold)
                  0)))))))

(provide 'org-fc-card)
;;; org-fc-card.el ends here
