;;; org-fc-index.el --- Represent an index for review content -*- lexical-binding: t; -*-

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

(require 'org-fc-lib)
(require 'org-fc-card)
(require 'org-fc-position)

(defclass org-fc-index ()
  ((cards
    :initform nil
    :initarg :cards
    :type list
    :documentation "The `org-fc-cards's for this index.")))

(defun org-fc-index--from-context (context)
  "Return an `org-fc-index' constructed from CONTEXT."
  (let* ((context-filter (plist-get context :filter))
         (filter (when context-filter
                   (org-fc--compile-filter context-filter)))
         (context-paths (plist-get context :paths))
         (paths (cond ((or (null context-paths)
                           (eq context-paths 'all))
                       org-fc-directories)
                      ((eq context-paths 'buffer)
                       (list (buffer-file-name)))
                      ((stringp context-paths)
                       (list context-paths))))
         (raw-index (funcall org-fc-index-function
                             paths
                             filter))
         (cards (mapcar #'org-fc-card--from-raw
                        raw-index)))
    (org-fc-index
     :cards cards)))

(cl-defmethod org-fc-index--to-shuffled-positions ((index org-fc-index))
  "Return all positions in INDEX in random order.

Positions are shuffled in a way that preserves the order of the positions for each card."
  (let* ((cards (oref index cards))
         (positions (sort
                     (mapcan
                      (lambda (card)
                        (let ((positions (oref card positions)))
                          (-zip-pair (org-fc-sorted-random (length positions))
                                     positions)))
                      cards)
                     (lambda (a b)
                       (> (car a) (car b))))))
    (mapcar #'cdr
            positions)))

(cl-defmethod org-fc-index--to-positions ((index org-fc-index))
  "Return all positions in INDEX."
  (let* ((cards (oref index cards)))
    (mapcan
     (lambda (card)
       (oref card positions))
     cards)))


;;; Footer

(provide 'org-fc-index)

;;; org-fc-index.el ends here
