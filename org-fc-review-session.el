;;; org-fc-review-session.el --- Represents a single review session -*- lexical-binding: t; -*-

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

(require 'org-fc-review-session-rating)

(defclass org-fc-review-session ()
  ((current-item
    :initform nil
    :initarg :current-item
    :documentation "The `org-fc-position' currently under review.")
   (paused
    :initform nil
    :initarg :paused
    :type boolean
    :documentation "t when the review session is paused; nil otherwise")
   (history
    :initform nil
    :initarg :paused)
   (rating
    :initform nil
    :initarg :rating
    :documentation "`org-fc-review-session-rating'.")
   (positions
    :initform nil
    :initarg :positions
    :documentation "List of `org-fc-position's.")))

(defun org-fc-awk-stats-reviews-as-rating (stats key)
  "Return the KEY of STATS as `org-fc-review-session-rating'."
  (cl-destructuring-bind (&key total again hard good easy &allow-other-keys)
      (pcase key
        ('all
         (plist-get stats :all))
        ('month
         (plist-get stats :month))
        ('week
         (plist-get stats :week))
        ('day
         (plist-get stats :day)))
    (org-fc-review-session-rating
     :total total
     :again again
     :hard hard
     :good good
     :easy easy)))

(cl-defmethod org-fc-review-session--from-positions ((positions list))
  "Create a new review session with POSITIONS."
  (let ((rating (if-let ((stats (org-fc-awk-stats-reviews)))
                    (org-fc-awk-stats-reviews-as-rating stats 'day)
                  (org-fc-review-session-rating))))
    (org-fc-review-session
     :positions positions
     :rating rating)))

(cl-defmethod org-fc-review-session--add-rating ((session org-fc-review-session) rating-to-add)
  "Store RATING in the review history of SESSION."
  (with-slots (rating) session
    (cl-case rating
      ('again (cl-incf (oref rating again)))
      ('hard (cl-incf (oref rating hard)))
      ('good (cl-incf (oref rating good)))
      ('easy (cl-incf (oref rating easy))))
    (cl-incf (oref rating total))))

;;; Footer

(provide 'org-fc-review-session)

;;; org-fc-review-session.el ends here
