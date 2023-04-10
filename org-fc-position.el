;;; org-fc-position.el --- Represents a single position -*- lexical-binding: t; -*-

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

(defclass org-fc-position ()
  ((box
    :initform 0
    :initarg :box
    :type (integer -1 *)
    :documentation "The positions's current Leitner system box.")
   (card
    :initform nil
    :initarg :card
    :documentation "The `org-fc-card' which contains this position.")
   (due
    :initform nil
    :initarg :due
    :type list
    :custom (repeat integer)
    :documentation "The time at which this position is due.")
   (ease
    :initform 0
    :initarg :ease
    :type (or float integer)
    :documentation "The ease factor.")
   (interval
    :initform 0
    :initarg :interval
    :type (or float integer)
    :documentation "The interval, in days.")
   (pos
    :initform ""
    :initarg :pos
    :type (or integer string)
    :documentation "The name of the position (e.g. \"front\" for double/normal or \"0\" for cloze)."))
  "Represents a single position.")

(cl-defmethod org-fc-position--is-due ((pos org-fc-position))
  "Return t if POS is due; else nil."
  (time-less-p (oref pos due) (current-time)))

(cl-defmethod org-fc-positions--filter-due ((positions list))
  "Filter POSITIONS to include only enabled and due positions."
  (let ((due-enabled-positions (cl-loop for pos in positions
                                        when (and (not (oref (oref pos card) suspended))
                                                  (org-fc-position--is-due pos))
                                        collect pos)))
    (if org-fc-bury-siblings
        (org-fc-positions--one-per-card due-enabled-positions)
      due-enabled-positions)))

(defun org-fc-positions--one-per-card (positions)
  "Return filtered list of POSITIONS; only one position per card."
  (let ((-compare-fn (lambda (a b)
                       (equal (oref (oref a card) id)
                              (oref (oref b card) id)))))
    (-distinct positions)))

;;; Footer

(provide 'org-fc-position)

;;; org-fc-position.el ends here
