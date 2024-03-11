;;; org-fc-algo-noop.el --- No-op spacing algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>

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
;; A no-op scheduling algorithm that logs no history entries
;; and doesn't update the review data or due date of a card.
;;
;; Ideally positions using this algorithm would not even have a
;; due-date but for now, org-fc assumes that every position has a
;; due-date it can use for scheduling cards to review.
;;
;;; Code:

(defclass org-fc-algo-noop (eieio-singleton org-fc-algo) ())

(cl-defmethod org-fc-algo-headers ((_algo org-fc-algo-noop))
  '())

(cl-defmethod org-fc-algo-update-review-data
  ((algo org-fc-algo-noop)
   (_position org-fc-position)
   _rating _delta)
  "Don't do anything, the card will remain due.")

(org-fc-register-algo 'noop org-fc-algo-noop)

;;; org-fc-algo-noop.el ends here
