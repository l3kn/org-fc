;;; org-fc-sm2.el --- SM2 Repetition Spacing Algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

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
;; This file implements a modified version of the SM2 review spacing algorithm,
;; similar to the one used by Anki.
;;
;;; Code:

;;;; Customization

(defcustom org-fc-sm2-changes
  '((again . -0.3)
    (hard . -0.15)
    (good . 0.0)
    (easy . 0.15))
  "Changes to a cards ease depending on its rating."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-sm2-fixed-intervals
  '(0.0 0.01 1.0 6.0)
  "Hard-coded intervals for the first few card boxes.
Values are in days."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-sm2-ease-min 1.3 "Lower bound for a cards ease."
  :type 'float
  :group 'org-fc)
(defcustom org-fc-sm2-ease-initial 2.5 "Initial ease."
  :type 'float
  :group 'org-fc)
(defcustom org-fc-sm2-ease-max 5.0 "Upper bound for a cards ease."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-sm2-fuzz-min 0.9
  "Lower bound for random interval fuzz factor."
  :type 'float
  :group 'org-fc)
(defcustom org-fc-sm2-fuzz-max 1.1
  "Upper bound for random interval fuzz factor."
  :type 'float
  :group 'org-fc)

(defun org-fc-sm2-fuzz (interval)
  "Apply fuzz to INTERVAL.
INTERVAL is by a random factor between `org-fc-sm2-fuzz-min' and
`org-fc-sm2-fuzz-max'"
  (*
   interval
   (+ org-fc-sm2-fuzz-min
      (cl-random (- org-fc-sm2-fuzz-max org-fc-sm2-fuzz-min)))))

;;;; Parameter Calculation

(defun org-fc-sm2-next-box (box rating)
  "Calculate the next box of a card in BOX, rated as RATING."
  (cond
   ;; If a card is rated easy, skip the learning phase
   ((and (eq box 0) (eq rating 'easy)) 2)
   ;; If the review failed, go back to box 0
   ((eq rating 'again) 0)
   ;; Otherwise, move forward one box
   (t (1+ box))))

(defun org-fc-sm2-next-ease (ease box rating)
  "Calculate the next ease of a card, based on the review RATING.
EASE and BOX are the current parameters of the card."
  (if (< box 2)
      ease
    (min
     (max
      (+ ease (alist-get rating org-fc-sm2-changes))
      org-fc-sm2-ease-min)
     org-fc-sm2-ease-max)))

(defun org-fc-sm2--next-interval (interval next-box next-ease)
  "Calculate the next interval of a card.
INTERVAL is the current interval of the card, NEXT-BOX and
NEXT-EASE are the new parameters of the card."
  (if (< next-box (length org-fc-sm2-fixed-intervals))
      (nth next-box org-fc-sm2-fixed-intervals)
    (org-fc-sm2-fuzz (* next-ease interval))))

(defun org-fc-sm2-next-parameters (ease box interval rating)
  "Calculate the next parameters of a card, based on the review RATING.
EASE, BOX and INTERVAL are the current parameters of the card."
  (let* ((next-ease (org-fc-sm2-next-ease ease box rating))
         (next-box (org-fc-sm2-next-box box rating))
         (next-interval (org-fc-sm2--next-interval interval next-box next-ease)))
    (list next-ease next-box next-interval)))

;;;; Footer

(provide 'org-fc-sm2)

;;; org-fc-sm2.el ends here
