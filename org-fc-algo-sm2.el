;;; org-fc-algo-sm2.el --- Variation of SM2 spacing algorithm -*- lexical-binding: t; -*-

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
;;
;;
;;; Code:

;;;; Parameters

(defcustom org-fc-algo-sm2-changes
  '((again . -0.3)
    (hard . -0.15)
    (good . 0.0)
    (easy . 0.15))
  "Changes to a cards ease depending on its rating."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-algo-sm2-fixed-intervals
  '(0.0 0.01 1.0 6.0)
  "Hard-coded intervals for the first few card boxes.
Values are in days."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-algo-sm2-ease-min 1.3 "Lower bound for a cards ease."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-algo-sm2-ease-initial 2.5 "Initial ease."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-algo-sm2-ease-max 5.0 "Upper bound for a cards ease."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-algo-sm2-fuzz-min 0.9
  "Lower bound for random interval fuzz factor."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-algo-sm2-fuzz-max 1.1
  "Upper bound for random interval fuzz factor."
  :type 'float
  :group 'org-fc)

;;;; Helper Functions

(defun org-fc-algo-sm2-fuzz (interval)
  "Apply fuzz to INTERVAL.
INTERVAL is by a random factor between `org-fc-sm2-fuzz-min' and
`org-fc-sm2-fuzz-max'"
  (*
   interval
   (+ org-fc-sm2-fuzz-min
      (cl-random (- org-fc-sm2-fuzz-max org-fc-sm2-fuzz-min)))))

;;;; Main Algorithm

(defun org-fc-algo-sm2-next-parameters (ease box interval rating)
  "Calculate the next parameters of a card, based on the review RATING.
EASE, BOX and INTERVAL are the current parameters of the card."
  (let* ((next-ease
          (if (< box 2)
              ease
            (min
             (max
              (+ ease (alist-get rating org-fc-sm2-changes))
              org-fc-sm2-ease-min)
             org-fc-sm2-ease-max)))
         (next-box
          (cond
           ;; If a card is rated easy, skip the learning phase
           ((and (eq box 0) (eq rating 'easy)) 2)
           ;; If the review failed, go back to box 0
           ((eq rating 'again) 0)
           ;; Otherwise, move forward one box
           (t (1+ box))))
         (next-interval
          (cond ((< next-box (length org-fc-sm2-fixed-intervals))
                 (nth next-box org-fc-sm2-fixed-intervals))
                ((and (eq org-fc-algorithm 'sm2-v2) (eq rating 'hard)) (* 1.2 interval))
                (t (org-fc-sm2-fuzz (* next-ease interval))))))
    (list next-ease next-box next-interval)))

(defun org-fc-algo-sm2-initial-review-data (position)
  "Initial SM2 review data for POSITION."
  (list position org-fc-sm2-ease-initial 0 0
        (org-fc-timestamp-now)))

;;; Footer

(provide 'org-fc-algo-sm2)

;;; org-fc-algo-sm2.el ends here
