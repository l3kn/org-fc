;;; org-fc-algo-sm2.el --- Variation of SM2 spacing algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Leon Rische

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

(require 'cl-lib)

(require 'org-fc-core)

;;;; Customization

(defcustom org-fc-algo-sm2-history-file (expand-file-name "org-fc-reviews.tsv" user-emacs-directory)
  "File to store review history in."
  :type 'string
  :group 'org-fc)

;;;; Properties

(org-fc-property org-fc-algo-sm2-ease-min 1.3
                 "Lower bound for a cards ease."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_EASE_MIN")

(org-fc-property org-fc-algo-sm2-ease-max 5.0
                 "Upper bound for a cards ease."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_EASE_MAX")

(org-fc-property org-fc-algo-sm2-ease-initial 2.5
                 "Initial ease."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_EASE_INITIAL")

(org-fc-property org-fc-algo-sm2-fuzz-min 0.9
                 "Lower bound for random interval fuzz factor."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_FUZZ_MIN")

(org-fc-property org-fc-algo-sm2-fuzz-max 1.1
                 "Upper bound for random interval fuzz factor."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_FUZZ_MAX")

(org-fc-property org-fc-algo-sm2-changes
                 '((again . -0.3)
                   (hard . -0.15)
                   (good . 0.0)
                   (easy . 0.15))
                 "Changes to a cards ease depending on its rating."
                 :type 'list
                 :group 'org-fc
                 :property "FC_SM2_CHANGES")

(org-fc-property org-fc-algo-sm2-intervals
                 '(0.0 0.01 1.0 6.0)
                 "Hard-coded intervals for the first few card boxes.
Values are in days."
                 :type 'list
                 :group 'org-fc
                 :property "FC_SM2_INTERVALS")

(org-fc-property org-fc-algo-sm2-hard-factor
                 nil
                 "When set to a non-nil value, the interval of a card rated hard increases by this factor
instead of the ease of the card."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_HARD_FACTOR")

(org-fc-property org-fc-algo-sm2-easy-bonus
                 1.0
                 "Factor applied to the interval of cards rated easy."
                 :type 'float
                 :group 'org-fc
                 :property "FC_SM2_EASY_BONUS")

(org-fc-property org-fc-algo-sm2-again-box
                 1
                 "Box cards rated `again' are moved to."
                 :type 'integer
                 :group 'org-fc
                 :property "FC_SM2_AGAIN_BOX")

;;;; Helper Functions

(defun org-fc-algo-sm2-fuzz (interval)
  "Apply fuzz to INTERVAL.
INTERVAL is by a random factor between `org-fc-algo-sm2-fuzz-min' and
`org-fc-algo-sm2-fuzz-max'"
  (let ((min (org-fc-algo-sm2-fuzz-min))
        (max (org-fc-algo-sm2-fuzz-max)))
    (* interval (+ min (cl-random (- max min))))))

;;;; Main Algorithm

(defun org-fc-algo-sm2-next-parameters (ease box interval rating)
  "Calculate the next parameters of a card, based on the review RATING.
EASE, BOX and INTERVAL are the current parameters of the card."
  (let* ((intervals (org-fc-algo-sm2-intervals))
         (changes (org-fc-algo-sm2-changes))
         (next-ease
          (if (< box 2)
              ease
            (min
             (max
              (+ ease (alist-get rating changes))
              (org-fc-algo-sm2-ease-min))
             (org-fc-algo-sm2-ease-max))))
         (next-box
          (cond
           ;; If a card is rated easy, skip the learning phase
           ((and (eq box 0) (eq rating 'easy)) 2)
           ;; If the review failed, reset the box
           ((eq rating 'again) (org-fc-algo-sm2-again-box))
           ;; Otherwise, move forward one box
           (t (1+ box))))
         (next-interval
          (cond ((< next-box (length intervals))
                 (nth next-box intervals))
                ((eq rating 'hard)
                 (let ((factor (or (org-fc-algo-sm2-hard-factor) next-ease)))
                   (* factor interval)))
                ((eq rating 'easy)
                 (org-fc-algo-sm2-fuzz
                  (* (org-fc-algo-sm2-easy-bonus) next-ease interval)))
                (t (org-fc-algo-sm2-fuzz (* next-ease interval))))))
    (list next-ease next-box next-interval)))

(defun org-fc-algo-sm2-initial-review-data ()
  "Initial SM2 review data for any position."
  (list
   'ease (format "%.2f" (org-fc-algo-sm2-ease-initial))
   'box (number-to-string 0)
   'interval (format "%.2f" 0)
   'due (org-fc-timestamp-in 0)))

(defun org-fc-algo-sm2-next-review-data (old-data rating)
  (let ((ease (string-to-number (plist-get old-data 'ease)))
        (box (string-to-number (plist-get old-data 'box)))
        (interval (string-to-number (plist-get old-data 'interval))))
    (cl-destructuring-bind (next-ease next-box next-interval)
        (org-fc-algo-sm2-next-parameters ease box interval rating)
      (list
       'ease (format "%.2f" next-ease)
       'box (number-to-string next-box)
       'interval (format "%.2f" next-interval)
       'due (org-fc-timestamp-in next-interval)))))

(defun org-fc-algo-sm2-history-add (position old-data rating delta)
  (let* ((card (oref position card))
         (path (oref (oref card file) path))
         (id (oref card id))
         (name (oref position name))

         (ease (string-to-number (plist-get old-data 'ease)))
         (box (string-to-number (plist-get old-data 'box)))
         (interval (string-to-number (plist-get old-data 'interval)))

         (elements
          (list
           (org-fc-timestamp-in 0)
           path
           id
           name
           (format "%.2f" ease)
           (format "%d" box)
           (format "%.2f" interval)
           (symbol-name rating)
           (format "%.2f" delta)
           (symbol-name 'sm2))))

    (append-to-file
     (format "%s\n" (mapconcat #'identity elements "\t"))
     nil
     org-fc-algo-sm2-history-file)))

;;; Footer

(provide 'org-fc-algo-sm2)

;;; org-fc-algo-sm2.el ends here
