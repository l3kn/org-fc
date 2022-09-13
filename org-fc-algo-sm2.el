;;; org-fc-algo-sm2.el --- Variation of SM2 spacing algorithm -*- lexical-binding: t; -*-

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
;;
;;
;;; Code:

(require 'cl-lib)

(require 'org-fc-core)

(defmacro org-fc-property (symbol standard doc &rest args)
  (let (defcustom-args property)
    (while args
      (let ((keyword (pop args)))
        (unless (symbolp keyword)
          (error "Junk in args %S" args))
        (unless args
          (error "Keyword %s is missing an argument" keyword))
        (let ((value (pop args)))
          (cl-case keyword
            (:property (setq property value))
            (t
             (push value defcustom-args)
             (push keyword defcustom-args))))))
    (unless property
      (error "Missing keyword :property"))
    (let ((property-symbol (intern (concat (symbol-name symbol) "-property"))))
      `(progn
         (defcustom
           ,symbol
           ,standard
           ,doc
           ,@defcustom-args)
         (defcustom
           ,property-symbol
           ,property
           ,(format "Headline property for `%s'" symbol)
           :type 'string
           :group ,(plist-get defcustom-args :group))
         (defun ,symbol ()
           ,(format "Getter for `%s'" symbol)
           (if-let ((value (org-entry-get (point) ,property-symbol t)))
               ;; TODO: Switch on possible types
               (read value)
             ;; ,(case (plist-get defcustom-args :type)
             ;;    ('string 'value)
             ;;    ('float '(string-to-number value))
             ;;    ('list '(read value))
             ;;    (t (error "Unsupported property type %s"
             ;; (plist-get defcustom-args :type)

             ,symbol))))))

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
           ;; If the review failed, go back to box 0
           ((eq rating 'again) 0)
           ;; Otherwise, move forward one box
           (t (1+ box))))
         (next-interval
          (cond ((< next-box (length intervals))
                 (nth next-box intervals))
                ((and (eq org-fc-algorithm 'sm2-v2) (eq rating 'hard)) (* 1.2 interval))
                (t (org-fc-algo-sm2-fuzz (* next-ease interval))))))
    (list
     next-ease
     next-box
     next-interval)))

(defun org-fc-algo-sm2-initial-review-data (position)
  "Initial SM2 review data for POSITION."
  (let* ((box 0)
         (ease (org-fc-algo-sm2-ease-initial))
         (interval 0)
         (due (org-fc-timestamp-in interval)))
    (list
     position
     ease
     box
     interval
     due)))

;;; Footer

(provide 'org-fc-algo-sm2)

;;; org-fc-algo-sm2.el ends here
