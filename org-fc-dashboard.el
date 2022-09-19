;;; org-fc-dashboard.el --- Dashboard buffer for org-fc -*- lexical-binding: t; -*-

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
;; The dashboard shows an overview over all cards in the system (or in
;; the current context.
;;
;;; Code:

(require 'svg)

(require 'org-fc-core)
(require 'org-fc-awk)
(require 'org-fc-review)

;;; Customization

(defcustom org-fc-dashboard-bar-chart-width 400
  "Width of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-bar-chart-height 20
  "Height of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-text-bar-chart-width 40
  "Width of the text-bar chart used to display review statistics.
This is only used org-fc is run in a non-graphic display
environment without svg support."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-buffer-name "*org-fc Dashboard*"
  "Name of the buffer to use for displaying the dashboard view."
  :type 'string
  :group 'org-fc)

;;; Variables

(defvar org-fc-dashboard-context org-fc-context-all
  "Context of the current dashboard view.")

;;; Helper Functions

(defun org-fc-dashboard--hashtable-to-alist (ht)
  "Convert a hash-table HT to an alist."
  (let (res)
    (dolist (key (hash-table-keys ht))
      (push (cons key (gethash key ht)) res))
    res))

;;; Stats

(defun org-fc-dashboard-stats (index)
  "Compute statistics for an INDEX of cards and positions."
  (let* ((total 0)
         (suspended 0)
         (by-type (make-hash-table))
         (ease-sum 0.0)
         (box-sum 0.0)
         (interval-sum 0.0)
         (new 0)
         (n-pos 0)
         ;; NOTE: This has to use `list' so incf + getf works as
         ;; expected
         (created (list :day 0 :week 0 :month 0))
         (due (list :now 0 :day 0 :week 0 :month 0))
         (now (current-time))
         (minus-day (time-subtract now (* 24 60 60)))
         (minus-week (time-subtract now (* 7 24 60 60)))
         (minus-month (time-subtract now (* 30 24 60 60)))
         (plus-day (time-add now (* 24 60 60)))
         (plus-week (time-add now (* 7 24 60 60)))
         (plus-month (time-add now (* 30 24 60 60))))
    (dolist (card index)
      (cl-incf total 1)
      (if (plist-get card :suspended)
          (cl-incf suspended 1)
        (let ((card-created (plist-get card :created)))

          (if (time-less-p minus-day card-created)
              (cl-incf (cl-getf created :day) 1))
          (if (time-less-p minus-week card-created)
              (cl-incf (cl-getf created :week) 1))
          (if (time-less-p minus-month card-created)
              (cl-incf (cl-getf created :month) 1))

          (dolist (pos (plist-get card :positions))
            (cl-incf n-pos 1)

            (let ((pos-due (plist-get pos :due)))
              (if (time-less-p pos-due now)
                  (cl-incf (cl-getf due :now) 1))
              (if (time-less-p pos-due plus-day)
                  (cl-incf (cl-getf due :day) 1))
              (if (time-less-p pos-due plus-week)
                  (cl-incf (cl-getf due :week) 1))
              (if (time-less-p pos-due plus-month)
                  (cl-incf (cl-getf due :month) 1)))

            (cl-incf ease-sum (plist-get pos :ease))
            (cl-incf box-sum (plist-get pos :box))
            (cl-incf interval-sum (plist-get pos :interval))

            (if (eq (plist-get pos :box) -1)
                (cl-incf new 1)))))
      (cl-incf (gethash (plist-get card :type) by-type 0) 1))
    (list :total total
          :suspended suspended
          :due due
          :by-type (org-fc-dashboard--hashtable-to-alist by-type)
          :created created
          :new new
          :avg-ease (/ ease-sum n-pos)
          :avg-box (/ box-sum n-pos)
          :avg-interval (/ interval-sum n-pos))))

;;; Bar Chart Generation

(defun org-fc-dashboard-bar-chart (stat)
  "Generate a svg bar-chart for the plist STAT."
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
         (fontsize (floor (* height 0.6)))
         (total (float (plist-get stat :total)))
         (pos 0)
         (values
          `((,(/ (plist-get stat :again) total) . "red")
            (,(/ (plist-get stat :hard) total) . "yellow")
            (,(/ (plist-get stat :good) total) . "green")
            (,(/ (plist-get stat :easy) total) . "darkgreen")))
         (svg (svg-create width height)))
    (dolist (value values)
      (svg-rectangle svg pos 0 (* width (car value)) height :fill (cdr value))
      (if (> (* width (car value)) (* 2 fontsize))
          (svg-text svg (format "%.1f" (* 100 (car value)))
                    :font-size fontsize
                    :fill "white"
                    :font-weight "bold"
                    :font-family "sans-serif"
                    :x (+ pos 5)
                    :y (+ fontsize (floor (- height fontsize) 2))))
      (setq pos (+ pos (* width (car value)))))
    (svg-image svg)))

(defun org-fc-dashboard-text-bar-chart (stat)
  "Generate a text bar-chart for the plist STAT."
  (cl-flet ((colored-bar (length color)
                         (propertize
                          (make-string length ?\s)
                          'font-lock-face `(:background ,color))))
    (let* ((width org-fc-dashboard-text-bar-chart-width)
           (total (float (plist-get stat :total)))
           (again (floor (* width (/ (plist-get stat :again) total))))
           (hard (floor (* width (/ (plist-get stat :hard) total))))
           (good (floor (* width (/ (plist-get stat :good) total))))
           ;; Make sure to use the total width
           (easy (- width again hard good)))
      (concat
       (colored-bar again "red")
       (colored-bar hard "yellow")
       (colored-bar good "green")
       (colored-bar easy "blue")))))

;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view (context)
  "Show the dashboard view for CONTEXT in the current buffer."
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (index (org-fc-index context))
         (stats (org-fc-dashboard-stats index))
         (created-stats (plist-get stats :created))
         (due-stats (plist-get stats :due))
         (reviews-stats (org-fc-awk-stats-reviews)))
    (with-current-buffer buf
      (erase-buffer)

      (insert
       (propertize "Card Statistics\n\n" 'face 'org-level-1))

      (insert (format "  New: %d \n"
                      (plist-get stats :new)))
      (insert "\n")
      (insert (format "  Created: %d (day) %d (week) %d (month) \n"
                      (plist-get created-stats :day)
                      (plist-get created-stats :week)
                      (plist-get created-stats :month)))
      (insert "\n")
      (insert (format
               "  %6d Cards, %d suspended\n"
               (plist-get stats :total)
               (plist-get stats :suspended)))
      (dolist (pair (plist-get stats :by-type))
        (insert (format "  %6d %s\n" (cdr pair) (car pair))))
      (insert "\n")
      (insert
       (propertize "Position Statistics\n\n" 'face 'org-level-1))

      (insert (format "  Due: %d (now) %d (day) %d (week) %d (month)\n\n"
                      (plist-get due-stats :now)
                      (plist-get due-stats :day)
                      (plist-get due-stats :week)
                      (plist-get due-stats :month)))

      (dolist (position '((:avg-ease . "Avg. Ease")
                          (:avg-box . "Avg. Box")
                          (:avg-interval . "Avg. Interval (days)")))
        (insert
         (format "  %6.2f %s\n"
                 (plist-get stats (car position))
                 (cdr position))))

      (insert "\n")

      (when reviews-stats
        (insert
         (propertize "Review Statistics (All Cards)\n\n" 'face 'org-level-1))

        (dolist (scope '((:day . "Day")
                         (:week . "Week")
                         (:month . "Month")
                         (:all . "All")))
          (when-let (stat (plist-get reviews-stats (car scope)))
            (when (> (plist-get stat :total) 0)
              (insert "    ")
              (if (and (display-graphic-p)
                       (memq 'svg (and (boundp 'image-types) image-types)))
                  (insert-image (org-fc-dashboard-bar-chart stat))
                (insert (org-fc-dashboard-text-bar-chart stat)))
              (insert (propertize (format " %s (%d)\n\n" (cdr scope) (plist-get stat :total)) 'face 'org-level-1))
              )))
        (insert "\n"))
      (insert
       (propertize "[r] Review\n" 'face 'org-level-1))
      (insert
       (propertize "[q] Quit\n" 'face 'org-level-1)))))

;;; Dashboard Mode

(defun org-fc-dashboard-review ()
  "Review the context of the current dashboard."
  (interactive)
  (org-fc-review org-fc-dashboard-context))

(defun org-fc-dashboard-revert (_ignore-auto _noconfirm)
  "Reload the current dashboard."
  (interactive)
  (org-fc-dashboard-view org-fc-dashboard-context))

(defvar org-fc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-fc-dashboard-review)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc main"
  "Major mode providing an overview of the flashcard system"
  (set (make-local-variable 'revert-buffer-function) #'org-fc-dashboard-revert)
  (setq-local cursor-type nil))

;;;###autoload
(defun org-fc-dashboard (context)
  "Open a buffer showing the dashboard view for CONTEXT."
  (interactive (list (org-fc-select-context)))
  (setq org-fc-dashboard-context context)
  (org-fc-dashboard-view context)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;; Footer

(provide 'org-fc-dashboard)

;;; org-fc-dashboard.el ends here
