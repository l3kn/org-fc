;;; org-fc-dashboard.el --- Dashboard for the SRS -*- lexical-binding: t; -*-

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
;; This file provides a dashboard showing statistics for the flashcard
;; system.
;;
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'svg)

(require 'org-fc-review)
(require 'org-fc-awk)

;;; Configuration

(defcustom org-fc-dashboard-bar-chart-width 400
  "Width of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)
(defcustom org-fc-dashboard-bar-chart-height 20
  "Height of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-buffer-name "*org-fc Main*"
  "Name of the buffer to use for displaying the dashboard view."
  :type 'string
  :group 'org-fc)

;;; Statistics

(defun org-fc-review-estimate (paths n)
  "Positions due in PATHS in the next N days."
  (let ((now (+ (time-to-seconds (current-time))
                (* 60 60 24 n))))
    (seq-count
     (lambda (pos) (< (time-to-seconds (plist-get pos :due)) now))
     (org-fc-awk-positions-for-paths paths))))

;;; Bar-Chart Generation

(defun org-fc-dashboard-bar-chart (stat)
  "Generate a svg bar-chart for the plist STAT."
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
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
      (setq pos (+ pos (* width (car value)))))
    (svg-image svg)))

(defun org-fc-dashboard-percent-right (stats)
  "Format review percentages in STATS."
  (let ((total (float (plist-get stats :total))))
    (format "  %5.2f | %5.2f | %5.2f | %5.2f"
            (or (* 100 (/ (plist-get stats :again) total)) 0.0)
            (or (* 100 (/ (plist-get stats :hard) total)) 0.0)
            (or (* 100 (/ (plist-get stats :good) total)) 0.0)
            (or (* 100 (/ (plist-get stats :easy) total)) 0.0))))

;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view ()
  "Show the dashboard view in the current buffer."
  (interactive)
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (cards-stats (org-fc-awk-stats-cards))
         (positions-stats (org-fc-awk-stats-positions))
         (reviews-stats (org-fc-awk-stats-reviews)))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (propertize "Flashcards\n\n" 'face 'org-level-1))

      (insert
       (propertize "  Card Statistics\n\n" 'face 'org-level-1))

      (insert (format "    New: %d (day) %d (week) %d (month) \n"
                      (plist-get cards-stats :created-day)
                      (plist-get cards-stats :created-week)
                      (plist-get cards-stats :created-month)))

      (insert "\n")
      (insert (format
               "    %6d Cards, %d suspended\n"
               (plist-get cards-stats :total)
               (plist-get cards-stats :suspended)))
      (dolist (position '((:type-normal . "Normal")
                          (:type-double . "Double")
                          (:type-text-input . "Text Input")
                          (:type-cloze . "Cloze")))
        (insert
         (format "    %6d %s\n"
                 (or (plist-get cards-stats (car position)) 0)
                 (cdr position))))

      (insert "\n")
      (insert
       (propertize "  Position Statistics\n\n" 'face 'org-level-1))

      (insert (format "    %6d Due Now\n\n" (plist-get positions-stats :due)))

      (dolist (position '((:avg-ease . "Avg. Ease")
                          (:avg-box . "Avg. Box")
                          (:avg-interval . "Avg. Interval (days)")))
        (insert
         (format "    %6.2f %s\n"
                 (plist-get positions-stats (car position))
                 (cdr position))))

      (insert "\n")

      (when reviews-stats
        (insert
         (propertize "  Review Statistics\n\n" 'face 'org-level-1))

        (dolist (scope '((:day . "Day")
                         (:week . "Week")
                         (:month . "Month")
                         (:all . "All")))
          (when-let (stat (plist-get reviews-stats (car scope)))
            (when (> (plist-get stat :total) 0)
              (insert (propertize (format "    %s (%d)\n" (cdr scope) (plist-get stat :total)) 'face 'org-level-1))
              (insert "    ")
              (insert-image (org-fc-dashboard-bar-chart stat))
              (insert (org-fc-dashboard-percent-right stat))
              (insert "\n\n"))))

        (insert "\n"))

      (insert
       (propertize "  [r] Review\n" 'face 'org-level-1))
      (insert
       (propertize "  [q] Quit\n" 'face 'org-level-1)))))

(defvar org-fc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-fc-review-all)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "G") 'org-fc-dashboard-view)
    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc main"
  "Major mode providing an overview of the flashcard system"
  (set (make-local-variable 'revert-buffer-function) #'org-fc-dashboard-view)
  (setq-local cursor-type nil))

;;;###autoload
(defun org-fc-dashboard ()
  "Open a buffer showing the dashboard view."
  (interactive)
  (org-fc-dashboard-view)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;;; Footer

(provide 'org-fc-dashboard)

;;; org-fc-dashboard.el ends here
