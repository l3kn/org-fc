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
  "Positions due in the next N days"
  (let ((now (+ (time-to-seconds (current-time))
                (* 60 60 24 n))))
    (count-if
     (lambda (pos) (< (time-to-seconds (plist-get pos :due)) now))
     (org-fc-awk-positions-for-paths paths))))

;;; Bar-Chart Generation

(defun org-fc-dashboard-bar-chart (stat)
  "Generate a svg bar-chart for the plist STAT"
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
         (total (float (plist-get stat 'total)))
         (values
          `((,(/ (plist-get stat 'again) total) . "red")
            (,(/ (plist-get stat 'hard) total) . "yellow")
            (,(/ (plist-get stat 'good) total) . "green")
            (,(/ (plist-get stat 'easy) total) . "darkgreen")))
         (svg (svg-create width height)))
    (do ((values values (cdr values))
         (pos 0 (+ pos (* width (caar values)))))
    ((null values) '())
  (svg-rectangle svg pos 0 (* width (caar values)) height :fill (cdar values)))
    (svg-image svg)))

(defun org-fc-dashboard-percent-right (stats)
  (let ((total (float (plist-get stats 'total))))
   (format "  %5.2f | %5.2f | %5.2f | %5.2f"
           (or (* 100 (/ (plist-get stats 'again) total)) 0.0)
           (or (* 100 (/ (plist-get stats 'hard) total)) 0.0)
           (or (* 100 (/ (plist-get stats 'good) total)) 0.0)
           (or (* 100 (/ (plist-get stats 'easy) total)) 0.0))))

;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view (_ignore-auto _noconfirm)
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (stats (org-fc-stats))
         (reviews-stats (org-fc-review-stats)))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (propertize "Flashcards\n\n" 'face 'org-level-1))

      (insert
       (propertize "  Card Statistics\n\n" 'face 'org-level-1))

      (let ((created (plist-get stats :created)))
        (insert "    New:")
        (loop for (key . value) in created do
              (insert (format " %d (%s)" (plist-get value 'total) key)))
        (insert "\n"))

      (insert "\n")
      (insert (format
               "    %6d Cards, %d suspended\n"
               (plist-get stats :total)
               (plist-get stats :suspended)))

      (let ((types (plist-get stats :types)))
       (loop for (type count) on types by #'cddr do
         (insert
          (format "    %6d %s\n"
                  count
                  (symbol-name type)))))

      (insert "\n")
      (insert
       (propertize "  Position Statistics\n\n" 'face 'org-level-1))

      (insert (format "    %6d Due Now\n\n" (plist-get stats :due)))

      (let ((avg (plist-get stats :avg)))
       (dolist (position '((:ease . "Avg. Ease")
                           (:box . "Avg. Box")
                           (:interval . "Avg. Interval (days)")))
         (insert
          (format "    %6.2f %s\n"
                  (plist-get avg (car position))
                  (cdr position)))))

      (insert "\n")

      (when reviews-stats
        (insert
         (propertize "  Review Statistics\n\n" 'face 'org-level-1))

        (loop for (key . value) in reviews-stats do
              (when (plusp (plist-get value 'total))
                (insert (propertize (format "    %s (%d)\n" key (plist-get value 'total)) 'face 'org-level-1))
                (insert "    ")
                (insert-image (org-fc-dashboard-bar-chart value))
                (insert (org-fc-dashboard-percent-right value))
                (insert "\n\n")))

        (insert "\n"))

      (insert
       (propertize "  [r] Review\n" 'face 'org-level-1))
      (insert
       (propertize "  [q] Quit\n" 'face 'org-level-1)))))

(defvar org-fc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-fc-review-all)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc main"
  "Major mode providing an overview of the flashcard system"
  (set (make-local-variable 'revert-buffer-function) #'org-fc-dashboard-view)
  (setq-local cursor-type nil))

;;;###autoload
(defun org-fc-dashboard ()
  (interactive)
  (org-fc-dashboard-view nil nil)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;; Exports

(provide 'org-fc-dashboard)
